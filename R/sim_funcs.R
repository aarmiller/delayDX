#' Prepare data for simulation
#'
#' @param time_map_data a time_map of visits
#' @param by_days the number of days to aggregate by in counting periods
#' @param start_day When to start counting prior to the index
#' @param event_name The variable name for the event indicator
#' @param cp_method The change-point method to fit visit counts (i.e. "lm","lm_quad","lm_cube", "quad", "cube", "exp", "spline", "cusum",
#' "pettitt", or "set_cp"). "set_cp" is not a change point detection method, rather it is a method to specify the change point and method used
#' to model the data prior to the specified change point
#' @param eval_criteria The evaluation criteria used to find change points, if using a
#' linear regression method
#' @param specify_cp Set a specific change point you want to use instead of searching for optimal change point. Enter a postive integer value
#' repersenting the days before the index on which you you want to specify the change point. (e.g. 100 would be 100 days before the index)
#' This is a required argument if cp_method = "set_cp"
#' @param week_period Logical to incorporate a "day of the week" effect into the linear model, if
#' method is "pettitt" of "cusum". Note this is only sensible for one-day period aggregation
#' @param auto_reg Logical that determines whether expected counts use a time-series framework that incorporates autoregression.
#' Will automatically fit periodicity, automatically setting week_period to TRUE. Only relevant for cusum and pettitt methods
#' @param set_cp_method The method used to model the data prior to a specified change point for the "set_cp" cp_method
#' (i.e. "linear", "quadratic", "cubic", "quartic", "quintic", "sextic", or "exponential")
#' @param prediction_bound_for_sim Logical to specify whether or not to use the estimated predicted value or the upper bound 90%
#' prediction value in the simulations. The defualt is FALSE which uses the estimated predicited value
#'
#' @examples
#'
#' ## Example to detect change point using cumsum ##
#' out <- prep_sim_data(time_map_data, by_days=1, start_day=1, event_name = "any_ssd", cp_method = "cumsum")
#'
#' ## Examples showing different ways to specify a change point ##
#'
#' # Using a change point detection method (e.g. cumsum or lm_cube), but enforce a specific change point
#' cp <- 21
#' out <- prep_sim_data(time_map_data, by_days=1, start_day=1, event_name = "any_ssd", cp_method = "cumsum", specify_cp = cp)
#'
#' # Using the "set_cp" in the cp_method argument to apply a cubic model to the data prior to the change point
#' cp <- 21
#' out <- prep_sim_data(time_map_data, by_days=1, start_day=1, event_name = "any_ssd", cp_method = "set_cp", specify_cp = cp,
#' set_cp_method = "cubic")
#'
#'
#'
#' @export
#'
prep_sim_data <- function(time_map_data,by_days=1,start_day=1, event_name = "any_ssd", cp_method = "lm_quad", specify_cp = NULL,
                          set_cp_method = NULL, eval_criteria="AIC", week_period=FALSE, prediction_bound_for_sim = FALSE,
                          auto_reg=FALSE){

  if (cp_method == "set_cp" & (is.null(specify_cp) | is.null(set_cp_method))){
    stop("If using the 'set_cp' method for cp_method, specify_cp and set_cp_method cannot be NULL")
  }

  sim_time_map <- time_map_data %>%
    dplyr::mutate(period = ((-days_since_dx - start_day)%/%by_days),  # create period based on time shifts
                  old_days_since_dx=days_since_dx,                    # store old days_since_dx for desting (DELETE LATER)
                  days_since_dx=-period) %>%                        # If we subtract 1 from period we need to make sure to subract the mean duration missed from the simulations by 1.
    # Or you can specify that start_day = 0L (instead of 1L) in count_prior_events_truven below and in boot_change_point.
    # This way you don't have to adjust the mean duration missed from the simulations.
    dplyr::mutate(enrolid_new=enrolid) %>%
    dplyr::select(enrolid,enrolid_new,period,days_since_dx,miss_ind=!!event_name,old_days_since_dx,inpatient,ed,outpatient) %>%
    dplyr::filter(period>=0)

  # Compute preliminary miss bins for simulation
  tmp <- count_prior_events_truven(sim_time_map,
                                   event_name = "miss_ind",
                                   start_day = 0L, # to account for adjusted days_since_dx value
                                   by_days = 1)

  # For change point detection methods
  if (cp_method != "set_cp"){
    miss_bins_visits <- tmp %>%
      find_change_point(var_name = "n_miss_visits",           # NOTE: need to edit for other counts
                        method = cp_method,
                        return_miss_only = TRUE,
                        eval_criteria = eval_criteria,
                        week_period = week_period,
                        auto_reg = auto_reg,
                        specify_cp = specify_cp)

    miss_bins_patients <- tmp %>%
      find_change_point(var_name = "n_miss_patients",           # NOTE: need to edit for other counts
                        method = cp_method,
                        return_miss_only = TRUE,
                        eval_criteria = eval_criteria,
                        week_period = week_period,
                        auto_reg = auto_reg,
                        specify_cp = specify_cp)

    # number of patients to simulate
    n_pat <- time_map_data %>%
      dplyr::distinct(enrolid) %>%
      nrow()

  } else{
    miss_bins_visits <- tmp %>%
      set_change_point(var_name = "n_miss_visits",           # NOTE: need to edit for other counts
                        method = set_cp_method,
                        return_miss_only = TRUE,
                        compare_all_methods = FALSE,
                        week_period = week_period,
                        specify_cp = specify_cp)

    miss_bins_patients <- tmp %>%
      set_change_point(var_name = "n_miss_patients",           # NOTE: need to edit for other counts
                        method = set_cp_method,
                        return_miss_only = TRUE,
                        compare_all_methods = FALSE,
                        week_period = week_period,
                        specify_cp = specify_cp)


    # number of patients to simulate
    n_pat <- time_map_data %>%
      dplyr::distinct(enrolid) %>%
      nrow()

  }

  return(list(time_map=sim_time_map,
              miss_bins_visits=miss_bins_visits,
              miss_bins_patients=miss_bins_patients,
              total_patients=n_pat,
              cp_method=cp_method,
              event_name=event_name,
              start_day=start_day,
              eval_criteria=eval_criteria,
              week_period=week_period,
              specify_cp = specify_cp,
              auto_reg = auto_reg,
              set_cp_method = set_cp_method,
              prediction_bound_for_sim = prediction_bound_for_sim))

}


#' Simulated miss visits from an estimated set of miss bins
#'
#' @param sim_data a dataset containing the time_map, miss bins and other parameters used for the simulation.
#'                 This dataset should be created using the `prep_sim_data()` function
#' @export
#'
sim_miss_visits <- function (sim_data, sim_duartion_for_regression = FALSE) {

  ## Draw miss_visits ##
  miss_draw <- sim_data$time_map %>%
    dplyr::filter(miss_ind==1) %>%  # filter to miss (i.e., SSD) visits
    dplyr::inner_join(dplyr::select(sim_data$miss_bins_visits,period, num_miss), by = "period") %>% # join in number missed by bin
    dplyr::mutate(rand = runif(n = n())) %>% # draw random number
    dplyr::arrange(period, rand) %>% # arrange by random draw
    dplyr::group_by(period) %>%
    dplyr::filter(row_number() <= num_miss) %>% # filter to the number in each bin corresponding to number missed
    dplyr::ungroup()

  ## Compute number missed and miss duration by patient ##
  sim_miss_num <- miss_draw %>%
    dplyr::group_by(enrolid_new) %>%
    dplyr::summarise(n_vis = n(),
                     n_vis_out = sum(outpatient==1),
                     n_vis_ed = sum(ed==1),
                     n_vis_inpatient = sum(inpatient==1),
                     dur = max(-(days_since_dx)))

  # for computing mean and median duration with the 0 visits
  tmp_num_not_drawn <- sim_data$total_patients-nrow(sim_miss_num)

  # stats when 0 miss patients are included
  w0_stats <- sim_miss_num %>%
    dplyr::full_join(tibble(enrolid_new=rep(-99,tmp_num_not_drawn)),by = "enrolid_new") %>%
    dplyr::mutate(across(.cols = everything(),~replace_na(.,0))) %>%
    dplyr::summarise_at(dplyr::vars(-enrolid_new),list(mean_w0=mean,median_w0=median))

  ## build table of number of missed visits
  sim_trial_n_visit_table <- compute_sim_trial_n_visit_table(sim_miss_num_data = sim_miss_num,
                                                             total_patients = sim_data$total_patients)


  ## build table of dureation of missed visits
  sim_trial_durration_table <- compute_sim_trial_duration_table(sim_miss_num_data = sim_miss_num,
                                                                upper_bound = sim_data$change_point)
  #sim_trial_durration_table <- NULL

  ## Compute summary statistics across all patients ###
  miss_summary <- sim_miss_num %>%
    dplyr::summarise(n_pat = n(),                          # number of patients missed
                     mean_n_vis = mean(n_vis),             # mean number of misses per patient
                     median_n_vis =median(n_vis),          # median number of misses per patient
                     mean_n_vis_out = mean(n_vis_out),     # mean number of ed misses per patient
                     median_n_vis_out =median(n_vis_out),  # median number of ed misses per patient
                     mean_n_vis_ed = mean(n_vis_ed),       # mean number of ed misses per patient
                     median_n_vis_ed =median(n_vis_ed),    # median number of ed misses per patient
                     mean_n_vis_inpatient = mean(n_vis_inpatient),     # mean number of ed misses per patient
                     median_n_vis_inpatient = median(n_vis_inpatient),  # median number of ed misses per patient
                     min_dur = min(dur),           # min duration
                     mean_dur = mean(dur),         # mean miss duration
                     median_dur = median(dur),     # median miss duration
                     max_dur = max(dur),
                     n_vis = sum(n_vis),
                     n_vis_out = sum(n_vis_out),
                     n_vis_ed = sum(n_vis_ed),
                     n_vis_inpatient = sum(n_vis_inpatient)) %>%            # max miss duration
    dplyr::bind_cols(w0_stats)

  ## Sim Duration for Regression ##
  if (sim_duartion_for_regression == TRUE){

    key <- time_map %>%
      dplyr::distinct(enrolid,enrolid_new)

    dur_model_boot <- sim_miss_num %>%
      dplyr::select(enrolid_new,dur) %>%
      dplyr::inner_join(key) %>%
      dplyr::select(-enrolid_new)

  } else{
    dur_model_boot <- NULL
  }

  return(list(sum_n_miss=sim_trial_n_visit_table,
              sum_duration=sim_trial_durration_table,
              miss_summary=miss_summary,
              dur_model_boot=dur_model_boot))
}


#' Run multiple simulations of missed visits
#'
#' @param sim_data a dataset containing the time_map, miss bins and other parameters used for the simulation.
#'                 This dataset should be created using the `prep_sim_data()` function
#' @param trials number of simulationed trials to run (default is 50)
#' @param no_bootstrapping Specifies whether you want to run the simulations without bootstrapping the original dataset
#' @param num_cores The number of worker cores to use. If not specified will determined the number of cores based on the which ever
#' is the smallest value between number of trials or detected number of cores - 1
#' @export
#'
run_sim_miss_visits <- function (sim_data, trials = 50, sim_duartion_for_regression = FALSE, no_bootstrapping = FALSE,
                                 num_cores = NULL) {
  if (no_bootstrapping == FALSE){
    tmp <- tibble::tibble(trial = 1:trials) %>%
      dplyr::mutate(data = purrr::map(trial,
                                    ~sim_miss_visits(sim_data = sim_data,
                                                      sim_duartion_for_regression = FALSE)))
  } else {
    simulation_data <- sim_data

    if (is.null(num_cores)) {
      num_cores <- min(trials, parallel::detectCores() - 1)
    } else {
      num_cores <- num_cores
    }

    cluster <- parallel::makeCluster(num_cores)

    parallel::clusterCall(cluster, function() library(tidyverse))
    parallel::clusterCall(cluster, function() library(delayDX))

    test <- parallel::parLapply(cl = cluster,
                               1:trials,
                               function(x){sim_miss_visits(sim_data = simulation_data,
                                                           sim_duartion_for_regression = FALSE)})
    parallel::stopCluster(cluster)
    gc()

    tmp <- tibble()
    for (i in 1:length(test)){
      tmp1 <- tibble(trial = i,
                     data = test[i])
      tmp <- bind_rows(tmp, tmp1)

    }
  }
  return(tmp)
}

#' Simulated missed patients from an estimated set of miss bins
#'
#' @param sim_data a dataset containing the time_map, miss bins and other parameters used for the simulation.
#'                 This dataset should be created using the `prep_sim_data()` function
#' @param new_draw_weight a weighing parameter used to assign preference to drawing previously "missed" patients
#'                        at each time step. A value of 0 applies strict preference to drawing patients who
#'                        have been assigned to miss in prior time steps, while a value 0.5 applies equal weight
#'                        to patients who have and have not been previously selected
#' @export
#'
sim_miss_patients <- function(sim_data,new_draw_weight=0.0){

  # pull out the miss bins for patients
  miss_bins <- sim_data$miss_bins_patients %>%
    dplyr::arrange(dplyr::desc(period))

  # pull out the time map where the miss indicator = 1
  miss_time_map <- sim_data$time_map %>%
    dplyr::filter(miss_ind==1)

  # extract the vector for the number missed
  num_miss <- miss_bins$num_miss

  # extract the vector for the period of the miss
  miss_periods <- miss_bins$period

  # placeholder for the enrolids drawn each period
  miss_draw <- tibble::tibble(period=as.integer(),
                              enrolid=as.integer())

  # loop to draw missed patients
  for (i in 1:length(miss_periods)){
    #print(c(i,miss_periods[i],num_miss[i]))

    # patients who were potentially missed
    pot_period_miss <- miss_time_map %>%
      dplyr::filter(period==miss_periods[i]) %>%
      dplyr::distinct(enrolid)

    # subset to patients previously missed
    eid_list1 <- miss_draw %>%
      dplyr::distinct(enrolid) %>%
      dplyr::inner_join(pot_period_miss,by="enrolid")

    # subset to patients not previously missed
    eid_list2 <- miss_draw %>%
      dplyr::distinct(enrolid) %>%
      dplyr::anti_join(pot_period_miss,.,by="enrolid")

    # draw_patients
    eid_draw <- draw_enrollees(enrolid_list1 = eid_list1$enrolid,
                               enrolid_list2 = eid_list2$enrolid,
                               num_draw = num_miss[i],
                               new_samp_prob = new_draw_weight)

    # combine enrolids
    miss_draw <- rbind(miss_draw,
                       tibble::tibble(period=miss_periods[i],
                                      enrolid=eid_draw))

  }

  # create final miss time map
  miss_draw <- miss_draw %>%
    dplyr::inner_join(miss_time_map,by = c("period", "enrolid"))


  ## Compute number missed and miss duration by patient ##
  sim_miss_num <- miss_draw %>%
    dplyr::group_by(enrolid_new) %>%
    dplyr::summarise(n_vis = n(),
                     n_vis_out = sum(outpatient==1),
                     n_vis_ed = sum(ed==1),
                     n_vis_inpatient = sum(inpatient==1),
                     dur = max(-(days_since_dx)))

  # for computing mean and median duration with the 0 visits
  tmp_num_not_drawn <- sim_data$total_patients-nrow(sim_miss_num)

  # stats when 0 miss patients are included
  w0_stats <- sim_miss_num %>%
    full_join(tibble(enrolid_new=rep(-99,tmp_num_not_drawn)),by = "enrolid_new") %>%
    mutate(across(.cols = everything(),~replace_na(.,0))) %>%
    summarise_at(vars(-enrolid_new),list(mean_w0=mean,median_w0=median))

  ## build table of number of missed visits
  sim_trial_n_visit_table <- compute_sim_trial_n_visit_table(sim_miss_num_data = sim_miss_num,
                                                             total_patients = sim_data$total_patients)


  ## build table of dureation of missed visits
  sim_trial_durration_table <- compute_sim_trial_duration_table(sim_miss_num_data = sim_miss_num,
                                                                upper_bound = sim_data$change_point)

  ## Compute summary statistics across all patients ###
  miss_summary <- sim_miss_num %>%
    dplyr::summarise(n_pat = n(),                          # number of patients missed
                     mean_n_vis = mean(n_vis),             # mean number of misses per patient
                     median_n_vis =median(n_vis),          # median number of misses per patient
                     mean_n_vis_out = mean(n_vis_out),     # mean number of ed misses per patient
                     median_n_vis_out =median(n_vis_out),  # median number of ed misses per patient
                     mean_n_vis_ed = mean(n_vis_ed),       # mean number of ed misses per patient
                     median_n_vis_ed =median(n_vis_ed),    # median number of ed misses per patient
                     mean_n_vis_inpatient = mean(n_vis_inpatient),     # mean number of ed misses per patient
                     median_n_vis_inpatient = median(n_vis_inpatient),  # median number of ed misses per patient
                     min_dur = min(dur),           # min duration
                     mean_dur = mean(dur),         # mean miss duration
                     median_dur = median(dur),     # median miss duration
                     max_dur = max(dur),
                     n_vis = sum(n_vis),
                     n_vis_out = sum(n_vis_out),
                     n_vis_ed = sum(n_vis_ed),
                     n_vis_inpatient = sum(n_vis_inpatient)) %>%            # max miss duration
    dplyr::bind_cols(w0_stats)



  return(list(sum_n_miss=sim_trial_n_visit_table,
              sum_duration=sim_trial_durration_table,
              miss_summary=miss_summary))
}


#' Run multiple simulations of missed patients
#'
#' @param sim_data a dataset containing the time_map, miss bins and other parameters used for the simulation.
#'                 This dataset should be created using the `prep_sim_data()` function
#' @param new_draw_weight a weighing parameter used to assign preference to drawing previously "missed" patients
#'                        at each time step. A value of 0 applies strict preference to drawing patients who
#'                        have been assigned to miss in prior time steps, while a value 0.5 applies equal weight
#'                        to patients who have and have not been previously selected.
#' @param trials number of simulationed trials to run (default is 50)
#' @param no_bootstrapping Specifies whether you want to run the simulations without bootstrapping the original dataset
#' @param num_cores The number of worker cores to use. If not specified will determined the number of cores based on the which ever
#' is the smallest value between number of trials or detected number of cores - 1
#' @export
#'
run_sim_miss_patients <- function (sim_data, trials = 50, new_draw_weight=0.0, no_bootstrapping = FALSE,
                                   num_cores = NULL) {
  if (no_bootstrapping == FALSE){
    tmp <-  tibble::tibble(trial = 1:trials) %>%
    dplyr::mutate(data = purrr::map(trial,
                                    ~sim_miss_patients(sim_data = sim_data,
                                                       new_draw_weight = new_draw_weight)))

  } else {
    simulation_data <- sim_data

    if (is.null(num_cores)) {
      num_cores <- min(trials, parallel::detectCores() - 1)
    } else {
      num_cores <- num_cores
    }

    cluster <- parallel::makeCluster(num_cores)

    parallel::clusterCall(cluster, function() library(tidyverse))
    parallel::clusterCall(cluster, function() library(delayDX))

     test <- parallel::parLapply(cl = cluster,
                                 1:trials,
                                 function(x){sim_miss_patients(sim_data = simulation_data,
                                                               new_draw_weight = new_draw_weight)})
     parallel::stopCluster(cluster)
     gc()

    tmp <- tibble()
    for (i in 1:length(test)){
      tmp1 <- tibble(trial = i,
                    data = test[i])
      tmp <- bind_rows(tmp, tmp1)

    }
  }
  return(tmp)
}

#' Bootstrap Estimation of Changepoint and simulated miss visits
#'
#' @param sim_data a dataset containing the time_map, miss bins and other parameters used for the simulation.
#'                 This dataset should be created using the `prep_sim_data()` function
#' @param n_sim_trials Number of trials to run in simulation of miss visits or miss patients
#' @param sim_version The simulation version to run. Options include "visits" (default) or "patients"
#' @param new_draw_weight A weighing parameter used to assign preference to drawing previously "missed" patients
#'                        at each time step. A value of 0 applies strict preference to drawing patients who
#'                        have been assigned to miss in prior time steps, while a value 0.5 applies equal weight
#'                        to patients who have and have not been previously selected
#' @param no_bootstrapping Specifies whether you want to run the simulations without bootstrapping the original dataset
#' @param num_cores The number of worker cores to use. If not specified will detect cores and use 1 less than the number of cores
#' @export
#'
boot_change_point <- function (sim_data, sim_version="visits", n_sim_trials = 100L,
                               new_draw_weight=0.0,sim_duartion_for_regression = FALSE,
                               eval_criteria="AIC", week_period=FALSE, num_cores = NULL,
                               no_bootstrapping = FALSE, auto_reg = FALSE) {
  if (no_bootstrapping == FALSE){
    # draw bootstrapped samples
    draw_time_map <- sim_data$time_map %>%
      dplyr::distinct(enrolid) %>%
      dplyr::sample_frac(1, replace = TRUE) %>%                 # sample enrolids with replacement
      dplyr::mutate(enrolid_new = row_number()) %>%             # generate new unique enrolids
      dplyr::inner_join(select(sim_data$time_map,-enrolid_new), # remove old enrolid_new
                        by = "enrolid") %>%                     # merge back into time map
      dplyr::mutate(enrolid_old=enrolid,                        # old enrolid for tracking
                    enrolid=enrolid_new)                        # change to new enrolid  (NOTE NEEDS TO OCCUR TO COUNT PATIENTS CORRECTLY)
  } else {
    draw_time_map <- sim_data$time_map
  }
  # simulation change-point data
  sim_cp <- count_prior_events_truven(draw_time_map,
                                      event_name = "miss_ind",   # note the count value has already been selected in the sim data
                                      start_day = 0L,  # to account for adjusted days_since_dx value
                                      by_days = 1L)

  if (sim_version=="visits"){
    if (sim_data$cp_method != "set_cp"){
      sim_cp <- sim_cp %>%
        find_change_point(var_name = "n_miss_visits",
                            method = sim_data$cp_method,
                            eval_criteria = sim_data$eval_criteria,
                            week_period = sim_data$week_period,
                            auto_reg = sim_data$auto_reg,
                            specify_cp = sim_data$specify_cp)
    } else {
      sim_cp <- sim_cp %>%
        set_change_point(var_name = "n_miss_visits",
                         method = sim_data$set_cp_method,
                         return_miss_only = FALSE,
                         compare_all_methods = FALSE,
                         week_period = sim_data$week_period,
                         specify_cp = sim_data$specify_cp)
    }
      # pull out the miss bins
      miss_bins <- sim_cp$miss_bins

      # update miss bins to reflect predicted value or upper bound prediction interval
      if (sim_data$prediction_bound_for_sim == FALSE){

        miss_bins <- miss_bins %>% select(period, Y,
                                          pred1, pred,
                                          num_miss)

      } else {
        miss_bins <- miss_bins %>% select(period, Y,
                                           pred1 = upper_int_pred1,
                                           pred,
                                           num_miss = num_miss_upper_int)
      }

      # estimated number of missed visits and observed missed visits
      miss_stats <- miss_bins %>%
        dplyr::summarise(miss_visits_est = sum(num_miss),
                         miss_visits_obs = sum(Y - pred1))

      # update sim data
      new_sim_data <- sim_data
      new_sim_data$time_map <- draw_time_map
      new_sim_data$miss_bins_visits <- miss_bins
      new_sim_data$change_point <- sim_cp$change_point$period

      # run simulation
      sim_res_results <- run_sim_miss_visits(sim_data = new_sim_data,
                                              trials = n_sim_trials,
                                              sim_duartion_for_regression = sim_duartion_for_regression,
                                              no_bootstrapping = no_bootstrapping,
                                              num_cores = num_cores)

    } else {
      if (sim_data$cp_method != "set_cp"){
        sim_cp <- sim_cp %>%
          find_change_point(var_name = "n_miss_patients",
                            method = sim_data$cp_method,
                            eval_criteria = sim_data$eval_criteria,
                            week_period = sim_data$week_period,
                            specify_cp = sim_data$specify_cp)
      } else {
        sim_cp <- sim_cp %>%
          set_change_point(var_name = "n_miss_patients",
                           method = sim_data$set_cp_method,
                           return_miss_only = FALSE,
                           compare_all_methods = FALSE,
                           week_period = sim_data$week_period,
                           specify_cp = sim_data$specify_cp)
      }

      # pull out the miss bins
      miss_bins <- sim_cp$miss_bins

      # update miss bins to reflect predicted value or upper bound prediction interval
      if (sim_data$prediction_bound_for_sim == FALSE){

        miss_bins <- miss_bins %>% select(period, Y,
                                          pred1, pred,
                                          num_miss)

      } else {
        miss_bins <- miss_bins %>% select(period, Y,
                                          pred1 = upper_int_pred1,
                                          pred,
                                          num_miss = num_miss_upper_int)
      }

      # estimated number of missed visits and observed missed visits
      # note that this is area under the curve and for patients this does not tell us much
      miss_stats <- miss_bins %>%
        dplyr::summarise(miss_patients_est = sum(num_miss),
                         miss_patients_obs = sum(Y - pred1))

      # update sim data
      new_sim_data <- sim_data
      new_sim_data$time_map <- draw_time_map
      new_sim_data$miss_bins_patients <- miss_bins
      new_sim_data$change_point <- sim_cp$change_point$period

      # run simulation
      sim_res_results <- run_sim_miss_patients(sim_data = new_sim_data,
                                                trials = n_sim_trials,
                                                new_draw_weight = new_draw_weight,
                                                no_bootstrapping = no_bootstrapping,
                                                num_cores = num_cores)
    }

  # aggregate results
  results <- list(change_point = sim_cp$change_point,
                  pred = sim_cp$pred,
                  miss_counts = miss_stats,
                  sim_visit_results = sim_res_results)
  return(results)
}

#' Run multiple bootstrapped change_point simulations (in parallel)
#'
#' @param sim_data A dataset containing the time_map, miss bins and other parameters used for the simulation.
#'                 This dataset should be created using the `prep_sim_data()` function
#' @param sim_version The simulation version to run. Options include "visits" (default) or "patients"
#' @param new_draw_weight A weighing parameter used to assign preference to drawing previously "missed" patients
#'                        at each time step. A value of 0 applies strict preference to drawing patients who
#'                        have been assigned to miss in prior time steps, while a value 0.5 applies equal weight
#'                        to patients who have and have not been previously selected
#' @param boot_trials The number of bootstrapped trials to run (default is 100)
#' @param n_sim_trials The number of trials to run in simulation of miss visits (default is 50)
#' @param num_cores The number of worker cores to use. If not specified will determined the number of cores based on the which ever
#' is the smallest value between number of boot_trials or detected number of cores - 1
#' @param no_bootstrapping Specifies whether you want to run the simulations without bootstrapping the original dataset
#' @examples
#'
#' ### Run simulations with bootstrapping and allow change point to vary with each bootstrap sample ###
#'
#' #load example final_time_map dataset
#' load("/Shared/Statepi_Diagnosis/grant_projects/hsv_enceph/scripts/validation/enrolled_ge_365/report_data.RData")
#'
#' # rename ED column
#' final_time_map <- final_time_map %>% rename(ed = ED)
#'
#' # run prep sim function
#' tmp_sim_data <- prep_sim_data(final_time_map, event_name = "any_ssd", cp_method = "cusum", start_day = 1L, by_days = 1L,
#'                               week_period = TRUE)
#'
#' #run simulations on number of visits
#' simulation_results <- run_cp_bootstrap(tmp_sim_data,
#'                                        sim_version = "visits",
#'                                        boot_trials = 500,
#'                                        n_sim_trials = 50,
#'                                        new_draw_weight = NULL,
#'                                        num_cores = NULL,
#'                                        sim_duartion_for_regression = FALSE,
#'                                        no_bootstrapping = FALSE)
#'
#'
#' ### Run simulations with bootstrapping and specify a change point applied to each bootstrap sample ###
#'
#' # set a change point
#' cp <- 20L
#'
#' # run prep sim function
#' tmp_sim_data <- prep_sim_data(final_time_map, event_name = "any_ssd", cp_method = "cusum", start_day = 1L, by_days = 1L,
#'                               week_period = TRUE, specify_cp = cp)
#'
#' #run simulations on number of visits
#' simulation_results <- run_cp_bootstrap(tmp_sim_data,
#'                                        sim_version = "visits",
#'                                        boot_trials = 500,
#'                                        n_sim_trials = 50,
#'                                        new_draw_weight = NULL,
#'                                        num_cores = NULL,
#'                                        sim_duartion_for_regression = FALSE,
#'                                        no_bootstrapping = FALSE)
#'
#'
#' ### Run simulations without bootstrapping and allow function to find the optimal change point for inputted data ###
#'
#' # run prep sim function
#' tmp_sim_data <- prep_sim_data(final_time_map, event_name = "any_ssd", cp_method = "cusum", start_day = 1L, by_days = 1L,
#'                               week_period = TRUE)
#'
#' #run simulations on number of visits
#' simulation_results <- run_cp_bootstrap(tmp_sim_data,
#'                                        sim_version = "visits",
#'                                        boot_trials = 0,
#'                                        n_sim_trials = 25000,
#'                                        new_draw_weight = NULL,
#'                                        num_cores = NULL,
#'                                        sim_duartion_for_regression = FALSE,
#'                                        no_bootstrapping = TRUE)
#'
#'
#' ### Run simulations without bootstrapping and specify a change point instead of allowing function to find the optimal change point to apply to the data ###
#'
#' # set a change point
#' cp <- 20L
#'
#' # run prep sim function
#' tmp_sim_data <- prep_sim_data(final_time_map, event_name = "any_ssd", cp_method = "cusum", start_day = 1L, by_days = 1L,
#'                               week_period = TRUE, specify_cp = cp)
#'
#' #run simulations on number of visits
#' simulation_results <- run_cp_bootstrap(tmp_sim_data,
#'                                        sim_version = "visits",
#'                                        boot_trials = 0,
#'                                        n_sim_trials = 25000,
#'                                        new_draw_weight = NULL,
#'                                        num_cores = NULL,
#'                                        sim_duartion_for_regression = FALSE,
#'                                        no_bootstrapping = TRUE)
#' @export
#'
#'
run_cp_bootstrap <-   function (sim_data, sim_version="visits", boot_trials = 100, n_sim_trials = 50,
                                new_draw_weight = 0.0, num_cores = NULL, sim_duartion_for_regression = FALSE,
                                no_bootstrapping = FALSE)   {
  simulation_data <- sim_data


  # Add an warning if you specify no boostrapping but set boot_trials >0
  if ( boot_trials > 0 & no_bootstrapping == TRUE)
    stop("If you specify 'no_boostrapping' == TRUE, you have to set 'boot_trial' to 0")

  if (boot_trials>0 & no_bootstrapping == FALSE){
    # set up clusters
    if (is.null(num_cores)) {
      num_cores <- min(boot_trials, parallel::detectCores() - 1)
    } else {
      num_cores <- num_cores
    }

    cluster <- parallel::makeCluster(num_cores)

    parallel::clusterCall(cluster, function() library(tidyverse))
    parallel::clusterCall(cluster, function() library(delayDX))

    if (sim_version=="visits"){
      tmp <- parallel::parLapply(cl = cluster,
                                 1:boot_trials,
                                 function(x){boot_change_point(sim_data = simulation_data,
                                                               sim_version="visits",
                                                               n_sim_trials = n_sim_trials,
                                                               sim_duartion_for_regression = FALSE)})
    } else {
      tmp <- parallel::parLapply(cl = cluster,
                                 1:boot_trials,
                                 function(x){boot_change_point(sim_data = simulation_data,
                                                               sim_version="patients",
                                                               n_sim_trials = n_sim_trials,
                                                               new_draw_weight = new_draw_weight,
                                                               sim_duartion_for_regression = FALSE)})
    }

    # pull out change points
    change_point <- map2(tmp,1:boot_trials,~.x$change_point %>%
                           mutate(boot_trial=.y)) %>%
      bind_rows() %>%
      select(boot_trial,dplyr::everything())

    # pull out miss visit counts
    miss_counts <- map2(tmp,1:boot_trials,~.$miss_counts %>%
                                mutate(boot_trial=.y)) %>%
      bind_rows() %>%
      select(boot_trial,dplyr::everything())

    # pull out predicted visits
    preds <- map2(tmp,1:boot_trials,~.$pred %>%
                    mutate(boot_trial=.y)) %>%
      bind_rows() %>%
      select(boot_trial,dplyr::everything())

    # pull out simulation results
    sim_visit_results <- map2(tmp,1:boot_trials,~.$sim_visit_results %>%
                                mutate(boot_trial=.y))  %>%
      bind_rows() %>%
      select(boot_trial,dplyr::everything())

    parallel::stopCluster(cluster)
    gc()

  } else {
    if (sim_version=="visits"){
      tmp <- boot_change_point(sim_data = simulation_data,
                               sim_version="visits",
                               n_sim_trials = n_sim_trials,
                               sim_duartion_for_regression = FALSE,
                               no_bootstrapping = TRUE,
                               num_cores = num_cores)
    } else {
      tmp <- boot_change_point(sim_data = simulation_data,
                               sim_version="patients",
                               n_sim_trials = n_sim_trials,
                               new_draw_weight = new_draw_weight,
                               sim_duartion_for_regression = FALSE,
                               no_bootstrapping = TRUE,
                               num_cores = num_cores)
    }

    # pull out change points
    change_point <- tmp$change_point %>%
                           mutate(boot_trial=0) %>%
      bind_rows() %>%
      select(boot_trial,dplyr::everything())

    # pull out miss visit counts
    miss_counts <- tmp$miss_counts %>%
                          mutate(boot_trial=0) %>%
      bind_rows() %>%
      select(boot_trial,dplyr::everything())

    # pull out predicted visits
    preds <- tmp$pred %>%
                    mutate(boot_trial=0) %>%
      bind_rows() %>%
      select(boot_trial,dplyr::everything())

    # pull out simulation results
    sim_visit_results <- tmp$sim_visit_results %>%
                                mutate(boot_trial=0)  %>%
      bind_rows() %>%
      select(boot_trial,dplyr::everything())

  }
  return(list(change_point = change_point,
              miss_counts = miss_counts,
              preds = preds,
              sim_visit_results = sim_visit_results))
}

