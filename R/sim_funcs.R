#' Prepare data for simulation
#'
#' @param time_map_data a time_map of visits
#' @param by_days the number of days to aggregate by in counting periods
#' @param start_day When to start counting prior to the index
#' @param event_name The variable name for the event indicator
#' @param cp_method The change-point method to fit visit counts
#' @export
#'
prep_sim_data <- function(time_map_data,by_days=1,start_day=1,event_name = "any_ssd",cp_method = "lm_quad"){

  sim_time_map <- time_map_data %>%
    dplyr::mutate(period = ((-days_since_dx - start_day)%/%by_days),  # create period based on time shifts
                  old_days_since_dx=days_since_dx,                    # store old days_since_dx for desting (DELETE LATER)
                  days_since_dx=-period-1) %>%                        # update days_since_dx for proper shifting
    dplyr::mutate(enrolid_new=enrolid) %>%
    dplyr::select(enrolid,enrolid_new,period,days_since_dx,miss_ind=!!event_name,old_days_since_dx,inpatient,ed) %>%
    filter(period>=0)

  # Compute preliminary miss bins for simulation
  tmp <- count_prior_events_truven(sim_time_map,
                                   event_name = "miss_ind",
                                   by_days = 1)

  miss_bins_visits <- tmp %>%
    find_change_point(var_name = n_miss_visits,           # NOTE: need to edit for other counts
                      method = cp_method,
                      return_miss_only = TRUE)

  miss_bins_patients <- tmp %>%
    find_change_point(var_name = n_miss_patients,           # NOTE: need to edit for other counts
                      method = cp_method,
                      return_miss_only = TRUE)

  # number of patients to simulate
  n_pat <- time_map_data %>%
    dplyr::distinct(enrolid) %>%
    nrow()

  return(list(time_map=sim_time_map,
              miss_bins_visits=miss_bins_visits,
              miss_bins_patients=miss_bins_patients,
              total_patients=n_pat,
              cp_method=cp_method,
              event_name=event_name))

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
                     n_vis_out = sum(ed==0 & inpatient==0),
                     n_vis_ed = sum(ed==1),
                     n_vis_inpatient = sum(inpatient==1),
                     dur = max(-days_since_dx))

  ## build table of number of missed visits
  sim_trial_n_visit_table <- compute_sim_trial_n_visit_table(sim_miss_num_data = sim_miss_num,
                                                             total_patients = sim_data$total_patients)


  ## build table of dureation of missed visits
  sim_trial_durration_table <- compute_sim_trial_duration_table(sim_miss_num_data = sim_miss_num)
  #sim_trial_durration_table <- NULL

  ## Compute summary statistics across all patients ###
  miss_summary <- sim_miss_num %>%
    dplyr::summarise(n_pat = n(),                  # number of patients missed
                     mean_n_vis = mean(n_vis),     # mean number of misses per patient
                     median_n_vis =median(n_vis),  # median number of misses per patient
                     mean_n_vis_out = mean(n_vis_out),     # mean number of ed misses per patient
                     median_n_vis_out =median(n_vis_out),  # median number of ed misses per patient
                     mean_n_vis_ed = mean(n_vis_ed),     # mean number of ed misses per patient
                     median_n_vis_ed =median(n_vis_ed),  # median number of ed misses per patient
                     mean_n_vis_inpatient = mean(n_vis_inpatient),     # mean number of ed misses per patient
                     median_n_vis_inpatient = median(n_vis_inpatient),  # median number of ed misses per patient
                     min_dur = min(dur),           # min duration
                     mean_dur = mean(dur),         # mean miss duration
                     median_dur = median(dur),     # median miss duration
                     max_dur = max(dur))           # max miss duration

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
#' @export
#'
run_sim_miss_visits <- function (sim_data, trials = 50, sim_duartion_for_regression = FALSE) {
  tibble::tibble(trial = 1:trials) %>%
    dplyr::mutate(data = purrr::map(trial,
                                    ~sim_miss_visits(sim_data = sim_data,
                                                      sim_duartion_for_regression = FALSE)))
}

#' Simulated missed patients from an estimated set of miss bins
#'
#' @param sim_data a dataset containing the time_map, miss bins and other parameters used for the simulation.
#'                 This dataset should be created using the `prep_sim_data()` function
#' @param new_draw_weight a weighing parameter used to assign preference to drawing previously "missed" patients
#'                        at each time step. A value of 0 applies strict preference to drawing patients who
#'                        have been assigned to miss in prior time steps, while a value 0.5 applies equal weight
#'                        to patients who have and have not been previously selected.
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
                     n_vis_out = sum(ed==0 & inpatient==0),
                     n_vis_ed = sum(ed==1),
                     n_vis_inpatient = sum(inpatient==1),
                     dur = max(-days_since_dx))

  ## build table of number of missed visits
  sim_trial_n_visit_table <- compute_sim_trial_n_visit_table(sim_miss_num_data = sim_miss_num,
                                                             total_patients = sim_data$total_patients)


  ## build table of dureation of missed visits
  sim_trial_durration_table <- compute_sim_trial_duration_table(sim_miss_num_data = sim_miss_num)

  ## Compute summary statistics across all patients ###
  miss_summary <- sim_miss_num %>%
    dplyr::summarise(n_pat = n(),                  # number of patients missed
                     n_miss_visit = sum(n_vis),    # total number of missed opportunities
                     mean_n_vis = mean(n_vis),     # mean number of misses per patient
                     median_n_vis =median(n_vis),  # median number of misses per patient
                     mean_n_vis_out = mean(n_vis_out),     # mean number of ed misses per patient
                     median_n_vis_out =median(n_vis_out),  # median number of ed misses per patient
                     mean_n_vis_ed = mean(n_vis_ed),     # mean number of ed misses per patient
                     median_n_vis_ed =median(n_vis_ed),  # median number of ed misses per patient
                     mean_n_vis_inpatient = mean(n_vis_inpatient),     # mean number of ed misses per patient
                     median_n_vis_inpatient = median(n_vis_inpatient),  # median number of ed misses per patient
                     min_dur = min(dur),           # min duration
                     mean_dur = mean(dur),         # mean miss duration
                     median_dur = median(dur),     # median miss duration
                     max_dur = max(dur))           # max miss duration



  return(list(sum_n_miss=sim_trial_n_visit_table,
              sum_duration=sim_trial_durration_table,
              miss_summary=miss_summary))
}


#' Run multiple simulations of missed visits
#'
#' @param sim_data a dataset containing the time_map, miss bins and other parameters used for the simulation.
#'                 This dataset should be created using the `prep_sim_data()` function
#' @param new_draw_weight a weighing parameter used to assign preference to drawing previously "missed" patients
#'                        at each time step. A value of 0 applies strict preference to drawing patients who
#'                        have been assigned to miss in prior time steps, while a value 0.5 applies equal weight
#'                        to patients who have and have not been previously selected.
#' @param trials number of simulationed trials to run (default is 50)
#' @export
#'
run_sim_miss_patients <- function (sim_data, trials = 50,new_draw_weight=0.0) {
  tibble::tibble(trial = 1:trials) %>%
    dplyr::mutate(data = purrr::map(trial,
                                    ~sim_miss_patients(sim_data = sim_data,
                                                       new_draw_weight = new_draw_weight)))
}

#' Bootstrap Estimation of Changepoint and simulated miss visits
#'
#' @param sim_data a dataset containing the time_map, miss bins and other parameters used for the simulation.
#'                 This dataset should be created using the `prep_sim_data()` function
#' @param n_sim_trials number of trials to run in simulation of miss visits or miss patients
#' @param sim_version the simulation version to run. Options include "visits" (default) or "patients".
#' @param new_draw_weight a weighing parameter used to assign preference to drawing previously "missed" patients
#'                        at each time step. A value of 0 applies strict preference to drawing patients who
#'                        have been assigned to miss in prior time steps, while a value 0.5 applies equal weight
#'                        to patients who have and have not been previously selected.
#' @export
#'
boot_change_point <- function (sim_data, sim_version="visits", n_sim_trials = 100L,
                               new_draw_weight=0.0,sim_duartion_for_regression = FALSE) {

  # draw bootstrapped samples
  draw_time_map <- sim_data$time_map %>%
    dplyr::distinct(enrolid) %>%
    dplyr::sample_frac(1, replace = TRUE) %>%                 # sample enrolids with replacement
    dplyr::mutate(enrolid_new = row_number()) %>%             # generate new unique enrolids
    dplyr::inner_join(select(sim_data$time_map,-enrolid_new), # remove old enrolid_new
                      by = "enrolid") %>%                     # merge back into time map
    dplyr::mutate(enrolid_old=enrolid,                        # old enrolid for tracking
                  enrolid=enrolid_new)                        # change to new enrolid  (NOTE NEEDS TO OCCUR TO COUNT PATIENTS CORRECTLY)

  # simulation change-point data
  sim_cp <- count_prior_events_truven(draw_time_map,
                                      event_name = "miss_ind",   # note the count value has already been selected in the sim data
                                      by_days = 1L)

  if (sim_version=="visits"){
    sim_cp <- sim_cp %>%
      find_change_point(var_name = n_miss_visits,
                        method = sim_data$cp_method)

    # pull out the miss bins
    miss_bins <- sim_cp$miss_bins

    # estimated number of missed visits and observed missed visits
    miss_stats <- miss_bins %>%
      dplyr::summarise(miss_visits_est = sum(num_miss),
                       miss_visits_obs = sum(Y - pred1))

    # update sim data
    new_sim_data <- sim_data
    new_sim_data$miss_bins_visits <- miss_bins

    # run simulation
    sim_res_patients <- run_sim_miss_visits(sim_data = new_sim_data,
                                            trials = n_sim_trials,
                                            sim_duartion_for_regression = FALSE)

  } else {
    sim_cp <- sim_cp %>%
      find_change_point(var_name = n_miss_patients,
                        method = sim_data$cp_method)

    # pull out the miss bins
    miss_bins <- sim_cp$miss_bins

    # estimated number of missed visits and observed missed visits
    # note that this is area under the curve and for patients this does not tell us much
    miss_stats <- miss_bins %>%
      dplyr::summarise(miss_patients_est = sum(num_miss),
                       miss_patients_obs = sum(Y - pred1))

    # update sim data
    new_sim_data <- sim_data
    new_sim_data$miss_bins_patients <- miss_bins

    # run simulation
    sim_res_patients <- run_sim_miss_patients(sim_data = new_sim_data,
                                              trials = n_sim_trials,
                                              new_draw_weight = new_draw_weight)

  }

  # aggregate results
  results <- list(change_point = sim_cp$change_point,
                  pred = sim_cp$pred,
                  miss_counts = miss_stats,
                  sim_visit_results = sim_res_patients)
  return(results)
}

#' Run multiple bootstrapped change_point simulations (in parallel)
#'
#' @param sim_data a dataset containing the time_map, miss bins and other parameters used for the simulation.
#'                 This dataset should be created using the `prep_sim_data()` function
#' @param sim_version the simulation version to run. Options include "visits" (default) or "patients".
#' @param new_draw_weight a weighing parameter used to assign preference to drawing previously "missed" patients
#'                        at each time step. A value of 0 applies strict preference to drawing patients who
#'                        have been assigned to miss in prior time steps, while a value 0.5 applies equal weight
#'                        to patients who have and have not been previously selected.
#' @param boot_trials number of bootstrapped trials to run (default is 100)
#' @param n_sim_trials number of trials to run in simulation of miss visits (default is 50)
#' @param num_cores The number of worker cores to use. If not specified will detect cores and use
#' 1 less than the number of cores
#' @export
#'
run_cp_bootstrap <-   function (sim_data, sim_version="visits", boot_trials = 100, n_sim_trials = 50,
                                new_draw_weight = 0.0, num_cores = NULL,sim_duartion_for_regression = FALSE)   {

  if (is.null(num_cores)) {
    num_cores <- parallel::detectCores() - 1
  }

  cluster <- parallel::makeCluster(num_cores)

  parallel::clusterCall(cluster, function() library(tidyverse))
  parallel::clusterCall(cluster, function() library(delayDX))

  simulation_data <- sim_data

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

  return(list(change_point = change_point,
              miss_counts = miss_counts,
              preds = preds,
              sim_visit_results = sim_visit_results))
}

