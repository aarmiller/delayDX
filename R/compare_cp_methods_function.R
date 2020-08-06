

#' Mean squared error
#' @name MSE
#' @param predicted Vector of predicted values
#' @param observed Vector of observed values
#' @return A scalar value
#'
#' @export
#'

MSE <- function(predicted, observed){
  x <- mean((observed - predicted)^2)
  return(x)
}

#' Root mean squared error
#' @name RMSE
#' @param predicted Vector of predicted values
#' @param observed Vector of observed values
#' @return A scalar value
#'
#' @export
#'

RMSE <- function(predicted, observed){
  x <- sqrt(mean((observed - predicted)^2))
  return(x)
}

#' Mean absolute error
#' @name MAE
#' @param predicted Vector of predicted values
#' @param observed Vector of observed values
#' @return A scalar value
#'
#' @export
#'

MAE <- function(predicted, observed){
  x <- mean(abs(observed - predicted))
  return(x)
}

#' Mean squared log error
#' @name MSLE
#' @param predicted Vector of predicted values
#' @param observed Vector of observed values
#' @return A scalar value
#'
#' @export
#'

MSLE <- function(predicted, observed){
  x <- mean((log(observed + 1) - log(predicted + 1))^2)
  return(x)
}

#' Root mean squared log error
#' @name RMSLE
#' @param predicted Vector of predicted values
#' @param observed Vector of observed values
#' @return A scalar value
#'
#' @export
#'

RMSLE <- function(predicted, observed){
  x <- sqrt(mean((log(observed + 1) - log(predicted + 1))^2))
  return(x)
}

#' Compare change point methods
#' @name run_compare_method
#' @param sim_data A dataset containing the time_map, miss bins and other parameters used for the simulation.
#'                 This dataset should be created using the `prep_sim_data()` function
#' @param method1 First change point method to compare
#' @param method2 Second change point method to compare
#' @param loss_fun The loss function used in the comparison (mean squared error (MSE), root mean squared error (RMSE),
#' mean absolute error (MAE), mean squared log error (MSLE), or root mean squared log error (RMSLE)
#' @return A one row tibble which 3 columns: 1) the loss function used, 2) the value for method 1, and 3) the value for method 2.
#'
#' @export
#'

run_compare_method <- function (sim_data, method1, method2, loss_fun = "MSE")  {

  if ((!method1 %in% c("lm", "lm_quad", "lm_cube", "quad", "cube", "exp", "spline", "cusum", "pettitt")) |
      (!method2 %in% c("lm", "lm_quad", "lm_cube", "quad", "cube", "exp", "spline", "cusum", "pettitt"))){
    stop('The method selected is not avaiable, please select from one of the following:
         "lm", "lm_quad", "lm_cube", "quad", "cube", "exp", "spline", "cusum", or "pettitt"')
  }

  if (!loss_fun %in% c("MSE", "RMSE", "MAE", "MSLE", "RMSLE")){
    stop('The method selected is not avaiable, please select from one of the following:
         "MSE", "RMSE", "MAE", "MSLE", "RMSLE"')
  }

  out <- sim_data$sim_cp %>%
    find_change_point(var_name = "n_miss_visits",
                      method = method1,
                      eval_criteria = sim_data$eval_criteria,
                      week_period = sim_data$week_period)
  cp <- out$change_point$period
  pred <- out$pred %>% filter(period > cp) %>% select(period, Y, pred1) %>% as_tibble()

  colnames(pred)[2] <- "observed"
  colnames(pred)[3] <- paste0("pred_", method1)

  out <- sim_data$sim_cp  %>%
    find_change_point(var_name = "n_miss_visits",
                      method = method2,
                      eval_criteria = sim_data$eval_criteria,
                      week_period = sim_data$week_period)
  cp <- out$change_point$period
  pred2 <- out$pred %>% filter(period > cp) %>% select(period, Y, pred1) %>% as_tibble()

  colnames(pred2)[2] <- "observed"
  colnames(pred2)[3] <- paste0("pred_", method2)


  pred_intersect <- pred %>% inner_join(pred2) %>% arrange(period)

  observed <-pred_intersect$observed
  assign(paste0("pred_", method1), pred_intersect[[paste0("pred_", method1)]])
  assign(paste0("pred_", method2), pred_intersect[[paste0("pred_", method2)]])

  compare_method <- get(loss_fun)

  out_method1 <- compare_method(get(paste0("pred_", method1)), observed)
  out_method2 <- compare_method(get(paste0("pred_", method2)), observed)

  result <- tibble(compare_method = loss_fun,
                   met = out_method1,
                   met2 = out_method2)

  names(result)[2] <- method1
  names(result)[3] <- method2

  return(result)
}

#' Bootstrapped change_point comparisons
#' @name boot_compare_methods
#' @param sim_data A dataset containing the time_map, miss bins and other parameters used for the simulation.
#'                 This dataset should be created using the `prep_sim_data()` function
#' @param method1 First change point method to compare
#' @param method2 Second change point method to compare
#' @param loss_fun The loss function used in the comparison (mean squared error (MSE), root mean squared error (RMSE),
#' mean absolute error (MAE), mean squared log error (MSLE), or root mean squared log error (RMSLE)
#' @return A one row tibble which 3 columns: 1) the loss function used, 2) the value for method 1, and 3) the value for method 2.
#'
#' @export
#'


boot_compare_methods <- function (sim_data, method1, method2, loss_fun) {

  if ((!method1 %in% c("lm", "lm_quad", "lm_cube", "quad", "cube", "exp", "spline", "cusum", "pettitt")) |
      (!method2 %in% c("lm", "lm_quad", "lm_cube", "quad", "cube", "exp", "spline", "cusum", "pettitt"))){
    stop('The method selected is not avaiable, please select from one of the following:
         "lm", "lm_quad", "lm_cube", "quad", "cube", "exp", "spline", "cusum", or "pettitt"')
  }

  if (!loss_fun %in% c("MSE", "RMSE", "MAE", "MSLE", "RMSLE")){
    stop('The method selected is not avaiable, please select from one of the following:
         "MSE", "RMSE", "MAE", "MSLE", "RMSLE"')
  }

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
                                      start_day = 0L,  # to account for adjusted days_since_dx value
                                      by_days = 1L)

  sim_data$sim_cp <- sim_cp

  # run compare funciton
  result <- run_compare_method(sim_data = sim_data,
                               method1 = method1,
                               method2 = method2,
                               loss_fun = loss_fun)

  return(result)
}

#' Run multiple bootstrapped change_point comparisons (in parallel)
#' @name compare_cp_methods_bootstrap
#' @param sim_data A dataset containing the time_map, miss bins and other parameters used for the simulation.
#'                 This dataset should be created using the `prep_sim_data()` function
#' @param boot_trials The number of bootstrapped trials to run (default is 100)
#' @param num_cores The number of worker cores to use. If not specified will determined the number of cores based on the which ever
#' is the smallest value between number of boot_trials or detected number of cores - 1.
#' @param method1 First change point method to compare
#' @param method2 Second change point method to compare
#' @param loss_fun The loss function used in the comparison (mean squared error (MSE), root mean squared error (RMSE),
#' mean absolute error (MAE), mean squared log error (MSLE), or root mean squared log error (RMSLE)
#' @param week_period Logical to incorporate a "day of the week" effect into.
#' @param eval_criteria The evaluation criteria used to find change points, if using a
#' linear regression method
#'
#' @return A tibble with two columns: 1) the bootstrap trial, 3) results from each trial (each trial contains a nested
#' tibble with 3 columns: 1) the loss function used, 2) the value for method 1, and 3) the value for method 2).
#'
#' @examples
#' #load example final_time_map dataset
#' load("/Shared/Statepi_Diagnosis/grant_projects/hsv_enceph/scripts/validation/enrolled_ge_365/report_data.RData")
#'
#' # rename ED column
#' final_time_map <- final_time_map %>% rename(ed = ED)
#'
#' # run prep sim function (note = enter any cp_method here, it just act as a place holder)
#' tmp_sim_data <- prep_sim_data(final_time_map, event_name = "any_ssd", cp_method = "lm", start_day = 1L, by_days = 1L,
#'                               week_period = TRUE)
#'
#' # run bootstrapped comparisons between two method (e.g. lm_cube vs. cusum) based on MSE
#' results <- compare_cp_methods_bootstrap(sim_data = tmp_sim_data,
#'                                         boot_trials = 1000L,
#'                                         num_cores = NULL,
#'                                         method1 = "lm_cube",
#'                                         method2 = "cusum",
#'                                         loss_fun = "MSE")
#'
#'
#' @export
#'

compare_cp_methods_bootstrap <-   function (sim_data, boot_trials  = 100L, num_cores = NULL,
                                            method1, method2, loss_fun) {

  if ((!method1 %in% c("lm", "lm_quad", "lm_cube", "quad", "cube", "exp", "spline", "cusum", "pettitt")) |
      (!method2 %in% c("lm", "lm_quad", "lm_cube", "quad", "cube", "exp", "spline", "cusum", "pettitt"))){
    stop('The method selected is not avaiable, please select from one of the following:
         "lm", "lm_quad", "lm_cube", "quad", "cube", "exp", "spline", "cusum", or "pettitt"')
  }

  if (!loss_fun %in% c("MSE", "RMSE", "MAE", "MSLE", "RMSLE")){
    stop('The method selected is not avaiable, please select from one of the following:
         "MSE", "RMSE", "MAE", "MSLE", or "RMSLE"')
  }

  simulation_data <- sim_data
  method_1  = method1
  method_2 =  method2
  loss_function = loss_fun

    # set up clusters
    if (is.null(num_cores)) {
      num_cores <- min(boot_trials, parallel::detectCores() - 1)
    } else {
      num_cores <- num_cores
    }

    cluster <- parallel::makeCluster(num_cores)

    parallel::clusterCall(cluster, function() library(tidyverse))
    parallel::clusterCall(cluster, function() library(delayDX))


    tmp <- parallel::parLapply(cl = cluster,
                                 1:boot_trials,
                                 function(x){boot_compare_methods(sim_data = simulation_data,
                                                                  method1 = method_1,
                                                                  method2 = method_2,
                                                                  loss_fun = loss_function)})
    parallel::stopCluster(cluster)
    gc()

    result <- tibble()
    for (i in 1:length(tmp)){
      tmp1 <- tibble(boot_trial = i,
                     data = tmp[i])
      result <- bind_rows(result, tmp1)

    }
 return(result)
}

