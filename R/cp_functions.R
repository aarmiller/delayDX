####################################################
#These are new, alternative changepoint functions
#to use when identifying a changepoint and the
#expected number of missed visits.They each take
#as an input the output of an object from
#count_prior_events_truven, and output a summary
#of the model fit, miss_bins, and other info
###################################################

#' Identify change point using a loss function method and find expected SSD visits/calculate misses. This is done
#' by fitting a linear model for data prior to i, with i ranging from period 0 to max period - 2. For a specified loss function,
#' the first local minima is used used to identifiy the optimal change point
#'
#' @title find_cp_loss_fun
#' @param data A dataframe output by count_prior_events_truven
#' @param var_name A character string of outcome for which to apply analysis
#' @param return_miss_only Logical to only return miss information
#' @param week_period Logical to incorporate a "day of the week" effect into
#' the linear model. Note this is only sensible for one-day period aggregation.
#' @param loss_function The loss function used to identify the change point (mean squared error (MSE), root mean squared error (RMSE),
#' mean absolute error (MAE), mean squared log error (MSLE), or root mean squared log error (RMSLE)
#' @param return_loss_fun_tab_only Logical to only return loss function table that includes estimates for all loss functions available
#' @param specify_cp Set a specific change point you want to use instead of searching for optimal change point. Enter a postive integer value
#' repersenting the days before the index on which you you want to specify the change point. (e.g. 100 would be 100 days before the index)
#' @return A list containing miss information, changepoint information, predictions,
#' the model itself, and a plot of the middle finger curve and model.
#'
#' @examples
#' cp_result_pettit <- final_time_map %>%
#' filter(days_since_dx >= -180) %>%
#' count_prior_events_truven(event_name = "any_ssd", start_day = 1, by_days = 1) %>%
#' find_cp_loss_fun(var_name = "n_miss_visits", return_miss_only = FALSE, week_period=TRUE, loss_function = "MAE")
#'
#' @export
#'
find_cp_loss_fun <- function(data, var_name = "n_miss_visits", return_miss_only = FALSE, return_loss_fun_tab_only = FALSE,
                            week_period=FALSE, specify_cp = NULL, loss_function = "RMSE"){

  #Require some necessary packages
  require(tidyverse)

  if (!loss_function %in% c("MSE", "RMSE", "MAE", "MSLE", "RMSLE")){
    stop('The method selected is not avaiable, please select from one of the following:
         "MSE", "RMSE", "MAE", "MSLE", or "RMSLE"')
  }

  #Reorder data for easy time series usage
  cp_out <- arrange(data, -period)

  #Create a dummy column that is variable of interest
  cp_out$var_name <- cp_out[[var_name]]

  # fit linear model by period from 0 to max period - 2
  loss_fun_table <- tibble()
  data <- cp_out %>% arrange(period) %>%
    mutate(week_period = as.factor(period %% 7))
  for (i in data %>% filter(period < max(period) -1) %>% .$period){
    dataset <- data %>% filter(period > i)

    if(week_period){
      pred <- predict.lm(lm(var_name ~ period + week_period, data = dataset))
    } else {
      pred <- predict.lm(lm(var_name ~ period, data = dataset))
    }

    out <- dataset %>% mutate(pred = pred)

    suppressWarnings(
    out1 <- tibble(period = i,
                   RMSE = RMSE(out$pred, out$var_name),
                   MSE = MSE(out$pred, out$var_name),
                   MAE = MAE(out$pred, out$var_name),
                   MSLE = MSLE(out$pred, out$var_name),
                   RMSLE = RMSLE(out$pred, out$var_name))
    )

    loss_fun_table <- bind_rows(loss_fun_table, out1)
  }

  if (return_loss_fun_tab_only){
   return(loss_fun_table)
  }

  #Identify CP and find which period it corresponds to
  if (is.null(specify_cp)){
    local_min <- which(diff(sign(diff(loss_fun_table[[loss_function]])))==-2)+1
    cp <- local_min[1]
  } else {
    cp <- specify_cp
  }

  #Extract data for the model, all periods after cp
  model_data <- cp_out %>% filter(period > cp) %>% mutate(period_neg=-1*period) %>%
    mutate(week_period = as.factor(period %% 7))

  #Fit model, different if request periodicity
  if(week_period){
    model <- lm(var_name ~ period_neg + week_period, data = model_data)
  } else{
    model <- lm(var_name ~ period_neg, data = model_data)
  }

  #Get prediction covariates and make predictions
  if(week_period){
    pred_vars <- cp_out %>% mutate(period_neg = -1*period) %>%
      mutate(week_period = as.factor(period %% 7)) %>% select(var_name, period_neg, week_period)
  } else{
    pred_vars <- cp_out %>% mutate(period_neg = -1*period) %>% select(var_name, period_neg)
  }

  model_pred_intervals <- predict.lm(model, pred_vars, interval = "prediction", level = 0.90)

  #Collect all data needed for cp_out in this function

  #First get miss bins and statistics. Hard code num_miss and num_miss_upper_int to be
  #floored at 0
  miss_bins <- data.frame(period=cp_out$period,
                          Y=cp_out$var_name,
                          pred1= model_pred_intervals[, "fit"],
                          lower_int_pred1 = model_pred_intervals[, "lwr"],
                          upper_int_pred1 = model_pred_intervals[, "upr"],
                          pred=cp_out$var_name,
                          num_miss = cp_out$var_name - model_pred_intervals[, "fit"],
                          num_miss_upper_int = cp_out$var_name - model_pred_intervals[, "upr"]) %>%
    mutate(num_miss = num_miss*(num_miss>=0)) %>%
    mutate(num_miss_upper_int = num_miss_upper_int*(num_miss_upper_int>=0))

  #Filter to only times beyond CP
  miss_bins <- miss_bins %>% filter(period <= cp)
  if (return_miss_only){
    return(miss_bins)
  }

  #Output data about the changepoint itself
  change_point <- data.frame(Y = cp_out %>% filter(period == cp) %>% .$var_name,
                               t = which(cp_out$period == cp),
                               period = cp)

  #Output data about predictions
  pred <- data.frame(period=cp_out$period,
                     Y=cp_out$var_name,
                     t = 1:nrow(cp_out),
                     pred1= model_pred_intervals[, "fit"],
                     lower_int_pred1 = model_pred_intervals[, "lwr"],
                     upper_int_pred1 = model_pred_intervals[, "upr"],
                     pred=cp_out$var_name,
                     num_miss = cp_out$var_name - model_pred_intervals[, "fit"],
                     num_miss_upper_int = cp_out$var_name - model_pred_intervals[, "upr"])

  cp_plot <- pred %>% mutate(t = t-max(t)) %>% ggplot2::ggplot(aes(t, pred)) +
    ggtitle(paste0("Loss function = ", loss_function, " & week effect = ", week_period))+
    ggplot2::geom_line(aes(y = pred1), color = "red",size=.8) +
    geom_ribbon(aes(ymin = lower_int_pred1, ymax = upper_int_pred1), fill = "red", alpha = 0.2)+
    ggplot2::geom_line(size=.8) +
    ggplot2::geom_point(aes(t,Y),size=.8) +
    ggplot2::theme_light() +
    ggplot2::geom_vline(xintercept = change_point$period*-1 , color="blue", size=.8)

  #Compile output
  cp_out <- list(miss_bins=miss_bins,
                 change_point=change_point,
                 pred=pred,
                 model=model,
                 cp_plot=cp_plot)


  return(cp_out)

}


#A function to identify the changepoint using the Pettitt method. Requires:
#data: an output of count_prior_events_truven
#var_name: a character string of the var we are modeling for
#return_miss_only: a logical to only return miss visit counts
#week_period: a logical to fit the linear before model with factors for days until index mod 7,
#i.e. to have a crude form of week periodicity

#' Identify changepoint using pettitt method, and find expected SSD visits/calculate misses
#' by fitting a linear model before the changepoint
#'
#' @param data A dataframe output by count_prior_events_truven
#' @param var_name A character string of outcome for which to apply analysis
#' @param return_miss_only Logical to only return miss information
#' @param week_period Logical to incorporate a "day of the week" effect into
#' the linear model. Note this is only sensible for one-day period aggregation.
#' @return A list containing miss information, changepoint information, predictions,
#' the model itself, and a plot of the middle finger curve and model.
#' @param specify_cp Set a specific change point you want to use instead of searching for optimal change point. Enter a postive integer value
#' repersenting the days before the index on which you you want to specify the change point. (e.g. 100 would be 100 days before the index)
#' @examples
#' cp_result_pettit <- final_time_map %>%
#' filter(days_since_dx >= -180) %>%
#' count_prior_events_truven(event_name = "any_ssd", start_day = 1, by_days = 1) %>%
#' find_cp_pettitt(var_name = "n_miss_visits", return_miss_only = FALSE, week_period=TRUE)
#' @export
find_cp_pettitt <- function(data, var_name = "n_miss_visits", return_miss_only = FALSE,
                            week_period=FALSE, specify_cp = NULL){

  #Require some necessary packages
  require(trend)
  require(changepoint)
  require(tidyverse)

  #Reorder data for easy time series usage
  cp_out <- arrange(data, -period)

  #Create a dummy column that is variable of interest
  cp_out$var_name <- cp_out[[var_name]]


  #Convert to a time series object
  t_series <- ts(cp_out$var_name, start = min(-1*cp_out$period),
                   frequency = 1)

  if (is.null(specify_cp)){
    #Identify CP and find which period it corresponds to
    cp <- cp_out$period[pettitt.test(t_series)$estimate[[1]]]
  } else {
    cp <- specify_cp
  }

  #Extract data for the model, all periods after cp
  model_data <- cp_out %>% filter(period > cp) %>% mutate(period_neg=-1*period) %>%
    mutate(week_period = as.factor(period %% 7))

  #Fit model, different if request periodicity
  if(week_period){
  model <- lm(var_name ~ period_neg + week_period, data=model_data)
  } else{
  model <- lm(var_name ~ period_neg, data=model_data)
  }

  #Get prediction covariates and make predictions
  if(week_period){
    pred_vars <- cp_out %>% mutate(period_neg=-1*period) %>%
      mutate(week_period = as.factor(period %% 7)) %>% select(var_name,period_neg,week_period)
  } else{
  pred_vars <- cp_out %>% mutate(period_neg=-1*period) %>% select(var_name,period_neg)
  }

  model_pred_intervals <- predict.lm(model, pred_vars, interval = "prediction", level = 0.90)

  #Collect all data needed for cp_out in this function

  #First get miss bins and statistics. Hard code num_miss and num_miss_upper_int to be
  #floored at 0
  miss_bins <- data.frame(period=cp_out$period,
                          Y=cp_out$var_name,
                          pred1= model_pred_intervals[, "fit"],
                          lower_int_pred1 = model_pred_intervals[, "lwr"],
                          upper_int_pred1 = model_pred_intervals[, "upr"],
                          pred=cp_out$var_name,
                          num_miss = cp_out$var_name - model_pred_intervals[, "fit"],
                          num_miss_upper_int = cp_out$var_name - model_pred_intervals[, "upr"]) %>%
    mutate(num_miss = num_miss*(num_miss>=0)) %>%
    mutate(num_miss_upper_int = num_miss_upper_int*(num_miss_upper_int>=0))

  #Filter to only times beyond CP
  miss_bins <- miss_bins %>% filter(period<=cp)
  if (return_miss_only){
    return(miss_bins)
  }

  if (is.null(specify_cp)){
    #Output data about the changepoint itself
    change_point <- data.frame(Y = cp_out$var_name[pettitt.test(t_series)$estimate[[1]]],
                               t = pettitt.test(t_series)$estimate[[1]],
                               period = cp)
  } else {
    #Output data about the changepoint itself
    change_point <- data.frame(Y = cp_out %>% filter(period == cp) %>% .$var_name,
                               t = which(cp_out$period == cp),
                               period = cp)
  }

  #Output data about predictions
  pred <- data.frame(period=cp_out$period,
                     Y=cp_out$var_name,
                     t = 1:nrow(cp_out),
                     pred1= model_pred_intervals[, "fit"],
                     lower_int_pred1 = model_pred_intervals[, "lwr"],
                     upper_int_pred1 = model_pred_intervals[, "upr"],
                     pred=cp_out$var_name,
                     num_miss = cp_out$var_name - model_pred_intervals[, "fit"],
                     num_miss_upper_int = cp_out$var_name - model_pred_intervals[, "upr"])


  cp_plot <- pred %>% mutate(t = t-max(t)) %>% ggplot2::ggplot(aes(t, pred)) +
    ggtitle(paste0("Change point method = Pettitt & week effect = ", week_period))+
    ggplot2::geom_line(aes(y = pred1), color = "red",size=.8) +
    geom_ribbon(aes(ymin = lower_int_pred1, ymax = upper_int_pred1), fill = "red", alpha = 0.2)+
    ggplot2::geom_line(size=.8) +
    ggplot2::geom_point(aes(t,Y),size=.8) +
    ggplot2::theme_light() +
    ggplot2::geom_vline(xintercept = change_point$period*-1 , color="blue", size=.8)


  #Compile output
  cp_out <- list(miss_bins=miss_bins,
                 change_point=change_point,
                 pred=pred,
                 model=model,
                 cp_plot=cp_plot)


  return(cp_out)

}


#A function to identify the changepoint using the CUSUM method. Requires:
#data: an output of count_prior_events_truven
#var_name: a character string of the var we are modeling for
#return_miss_only: a logical to only return miss visit counts
#week_period: a logical to fit the linear before model with factors for days until index mod 7,
#i.e. to have a crude form of week periodicity


#' Identify changepoint using CUSUM method, and find expected SSD visits/calculate misses
#' by fitting a linear model before the changepoint
#'
#' @param data A dataframe output by count_prior_events_truven
#' @param var_name A character string of outcome for which to apply analysis
#' @param return_miss_only Logical to only return miss information
#' @param week_period Logical to incorporate a "day of the week" effect into
#' the linear model. Note this is only sensible for one-day period aggregation.
#' @param specify_cp Set a specific change point you want to use instead of searching for optimal change point. Enter a postive integer value
#' repersenting the days before the index on which you you want to specify the change point. (e.g. 100 would be 100 days before the index)
#' @return A list containing miss information, changepoint information, predictions,
#' the model itself, and a plot of the middle finger curve and model.
#' @examples
#' cp_result_cusum <- final_time_map %>%
#' filter(days_since_dx >= -180) %>%
#' count_prior_events_truven(event_name = "any_ssd", start_day = 1, by_days = 1) %>%
#' find_cp_cusum(var_name = "n_miss_visits", return_miss_only = FALSE, week_period=TRUE)
#' @export
find_cp_cusum <- function(data, var_name = "n_miss_visits", return_miss_only = FALSE,
                            week_period=FALSE, specify_cp = NULL){

  #Require some necessary packages
  require(trend)
  require(changepoint)
  require(tidyverse)

  #Reorder data for easy time series usage
  cp_out <- arrange(data, -period)

  #Create a dummy column that is variable of interest
  cp_out$var_name <- cp_out[[var_name]]

  #Convert to a time series object
  t_series <- ts(cp_out$var_name, start = min(-1*cp_out$period),
                 frequency = 1)

  if (is.null(specify_cp)){
  #Identify CP and find which period it corresponds to
    cp_est <- suppressWarnings( cpts(cpt.mean(t_series,pen.value=1,penalty='None',test.stat='CUSUM')) )
    cp <- cp_out$period[cp_est]
  } else {
    cp <- specify_cp
  }

  #Extract data for the model, all periods after cp
  model_data <- cp_out %>% filter(period > cp) %>% mutate(period_neg=-1*period) %>%
    mutate(week_period = as.factor(period %% 7))

  #Fit model, different if request periodicity
  if(week_period){
    model <- lm(var_name ~ period_neg + week_period, data=model_data)
  } else{
    model <- lm(var_name ~ period_neg, data=model_data)
  }

  #Get prediction covariates and make predictions
  if(week_period){
    pred_vars <- cp_out %>% mutate(period_neg=-1*period) %>%
      mutate(week_period = as.factor(period %% 7)) %>% select(var_name,period_neg,week_period)
  } else{
    pred_vars <- cp_out %>% mutate(period_neg=-1*period) %>% select(var_name,period_neg)
  }

  model_pred_intervals <- predict.lm(model, pred_vars, interval = "prediction", level = 0.90)

  #Collect all data needed for cp_out in this function

  #First get miss bins and statistics. Hard code num_miss and num_miss_upper_int to be
  #floored at 0
  miss_bins <- data.frame(period=cp_out$period,
                          Y=cp_out$var_name,
                          pred1= model_pred_intervals[, "fit"],
                          lower_int_pred1 = model_pred_intervals[, "lwr"],
                          upper_int_pred1 = model_pred_intervals[, "upr"],
                          pred=cp_out$var_name,
                          num_miss = cp_out$var_name - model_pred_intervals[, "fit"],
                          num_miss_upper_int = cp_out$var_name - model_pred_intervals[, "upr"]) %>%
    mutate(num_miss = num_miss*(num_miss>=0)) %>%
    mutate(num_miss_upper_int = num_miss_upper_int*(num_miss_upper_int>=0))

  #Filter to only times beyond CP
  miss_bins <- miss_bins %>% filter(period<=cp)
  if (return_miss_only){
    return(miss_bins)
  }

  if (is.null(specify_cp)){
    #Output data about the changepoint itself
    change_point <- data.frame(Y = cp_out$var_name[cp_est],
                               t = cp_est,
                               period = cp)
  } else {
    #Output data about the changepoint itself
    change_point <- data.frame(Y = cp_out %>% filter(period == cp) %>% .$var_name,
                               t = which(cp_out$period == cp),
                               period = cp)
  }

  #Output data about predictions
  pred <- data.frame(period=cp_out$period,
                     Y=cp_out$var_name,
                     t = 1:nrow(cp_out),
                     pred1= model_pred_intervals[, "fit"],
                     lower_int_pred1 = model_pred_intervals[, "lwr"],
                     upper_int_pred1 = model_pred_intervals[, "upr"],
                     pred=cp_out$var_name,
                     num_miss = cp_out$var_name - model_pred_intervals[, "fit"],
                     num_miss_upper_int = cp_out$var_name - model_pred_intervals[, "upr"])


  cp_plot <- pred %>% mutate(t = t-max(t)) %>% ggplot2::ggplot(aes(t, pred)) +
    ggtitle(paste0("Change point method = CUSUM & week effect = ", week_period))+
    ggplot2::geom_line(aes(y = pred1), color = "red",size=.8) +
    geom_ribbon(aes(ymin = lower_int_pred1, ymax = upper_int_pred1), fill = "red", alpha = 0.2)+
    ggplot2::geom_line(size=.8) +
    ggplot2::geom_point(aes(t,Y),size=.8) +
    ggplot2::theme_light() +
    ggplot2::geom_vline(xintercept = change_point$period*-1 , color="blue", size=.8)


  #Compile output
  cp_out <- list(miss_bins=miss_bins,
                 change_point=change_point,
                 pred=pred,
                 model=model,
                 cp_plot=cp_plot)

  return(cp_out)

}


#This function is the old find_change_point function, that finds changepoint using
#linear regression with a changepoint

#' Find the change point in count data using linear regression models
#'
#' @param data A dataset of visit counts
#' @param var_name The name of the count variable to find the change-point for
#' @param method The method used to fit curves before and after the changepoint. Options include "lm",
#' "lm_quad", "lm_cube", "quad", "cube", "exp", "spline"
#' @param eval_criteria The evaluation criteria used to find change points
#' @param return_miss_only Logical argument to only return the tibbles of miss visit counts
#' @param specify_cp Set a specific change point you want to use instead of searching for optimal change point. Enter a postive integer value
#' repersenting the days before the index on which you you want to specify the change point. (e.g. 100 would be 100 days before the index)
#' @examples
#' cp_result_original <- final_time_map %>%
#' count_prior_events_truven(event_name = "any_ssd", start_day = 1, by_days = 1) %>%
#' find_cp_linreg(var_name="n_miss_visits", method="lm_cube")
#'
#' @export
find_cp_linreg <- function(data,var_name="n_miss_visits",method="lm",eval_criteria="AIC", return_miss_only=FALSE,
                           specify_cp = NULL){

  data$var_name <- data[[var_name]]

  data <- data %>%
    dplyr::arrange(dplyr::desc(period)) %>%
    dplyr::mutate(Y=var_name,
                  t=dplyr::row_number())

  if (is.null(specify_cp)) {
    if (method=="spline"){
      fits = tibble::tibble(cp=3:max(data$t)) %>%
        dplyr::mutate(res=purrr::map(cp,
                                     ~fit_cp_spline(data = data, x=.) )) %>%
        tidyr::unnest(res)
    }

    if (method=="lm"){
      fits = tibble::tibble(cp=2:max(data$t)) %>%
        dplyr::mutate(res=purrr::map(cp, ~fit_cp_lm(data = data, x=.) )) %>%
        tidyr::unnest(res)
    }

    if (method=="lm_cube"){
      fits = tibble::tibble(cp=2:max(data$t)) %>%
        dplyr::mutate(res=purrr::map(cp, ~fit_cp_lm_cube(data = data, x=.) )) %>%
        tidyr::unnest(res)
    }

    if (method=="cube"){
      fits = tibble::tibble(cp=2:max(data$t)) %>%
        dplyr::mutate(res=purrr::map(cp, ~fit_cp_cube(data = data, x=.) )) %>%
        tidyr::unnest(res)
    }

    if (method=="quad"){
      fits = tibble::tibble(cp=2:max(data$t)) %>%
        dplyr::mutate(res=purrr::map(cp, ~fit_cp_quad(data = data, x=.) )) %>%
        tidyr::unnest(res)
    }

    if (method=="lm_quad"){
      fits = tibble::tibble(cp=2:max(data$t)) %>%
        dplyr::mutate(res=purrr::map(cp, ~fit_cp_lm_quad(data = data, x=.) )) %>%
        tidyr::unnest(res)
    }

    if (method=="exp"){
      fits = tibble::tibble(cp=2:max(data$t)) %>%
        dplyr::mutate(res=purrr::map(cp, ~fit_cp_exp(data = data, x=.) )) %>%
        tidyr::unnest(res)
    }

    if (eval_criteria %in% c("r.squared","adj.r.squared")){
      change_t <- fits$cp[fits[eval_criteria]==max(fits[eval_criteria])]
    } else {
      change_t <- fits$cp[fits[eval_criteria]==min(fits[eval_criteria])]
    }
  } else{
    change_t <- data$t[data$period == specify_cp]

    if (method=="spline"){
      fits = fit_cp_spline(data = data, x=change_t)
    }

    if (method=="lm"){
      fits = fit_cp_lm(data = data, x=change_t)
    }

    if (method=="lm_cube"){
      fits = fit_cp_lm_cube(data = data, x=change_t)
    }

    if (method=="cube"){
      fits = fit_cp_cube(data = data, x=change_t)
    }

    if (method=="quad"){
      fits = fit_cp_quad(data = data, x=change_t)
    }

    if (method=="lm_quad"){
      fits = fit_cp_lm_quad(data = data, x=change_t)
    }

    if (method=="exp"){
      fits = fit_cp_exp(data = data, x=change_t)
    }
  }

  if (method=="spline"){
    out <- fit_cp_spline(data = data, x=change_t,return_all = TRUE)
  }

  if (method=="quad"){
    out <- fit_cp_quad(data = data, x=change_t,return_all = TRUE)
  }

  if (method=="cube"){
    out <- fit_cp_cube(data = data, x=change_t,return_all = TRUE)
  }

  if (method=="lm"){
    out <- fit_cp_lm(data = data, x=change_t,return_all = TRUE)
  }

  if (method=="lm_quad"){
    out <- fit_cp_lm_quad(data = data, x=change_t,return_all = TRUE)
  }

  if (method=="lm_cube"){
    out <- fit_cp_lm_cube(data = data, x=change_t,return_all = TRUE)
  }

  if (method=="exp"){
    out <- fit_cp_exp(data = data, x=change_t,return_all = TRUE)
  }


  change_point <- data %>%
    dplyr::filter(t==change_t) %>%
    dplyr::select(Y,t,period)

  cp_plot <- out$pred %>% mutate(t = t-max(t)) %>% ggplot2::ggplot(aes(t, pred)) +
    ggtitle(paste0("Change point method = ", method)) +
    ggplot2::geom_line(aes(y = pred1), color = "red", size=.8) +
    ggplot2::geom_line(size=.8) +
    ggplot2::geom_point(aes(t,Y), size=.8) +
    ggplot2::theme_light() +
    ggplot2::geom_vline(xintercept = change_point$period*-1 , color="blue", size=.8) +
    ggplot2::geom_ribbon(aes(x=t, ymin = pred_low, ymax = pred_high),
                         fill = "red", alpha = 0.2, inherit.aes = FALSE)

  miss_bins <- out$pred  %>%
    dplyr::mutate(num_miss=pred-pred1) %>%
    dplyr::filter(num_miss>0) %>%
    dplyr::select(period,Y,pred,pred1,num_miss)

  miss_stats <- miss_bins %>%
    dplyr::summarise(miss_visits_est=sum(num_miss),
                     miss_visits_obs=sum(Y-pred1))

  if (return_miss_only==TRUE){
    out <- miss_bins
  } else {
    out <- c(list(change_point = change_point,
                  cp_plot = cp_plot,
                  cp_fits = fits),
             out,
             list(miss_bins = miss_bins,
                  miss_stats = miss_stats))
  }

  return(out)

}

#Wrapper function for all of these, so that they can be called easier. Should encompass
#all options past and present, and be 100% backward compatible


#' Find the change point in count data. This is a backwards-compatible wrapper function to
#' find the changepoint, which calls other methods.
#'
#' @param data A dataset of visit counts, output by count_prior_events_truven
#' @param var_name The name of the count variable to find the change-point for
#' @param method The method used to find changepoint. Options include "lm",
#' "lm_quad", "lm_cube", "quad", "cube", "exp", "spline", "pettitt", "cusum",
#' "MSE", "RMSE", "MAE", "MSLE", "RMSLE"
#' @param eval_criteria The evaluation criteria used to find change points, if using a
#' linear regression method
#' @param week_period Logical to incorporate a "day of the week" effect into the linear model, if
#' method is "pettitt" of "cusum". Note this is only sensible for one-day period aggregation
#' @param return_miss_only Logical argument to only return the tibbles of miss visit counts
#' @param specify_cp Set a specific change point you want to use instead of searching for optimal change point. Enter a postive integer value
#' repersenting the days before the index on which you you want to specify the change point. (e.g. 100 would be 100 days before the index)
#' @return A list containing tibbles of information about missed visits. These tibbles change
#' depending on the method used, but all contain miss predictions and a plot
#'
#' @examples
#'
#' cp_result_original <- final_time_map %>%
#' count_prior_events_truven(event_name = "any_ssd", start_day = 1, by_days = 1) %>%
#' find_change_point(var_name="n_miss_visits", method="lm_cube")
#'
#' @export
find_change_point <- function(data,var_name="n_miss_visits",method,eval_criteria="AIC",
                              return_miss_only=FALSE, week_period=FALSE, specify_cp = NULL){

  if(method %in% c("lm","lm_quad","lm_cube", "quad", "cube", "exp", "spline")){
    output <- find_cp_linreg(data, var_name=var_name,method=method,eval_criteria = eval_criteria,
                             return_miss_only = return_miss_only,
                             specify_cp = specify_cp)
    return(output)
  } else if(method=="pettitt"){
    output <- find_cp_pettitt(data, var_name=var_name, return_miss_only = return_miss_only,
                              week_period = week_period,
                              specify_cp = specify_cp)
    return(output)
  } else if(method=="cusum"){
    output <- find_cp_cusum(data, var_name=var_name, return_miss_only = return_miss_only,
                            week_period = week_period,
                            specify_cp = specify_cp)
    return(output)
  } else if(method %in% c("MSE", "RMSE", "MAE", "MSLE", "RMSLE")){
    output <- find_cp_loss_fun(data, var_name = var_name, return_miss_only = return_miss_only,
                               loss_function = method,
                               return_loss_fun_tab_only = FALSE,
                               week_period = week_period,
                               specify_cp = specify_cp)
    return(output)
  } else{
    cat("Error: No valid method was supplied. Returning ")
    return(NULL)
  }

}

