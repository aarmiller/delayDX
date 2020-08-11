#' Set a change point and fit a linear model on data prior to the change point to obtian expected SSD visits/calculate misses
#'
#' @title set_cp_lm
#' @param data A dataframe output by count_prior_events_truven
#' @param var_name A character string of outcome for which to apply analysis
#' @param return_miss_only Logical to only return miss information
#' @param return_eval_only Logical to only return model evaluation criteria info
#' @param week_period Logical to incorporate a "day of the week" effect into model. Note this is only sensible for one-day period aggregation
#' @param specify_cp A postive integer value for the specific change point you want to use. The value represents
#' the days before the index on which you you want to specify the change point (e.g. 100 would be 100 days before the index)
#'@return A list containing miss information, changepoint information, predictions,
#' the model itself, and a plot of the middle finger curve and model
#'

#' @export
#'

set_cp_lm <- function(data, var_name = "n_miss_visits", return_miss_only = FALSE, return_eval_only = FALSE,
                            week_period=FALSE, specify_cp = NULL){

  #Require some necessary packages
  require(tidyverse)

  # error if not change point specified
  if(is.null(specify_cp)){
    stop(paste0("Please specify a postive integer value that represents the days before the",
         " index on which you you want to specify the change point. (e.g. 100 would be 100 days before the index"))
  }
  #Reorder data for easy time series usage
  cp_out <- arrange(data, -period)

  #Create a dummy column that is variable of interest
  cp_out$var_name <- cp_out[[var_name]]

  #Set the change point
  cp <- specify_cp

  #Extract data for the model, all periods after cp
  model_data <- cp_out %>% filter(period > cp) %>% mutate(period_neg = -1*period) %>%
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
    pred_at_or_prior_cp1 <- model_data %>% select(var_name, period_neg, week_period)
  } else{
    pred_vars <- cp_out %>% mutate(period_neg = -1*period) %>% select(var_name, period_neg)
    pred_at_or_prior_cp1 <- model_data %>% select(var_name, period_neg)
  }

  #get predicted values for entire dataset
  model_pred_intervals <- predict.lm(model, pred_vars, interval = "prediction", level = 0.90)

  #get predicted values for data prior to cp
  pred_at_or_prior_cp <- predict.lm(model, pred_at_or_prior_cp1, interval = "prediction")

  #Get model fit info
  AIC <- AIC(model)
  rsqr <- summary(model)$r.squared
  adjrsqr <- summary(model)$adj.r.squared

  eval_table <- tibble(model = "lm",
                       AIC = AIC,
                       r.squared = rsqr,
                       adj.r.squared = adjrsqr,
                       MSE = MSE(pred_at_or_prior_cp[, "fit"], model_data$var_name),
                       RMSE = RMSE(pred_at_or_prior_cp[, "fit"], model_data$var_name),
                       MSLE = MSLE(pred_at_or_prior_cp[, "fit"], model_data$var_name),
                       RMSLE = RMSLE(pred_at_or_prior_cp[, "fit"], model_data$var_name),
                       MAE = MAE(pred_at_or_prior_cp[, "fit"], model_data$var_name))

  if (return_eval_only){
    return(eval_table)
  }

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
    ggtitle(paste0("Method = 'lm'", " & day of the week = ", week_period))+
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

#' Set a change point and fit a quadratic model on data prior to the change point to obtian expected SSD visits/calculate misses
#'
#' @title set_cp_quad
#' @param data A dataframe output by count_prior_events_truven
#' @param var_name A character string of outcome for which to apply analysis
#' @param return_miss_only Logical to only return miss information
#' @param return_eval_only Logical to only return model evaluation criteria info
#' @param week_period Logical to incorporate a "day of the week" effect into model. Note this is only sensible for one-day period aggregation.
#' @param specify_cp A postive integer value for the specific change point you want to use. The value represents
#' the days before the index on which you you want to specify the change point (e.g. 100 would be 100 days before the index)
#'@return A list containing miss information, changepoint information, predictions,
#' the model itself, and a plot of the middle finger curve and model
#'

#' @export
#'

set_cp_quad <- function(data, var_name = "n_miss_visits", return_miss_only = FALSE, return_eval_only = FALSE,
                   week_period=FALSE, specify_cp = NULL){

  #Require some necessary packages
  require(tidyverse)

  # error if not change point specified
  if(is.null(specify_cp)){
    stop(paste0("Please specify a postive integer value that represents the days before the",
                " index on which you you want to specify the change point. (e.g. 100 would be 100 days before the index"))
  }
  #Reorder data for easy time series usage
  cp_out <- arrange(data, -period)

  #Create a dummy column that is variable of interest
  cp_out$var_name <- cp_out[[var_name]]

  #Set the change point
  cp <- specify_cp

  #Extract data for the model, all periods after cp
  model_data <- cp_out %>% filter(period > cp) %>%
    mutate(period_sqr = period^2,
           period_neg = -1*period,
           period_sqr_neg = -1*period_sqr) %>%
    mutate(week_period = as.factor(period %% 7))

  #Fit model, different if request periodicity
  if(week_period){
    model <- lm(var_name ~ period_neg + period_sqr_neg + week_period, data = model_data)
  } else{
    model <- lm(var_name ~ period_neg + period_sqr_neg, data = model_data)
  }

  #Get prediction covariates and make predictions
  if(week_period){
    pred_vars <- cp_out %>% mutate(period_sqr = period^2,
                                      period_neg = -1*period,
                                      period_sqr_neg = -1*period_sqr) %>%
      mutate(week_period = as.factor(period %% 7)) %>%
      select(var_name, period_neg, period_sqr_neg, week_period)
    pred_at_or_prior_cp1 <- model_data %>% select(var_name, period_neg, period_sqr_neg, week_period)
  } else{
    pred_vars <- cp_out %>% mutate(period_sqr = period^2,
                                       period_neg = -1*period,
                                       period_sqr_neg = -1*period_sqr) %>%
      select(var_name, period_neg, period_sqr_neg)
    pred_at_or_prior_cp1 <- model_data %>% select(var_name, period_neg, period_sqr_neg)
  }

  #get predicted values for entire dataset
  model_pred_intervals <- predict.lm(model, pred_vars, interval = "prediction", level = 0.90)

  #get predicted values for data prior to cp
  pred_at_or_prior_cp <- predict.lm(model, pred_at_or_prior_cp1, interval = "prediction")

  #Get model fit info
  AIC <- AIC(model)
  rsqr <- summary(model)$r.squared
  adjrsqr <- summary(model)$adj.r.squared

  eval_table <- tibble(model = "quad",
                       AIC = AIC,
                       r.squared = rsqr,
                       adj.r.squared = adjrsqr,
                       MSE = MSE(pred_at_or_prior_cp[, "fit"], model_data$var_name),
                       RMSE = RMSE(pred_at_or_prior_cp[, "fit"], model_data$var_name),
                       MSLE = MSLE(pred_at_or_prior_cp[, "fit"], model_data$var_name),
                       RMSLE = RMSLE(pred_at_or_prior_cp[, "fit"], model_data$var_name),
                       MAE = MAE(pred_at_or_prior_cp[, "fit"], model_data$var_name))

  if (return_eval_only){
    return(eval_table)
  }

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
    ggtitle(paste0("Method = 'quad'", " & day of the week = ", week_period))+
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

#' Set a change point and fit a cubic model on data prior to the change point to obtian expected SSD visits/calculate misses
#'
#' @title set_cp_cubic
#' @param data A dataframe output by count_prior_events_truven
#' @param var_name A character string of outcome for which to apply analysis
#' @param return_miss_only Logical to only return miss information
#' @param return_eval_only Logical to only return model evaluation criteria info
#' @param week_period Logical to incorporate a "day of the week" effect into model. Note this is only sensible for one-day period aggregation.
#' @param specify_cp A postive integer value for the specific change point you want to use. The value represents
#' the days before the index on which you you want to specify the change point (e.g. 100 would be 100 days before the index)
#'@return A list containing miss information, changepoint information, predictions,
#' the model itself, and a plot of the middle finger curve and model
#'

#' @export
#'

set_cp_cubic <- function(data, var_name = "n_miss_visits", return_miss_only = FALSE, return_eval_only = FALSE,
                     week_period=FALSE, specify_cp = NULL){

  #Require some necessary packages
  require(tidyverse)

  # error if not change point specified
  if(is.null(specify_cp)){
    stop(paste0("Please specify a postive integer value that represents the days before the",
                " index on which you you want to specify the change point. (e.g. 100 would be 100 days before the index"))
  }
  #Reorder data for easy time series usage
  cp_out <- arrange(data, -period)

  #Create a dummy column that is variable of interest
  cp_out$var_name <- cp_out[[var_name]]

  #Set the change point
  cp <- specify_cp

  #Extract data for the model, all periods after cp
  model_data <- cp_out %>% filter(period > cp) %>%
    mutate(period_sqr = period^2,
           period_cube = period^3,
           period_neg = -1*period,
           period_sqr_neg = -1*period_sqr,
           period_cube_neg = -1*period_cube) %>%
    mutate(week_period = as.factor(period %% 7))

  #Fit model, different if request periodicity
  if(week_period){
    model <- lm(var_name ~ period_neg + period_sqr_neg + period_cube_neg + week_period, data = model_data)
  } else{
    model <- lm(var_name ~ period_neg + period_sqr_neg + period_cube_neg, data = model_data)
  }

  #Get prediction covariates and make predictions
  if(week_period){
    pred_vars <- cp_out %>% mutate(period_sqr = period^2,
                                   period_cube = period^3,
                                   period_neg = -1*period,
                                   period_sqr_neg = -1*period_sqr,
                                   period_cube_neg = -1*period_cube) %>%
      mutate(week_period = as.factor(period %% 7)) %>%
      select(var_name, period_neg, period_sqr_neg, period_cube_neg, week_period)
    pred_at_or_prior_cp1 <- model_data %>% select(var_name, period_neg, period_sqr_neg, period_cube_neg, week_period)
  } else{
    pred_vars <- cp_out %>% mutate(period_sqr = period^2,
                                   period_cube = period^3,
                                   period_neg = -1*period,
                                   period_sqr_neg = -1*period_sqr,
                                   period_cube_neg = -1*period_cube) %>%
      select(var_name, period_neg, period_sqr_neg, period_cube_neg)
    pred_at_or_prior_cp1 <- model_data %>% select(var_name, period_neg, period_sqr_neg, period_cube_neg)
  }

  #get predicted values for entire dataset
  model_pred_intervals <- predict.lm(model, pred_vars, interval = "prediction", level = 0.90)

  #get predicted values for data prior to cp
  pred_at_or_prior_cp <- predict.lm(model, pred_at_or_prior_cp1, interval = "prediction")

  #Get model fit info
  AIC <- AIC(model)
  rsqr <- summary(model)$r.squared
  adjrsqr <- summary(model)$adj.r.squared

  eval_table <- tibble(model = "cubic",
                       AIC = AIC,
                       r.squared = rsqr,
                       adj.r.squared = adjrsqr,
                       MSE = MSE(pred_at_or_prior_cp[, "fit"], model_data$var_name),
                       RMSE = RMSE(pred_at_or_prior_cp[, "fit"], model_data$var_name),
                       MSLE = MSLE(pred_at_or_prior_cp[, "fit"], model_data$var_name),
                       RMSLE = RMSLE(pred_at_or_prior_cp[, "fit"], model_data$var_name),
                       MAE = MAE(pred_at_or_prior_cp[, "fit"], model_data$var_name))

  if (return_eval_only){
    return(eval_table)
  }

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
    ggtitle(paste0("Method = 'cubic'", " & day of the week = ", week_period))+
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


#' Set a change point and fit a model with a specified polynomial order on data prior to the change point to obtian expected SSD visits/calculate misses
#'
#' @title set_cp_poly
#' @param data A dataframe output by count_prior_events_truven
#' @param var_name A character string of outcome for which to apply analysis
#' @param return_miss_only Logical to only return miss information
#' @param return_eval_only Logical to only return model evaluation criteria info
#' @param week_period Logical to incorporate a "day of the week" effect into model. Note this is only sensible for one-day period aggregation.
#' @param specify_cp A postive integer value for the specific change point you want to use. The value represents
#' the days before the index on which you you want to specify the change point. (e.g. 100 would be 100 days before the index)
#' @param poly_order The polynomial order you want to apply (i.e. 'linear', 'quadratic', 'cubic', 'quartic', 'quintic', or 'sextic')
#' @param use_orthogonal_poly A logical to use raw or orthogonal polynomials. TRUE uses orthogonal polynomials. not raw
#'@return A list containing miss information, changepoint information, predictions,
#' the model itself, and a plot of the middle finger curve and model
#'

#' @export
#'

set_cp_poly <- function(data, var_name = "n_miss_visits", return_miss_only = FALSE, return_eval_only = FALSE,
                         week_period=FALSE, specify_cp = NULL, poly_order = "linear", use_orthogonal_poly = FALSE){

  # error if poly_order misspecified
  if (is.null(poly_order) || (!poly_order %in% c("linear", "quadratic", "cubic", "quartic", "quintic", "sextic"))){
    stop("The poly_order input is not available. Please select from one of the following:
         'linear', 'quadratic', 'cubic', 'quartic', 'quintic', or 'sextic'")
  }
  #names of polynomial orders
  poly_order_table <- tibble(name = c("linear", "quadratic", "cubic", "quartic", "quintic",  "sextic"),
                             poly_order = 1:6)

  poly_order_table1 <- poly_order_table[poly_order_table$name == poly_order, ]

  n_poly_order <- poly_order_table1 %>% .$poly_order

  name_poly_order <- poly_order_table1 %>% .$name

  #Require some necessary packages
  require(tidyverse)

  # error if no change point specified
  if(is.null(specify_cp)){
    stop(paste0("Please specify a postive integer value that represents the days before the",
                " index on which you you want to specify the change point. (e.g. 100 would be 100 days before the index"))
  }
  #Reorder data for easy time series usage
  cp_out <- arrange(data, -period)

  #Create a dummy column that is variable of interest
  cp_out$var_name <- cp_out[[var_name]]

  #Set the change point
  cp <- specify_cp

  #Extract data for the model, all periods after cp
  model_data <- cp_out %>% filter(period > cp) %>%
    mutate(period_neg = -1*period) %>%
    mutate(week_period = as.factor(period %% 7))

  #Fit model, different if request periodicity
  if(week_period){
    model <- lm(var_name ~ poly(period_neg, n_poly_order, raw = !use_orthogonal_poly) + week_period, data = model_data)
  } else{
    model <- lm(var_name ~ poly(period_neg, n_poly_order, raw = !use_orthogonal_poly), data = model_data)
  }

  #Get prediction covariates and make predictions
  if(week_period){
    pred_vars <- cp_out %>% mutate(period_neg = -1*period) %>%
      mutate(week_period = as.factor(period %% 7)) %>%
      select(var_name, period_neg, week_period)
    pred_at_or_prior_cp1 <- model_data %>% select(var_name, period_neg, week_period)
  } else{
    pred_vars <- cp_out %>% mutate(period_neg = -1*period) %>%
      select(var_name, period_neg)
    pred_at_or_prior_cp1 <- model_data %>% select(var_name, period_neg)
  }

  #get predicted values for entire dataset
  model_pred_intervals <- predict.lm(model, pred_vars, interval = "prediction", level = 0.90)

  #get predicted values for data prior to cp
  pred_at_or_prior_cp <- predict.lm(model, pred_at_or_prior_cp1, interval = "prediction")

  #Get model fit info
  AIC <- AIC(model)
  rsqr <- summary(model)$r.squared
  adjrsqr <- summary(model)$adj.r.squared

  eval_table <- tibble(model = name_poly_order,
                       AIC = AIC,
                       r.squared = rsqr,
                       adj.r.squared = adjrsqr,
                       MSE = MSE(pred_at_or_prior_cp[, "fit"], model_data$var_name),
                       RMSE = RMSE(pred_at_or_prior_cp[, "fit"], model_data$var_name),
                       MSLE = MSLE(pred_at_or_prior_cp[, "fit"], model_data$var_name),
                       RMSLE = RMSLE(pred_at_or_prior_cp[, "fit"], model_data$var_name),
                       MAE = MAE(pred_at_or_prior_cp[, "fit"], model_data$var_name))

  if (return_eval_only){
    return(eval_table)
  }

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
    ggtitle(paste0("Method = ", str_to_title(name_poly_order), " & day of the week = ", week_period))+
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


#' Set a change point and fit a exponential model on data prior to the change point to obtian expected SSD visits/calculate misses
#'
#' @title set_cp_exp
#' @param data A dataframe output by count_prior_events_truven
#' @param var_name A character string of outcome for which to apply analysis
#' @param return_miss_only Logical to only return miss information
#' @param return_eval_only Logical to only return model evaluation criteria info
#' @param week_period Logical to incorporate a "day of the week" effect into model. Note this is only sensible for one-day period aggregation.
#' @param specify_cp A postive integer value for the specific change point you want to use. The value represents
#' the days before the index on which you you want to specify the change point (e.g. 100 would be 100 days before the index)
#'@return A list containing miss information, changepoint information, predictions,
#' the model itself, and a plot of the middle finger curve and model
#'

#' @export
#'

set_cp_exp <- function(data, var_name = "n_miss_visits", return_miss_only = FALSE, return_eval_only = FALSE,
                      week_period=FALSE, specify_cp = NULL){

  #Require some necessary packages
  require(tidyverse)

  # error if not change point specified
  if(is.null(specify_cp)){
    stop(paste0("Please specify a postive integer value that represents the days before the",
                " index on which you you want to specify the change point. (e.g. 100 would be 100 days before the index"))
  }
  #Reorder data for easy time series usage
  cp_out <- arrange(data, -period)

  #Create a dummy column that is variable of interest
  cp_out$var_name <- cp_out[[var_name]]

  #Set the change point
  cp <- specify_cp

  #Extract data for the model, all periods after cp
  model_data <- cp_out %>% filter(period > cp) %>% mutate(period_neg = -1*period) %>%
    mutate(week_period = as.factor(period %% 7))

  #Fit model, different if request periodicity
  if(week_period){
    model <- lm(log(var_name) ~ period_neg + week_period, data = model_data)
  } else{
    model <- lm(log(var_name) ~ period_neg, data = model_data)
  }

  #Get prediction covariates and make predictions
  if(week_period){
    pred_vars <- cp_out %>% mutate(period_neg = -1*period) %>%
      mutate(week_period = as.factor(period %% 7)) %>% select(var_name, period_neg, week_period)
    pred_at_or_prior_cp1 <- model_data %>% select(var_name, period_neg, week_period)
  } else{
    pred_vars <- cp_out %>% mutate(period_neg = -1*period) %>% select(var_name, period_neg)
    pred_at_or_prior_cp1 <- model_data %>% select(var_name, period_neg)
  }

  #get predicted values for entire dataset
  model_pred_intervals <- exp(predict.lm(model, pred_vars, interval = "prediction", level = 0.90))

  #get predicted values for data prior to cp
  pred_at_or_prior_cp <- exp(predict.lm(model, pred_at_or_prior_cp1, interval = "prediction"))

  #Get model fit info
  AIC <- AIC(model)
  rsqr <- summary(model)$r.squared
  adjrsqr <- summary(model)$adj.r.squared

  eval_table <- tibble(model = "exponential",
                       AIC = AIC,
                       r.squared = rsqr,
                       adj.r.squared = adjrsqr,
                       MSE = MSE(pred_at_or_prior_cp[, "fit"], model_data$var_name),
                       RMSE = RMSE(pred_at_or_prior_cp[, "fit"], model_data$var_name),
                       MSLE = MSLE(pred_at_or_prior_cp[, "fit"], model_data$var_name),
                       RMSLE = RMSLE(pred_at_or_prior_cp[, "fit"], model_data$var_name),
                       MAE = MAE(pred_at_or_prior_cp[, "fit"], model_data$var_name))

  if (return_eval_only){
    return(eval_table)
  }

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
    ggtitle(paste0("Method = 'Exponential'", " & day of the week = ", week_period))+
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


#' Set the change point and identify the optimal method to model the data prior to the change point
#' @title set_change_point
#' @param data A dataset of visit counts, output by count_prior_events_truven
#' @param var_name The name of the count variable to find the change-point for
#' @param method The method used to find changepoint. Options include "linear", "quadratic", "cubic", "quartic",
#' "quintic", "sextic" , or "exponential"
#' @param eval_criteria The evaluation criteria used to compare models
#' @param week_period Logical to incorporate a "day of the week" effect into the momdel
#' Note this is only sensible for one-day period aggregation
#' @param return_miss_only Logical argument to only return the tibbles of miss visit counts
#' @param specify_cp A postive integer value for the specific change point you want to use. The value represents
#' the days before the index on which you you want to specify the change point. (e.g. 100 would be 100 days before the index)
#' @param compare_all_methods A logical to compare all available. If TRUE, will return a tibble of methods by evaluation criteria
#' @return A list containing tibbles of information about missed visits
#'
#' @examples
#'
#' results <- final_time_map %>%
#' count_prior_events_truven(event_name = "any_ssd", start_day = 1, by_days = 1) %>%
#' set_change_point(var_name = "n_miss_visits", method = "cubic", specify_cp = 21)
#'
#' @export
set_change_point <- function(data, var_name="n_miss_visits", method = NULL, compare_all_methods = FALSE,
                              return_miss_only = FALSE, week_period = FALSE, specify_cp = NULL){

  #Require some necessary packages
  require(tidyverse)

  # error if no change point specified
  if(is.null(specify_cp)){
    stop(paste0("Please specify a postive integer value that represents the days before the",
                " index on which you you want to specify the change point. (e.g. 100 would be 100 days before the index"))
  }


  if (compare_all_methods == TRUE){
   all_combs <-  tibble(method = c("linear", "quadratic", "cubic", "quartic", "quintic", "sextic" ,"exponential")) %>%
      mutate(week_period = map(method, ~c(TRUE, FALSE))) %>% unnest(week_period)

   poly_all <- all_combs %>% filter(method != "exponential") %>% mutate(out = map2(method, week_period,
                                                                                   ~set_cp_poly(data = data,
                                                                                                var_name = var_name,
                                                                                                poly_order = .x,
                                                                                                return_eval_only = TRUE,
                                                                                                week_period = .y,
                                                                                                specify_cp = specify_cp))) %>%
     unnest(out)

   expo_only <- all_combs %>% filter(method == "exponential") %>% mutate(out = map2(method, week_period,
                                                                                   ~set_cp_exp(data = data,
                                                                                                var_name = var_name,
                                                                                                return_eval_only = TRUE,
                                                                                                week_period = .y,
                                                                                                specify_cp = specify_cp))) %>%
     unnest(out)

   all_eval <- bind_rows(poly_all, expo_only) %>% select(-model) %>% arrange(AIC)
   return(all_eval)
  } else {

    # error if method misspecified
    if (is.null(method) || (!method %in% c("linear", "quadratic", "cubic", "quartic", "quintic", "sextic" ,"exponential"))){
      stop("The method input is not available. Please select from one of the following:
         'linear', 'quadratic', 'cubic', 'quartic', 'quintic', 'sextic', or 'exponential'")
    }

    if (method == "exponential"){
      out <- set_cp_exp(data = data,
                        var_name = var_name,
                        return_eval_only = FALSE,
                        return_miss_only = return_miss_only,
                        week_period = week_period,
                        specify_cp = specify_cp)
    } else {
      out <- set_cp_poly(data = data,
                         var_name = var_name,
                         poly_order = method,
                         return_eval_only = FALSE,
                         return_miss_only = return_miss_only,
                         week_period = week_period,
                         specify_cp = specify_cp)
    }
  }
  return(out)
}



