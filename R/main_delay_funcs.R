

#' Count prior visits assocatied with a given condition
#'
#' @importFrom rlang .data
#'
#' @param time_map_data A timemap with indicators for specific events (i.e.
#' diagnoses during a given visit)
#' @param event_name The variable name for the even indicator
#' @param start_day When to start counting prior to the index
#' @param by_days The number of days in each bin to count by
#'
#' @export
count_prior_events_truven <- function(time_map_data,event_name,start_day=1L,by_days=1L){

  tmp <- time_map_data %>%
    dplyr::rename(miss_ind=!!event_name) %>%
    dplyr::filter(.data$days_since_dx<=-start_day) %>%
    dplyr::arrange(-.data$days_since_dx) %>%
    dplyr::mutate(period=by_days*((-.data$days_since_dx-start_day) %/% by_days)) %>%
    dplyr::group_by(.data$period)

  tmp1 <- tmp %>%
    dplyr::summarise(n_visits=dplyr::n(),
                     n_patients=dplyr::n_distinct(.data$enrolid))

  tmp2 <- tmp %>%
    filter(miss_ind==1) %>%
    dplyr::summarise(n_miss_visits=n(),
                     n_miss_patients=dplyr::n_distinct(.data$enrolid))

  dplyr::inner_join(tmp1,tmp2,by="period")
}

#' Find the change point in count data
#'
#' @param data A dataset of visit counts
#' @param var_name The name of the count variable to find the change-point for
#' @param method The method used to fit curves before and after the changepoint. Options include "lm",
#' "lm_quad", "lm_cube", "quad", "cube", "exp", "spline"
#' @param eval_criteria The evaluation criteria used to find change points
#' @param return_miss_only Logical argument to only return the tibbles of miss visit counts
#'
#' @export
find_change_point <- function(data,var_name,method="lm",eval_criteria="AIC", return_miss_only=FALSE){

  tmp_var_name <- rlang::enquo(var_name)

  data <- data %>%
    dplyr::arrange(dplyr::desc(period)) %>%
    dplyr::mutate(Y=!!tmp_var_name,
                  t=dplyr::row_number())

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

  cp_plot <- out$pred %>%
    ggplot2::ggplot(aes(t,pred)) +
    ggplot2::geom_line(aes(y=pred1),color="red") +
    ggplot2::geom_line() +
    ggplot2::geom_point(aes(t,Y)) +
    ggplot2::geom_ribbon(aes(x=t,ymin = pred_low, ymax = pred_high),
                         fill = "steelblue2", alpha = 0.3, inherit.aes = FALSE) +
    ggplot2::theme_light()

  change_point <- data %>%
    dplyr::filter(t==change_t) %>%
    dplyr::select(Y,t,period)

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



