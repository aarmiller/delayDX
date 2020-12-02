

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
    dplyr::filter(miss_ind==1) %>%
    dplyr::summarise(n_miss_visits=dplyr::n(),
                     n_miss_patients=dplyr::n_distinct(.data$enrolid))

  dplyr::inner_join(tmp1,tmp2,by="period")
}





