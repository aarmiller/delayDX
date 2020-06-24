###########################################
#These functions either ready data to be
#used for fitting delay and incidence models,
#or actually fit those models.
###########################################


#This function outputs a dataset that can easily be used to
#make an incidence model labeling a miss as 1, and the index
#as 0. It takes as arguments:
#time_map: a formatted time map
#db_con: a connection to a small, cond-specific database
#change_point: the change_point to use when segmenting
#days_before: number of days before index to label as a miss


#Roxygen comments

#' Prepare a time map for incidence modeling
#'
#' @param time_map A time map in the format of the package example
#' @param db_con A connection to a small, condition-specific database
#' @param change_point The changepoint on which to base the dataset (must be <=-1)
#' @param days_before Number of days before index to start classifying as a miss
#' @return A dataframe containing the outcome "miss" and covariates for each row
#' @examples
#' get_incidence_data(time_map, db_con, change_point=-90, days_before= -1)
get_incidence_data <- function(time_map, db_con, change_point, days_before = -1){

  #Require certain packages
  #require(tidyverse)
  #require(delayDX)
  #require(lubridate)

  #Define a function from smallDB for convenience, so we don't need to
  #load the entire package
  collect_table <- function(settings = c("inpatient", "outpatient"),
            sources = c("ccae", "mdcr"), years = NULL)
  {
    if (is.null(years)) {
      years <- stringr::str_pad(c(1:17), 2, pad = "0")
    }
    tibble::tibble(setting = settings) %>% dplyr::mutate(source = purrr::map(.data$setting,
                                                                             ~sources)) %>% tidyr::unnest() %>% dplyr::mutate(year = purrr::map(source,
                                                                                                                                                ~years)) %>% tidyr::unnest()
  }



  #Extract unique enrolids
  enrolids <- unique(time_map$enrolid)

  #Define helper functions to aid in extracting demographic information

  #Get demographic data
  gether_enroll_data <- function (collect_tab = collect_table(), enrolid_list, vars = c("dobyr",
                                                                                        "sex",
                                                                                        "emprel"), db_con)
  {
    temp <- collect_table() %>% dplyr::select(-.data$setting) %>%
      dplyr::mutate(data = purrr::map2(.data$source, .data$year,
                                       ~dplyr::tbl(db_con, paste0("enrollees_", .x, "_",
                                       .y)) %>% dplyr::filter(enrolid %in% enrolid_list) %>%
                                       dplyr::select(c("enrolid", vars)) %>%
                                       dplyr::collect() %>% dplyr::mutate(enrolid = as.integer(enrolid))))
    out <- temp %>% dplyr::select(.data$data) %>% tidyr::unnest() %>%
      dplyr::distinct()
    return(out)
  }
  #Get location data
  collapse_enrollment <- function (enrolid_list, db_con, vars = c("egeoloc", "msa", "plantyp" ,"indstry"),
                                   collect_tab = collect_table())
  {
    temp <- collect_tab %>% dplyr::select(-.data$setting) %>%
      dplyr::mutate(data = purrr::map2(.data$source, .data$year,
      ~dplyr::tbl(db_con, paste0("enrollment_detail_",
      .x, "_", .y)) %>% dplyr::filter(.data$enrolid %in%
      enrolid_list) %>% dplyr::select(c("enrolid",
      "dtstart", "dtend", vars)) %>% dplyr::collect() %>%
      dplyr::mutate(enrolid = as.integer(.data$enrolid))))

    temp <- temp %>% dplyr::select(.data$data) %>% dplyr::mutate(data = purrr::map(.data$data,
      ~dplyr::mutate_at(., .vars = dplyr::vars(vars), .funs = list(as.integer)))) %>%
      tidyr::unnest()

    temp_strata <- temp %>% dplyr::select(c("enrolid", vars)) %>%
      dplyr::distinct() %>% dplyr::mutate(strata = dplyr::row_number())
    temp <- temp %>% dplyr::inner_join(temp_strata, by = c("enrolid",vars))

    out <- temp %>% dplyr::arrange(.data$enrolid, .data$dtstart) %>%
      dplyr::group_by(.data$enrolid) %>% dplyr::mutate(gap = ((.data$dtstart -
      dplyr::lag(.data$dtend)) > 1) | .data$strata != dplyr::lag(.data$strata),
      gap = ifelse(is.na(.data$gap), FALSE, .data$gap)) %>%
      dplyr::mutate(period = cumsum(.data$gap)) %>% dplyr::group_by_at(c("enrolid",
      "period", vars)) %>% dplyr::summarise(dtstart = min(.data$dtstart),
      dtend = max(.data$dtend)) %>% dplyr::ungroup()

    return(out)
  }

  #Call these functions
  enroll_collapsed <- collapse_enrollment(enrolid_list = enrolids,
                                          vars = c("egeoloc", "msa", "plantyp" ,"indstry"),
                                          db_con = db_con)
  demo_data <- gether_enroll_data(enrolid_list = enrolids,
                                  vars = c("dobyr","sex","emprel"),
                                  db_con = db_con)

  #Join this data to the time map and format
  #demographic data, extract age
  demo_time_map <- time_map %>% inner_join(demo_data, by='enrolid') %>%
    mutate(year=year(as_date(admdate))) %>%
    mutate(age=year-dobyr)

  #Location data, must make sure right enrollment period is used
  demo_time_map <- demo_time_map %>% inner_join(enroll_collapsed, by ='enrolid') %>%
    filter(admdate >= dtstart & admdate <= dtend) %>%
    select(-c(dtstart,dtend,period,visit_no))

  #Try to clean up a little bit, and reformat
  demo_time_map <- demo_time_map %>%
    mutate(age_cat=cut(age,breaks = c(-1,17,35,45,55,65,130))) %>%
    mutate_at(vars(sex),funs(as.factor)) %>%
    mutate(year = year(as_date(admdate))) %>%
    mutate(emprel=as.factor(emprel)) %>%
    mutate(day=as.numeric(as_date(admdate)),
           year.factor=as.factor(year(as_date(admdate))),
           year=year(as_date(admdate)),
           month.factor=as.factor(month(as_date(admdate))),
           month=month(as_date(admdate)))

  demo_time_map <- demo_time_map %>%
    mutate(plan = factor(plantyp,
                  levels = c(0, 2:9),
                  labels = c("Potentially Basic/major medical (0 in data, no 1",
                             "Comprehensive",
                             "Exclusive Provider",
                             "Health Maintenance ",
                             "NonCapitatedPointofService",
                             "Preferred Provider",
                             "Capitated/Partially",
                             "Consumer-DrivenPlan",
                             "High DeductiblePlan"))) %>%
    mutate(indus = factor(indstry,
                          levels = 1:7,
                          labels = c("Oil & Gas Extraction, Mining",
                                     "Manufacturing,DurGoods",
                                     "Manufacturing,NondurGoods",
                                     "Transportation,Comms,Util",
                                     "RetailTrade",
                                     "Finance,Insurance,RealEstate",
                                     "Services")))

  #get misses and non miss visits, for outcome we are modeling
  miss_cases <- demo_time_map %>%
    filter(any_ssd == 1,
           between(days_since_dx, change_point, days_before)) %>%
    mutate(miss=1L)

  non_miss_cases <- demo_time_map %>%
    filter(days_since_dx == 0) %>%
    mutate(miss=0L)

  final_model_data <- bind_rows(miss_cases,non_miss_cases)

  return(final_model_data)

}



