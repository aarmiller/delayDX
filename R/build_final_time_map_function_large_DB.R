#' Builds a time map containing visit specific information for a specific condition and cohort for large databases.
#' @name build_final_time_map_large_DB
#' @param condition_short_name The condition of interest. Specifically the short_name version of the condition
#' @param duration_prior_to_index How far from the index should the time map extend back (e.g. 90, 180, 365)
#' @param time_map The time_map to which the indicators will be added to
#' @param cohort_path Path to the specific cohort for the condition of interest
#' @param ssd_list A list of diagnosis codes of interest. The diagnosis codes need to be seperated into
#' diagnosis categories (e.g. cough, fever, etc.). Additionally, within the categories, diagnosis codes should be seperated into ICD 9 and
#' ICD 10 specific codes, with list elements labled as icd9_codes and icd10_codes
#' @return A tibble with all the specified diagnosis codes and the corresponding visit key where the diagnosis codes appeared
#' @examples
#' ssds <- read_rds("/Shared/Statepi_Diagnosis/params/ssd_codes/ssd_hsv_enceph.RDS")
#'
#' final_time_map <- build_final_time_map_large_DB(condition_short_name = "hsv_enceph",
#'                                                 duration_prior_to_index = 365L,
#'                                                 time_map = time_map,
#'                                                 cohort_path = "/Shared/Statepi_Diagnosis/grant_projects/hsv_enceph/scripts/validation/enrolled_ge_365/updated_index_cohort.RDS",
#'                                                 ssd_list = ssds)
#' @export
#'

build_final_time_map_large_DB <- function (condition_short_name, duration_prior_to_index = 365L, time_map,
                                  cohort_path, ssd_list){

    #update all_dx to include dx date instead of time to dx (this is necessary as index dates are shifted do time to dx needs to be recalculated)
    all_dx <- all_dx  %>%
    inner_join(index_data %>% select(enrolid, index_date)) %>%
    mutate(visit_date = index_date + days_since_index) %>%
    select(-days_since_index, - index_date)

    # Get subset of enrolids that will be used in the analysis
    final_cohort <- read_rds(cohort_path)

    index_data <- final_cohort %>%
      mutate(time_before_index = index_date - first_enroll) %>%
      select(enrolid, index_date , enroll_first = first_enroll, time_before_index, first_dx_source = index_dx_source, stdplac, key)

    #update all_dx to include adjusted days_since_index using new index dates also select for enrolids in cohort
    all_dx <- all_dx  %>%
      inner_join(index_data %>% select(enrolid, index_date)) %>%
      mutate(days_since_index = visit_date - index_date) %>%
      select(-visit_date, - index_date)

    # select for enroilid that were enrolled >=duration_prior_to_index days prior to index
    enrolled_ge_1year <- index_data %>% filter(time_before_index >= duration_prior_to_index) %>% .$enrolid %>% unique()
    index_data <- index_data %>% filter(time_before_index >= duration_prior_to_index)
    dx_data <- all_dx %>% filter(enrolid %in% enrolled_ge_1year) %>% rename(days_since_dx=days_since_index) %>%
      filter(between(days_since_dx,-duration_prior_to_index,0))
    rx_data <- all_rx %>% inner_join(index_data %>% select(enrolid, index_date)) %>%
      mutate(days_since_rx = rx_date - index_date) %>%
      filter(between(days_since_rx,-duration_prior_to_index,0)) %>% select(enrolid, rx, days_since_rx)

    # split the dataset
    enrolid_table <- index_data$enrolid %>% unique()
    n <- 900000L
    split_enrolid <- split(enrolid_table, ceiling(seq_along(enrolid_table)/n))

    time_map_temp <- tibble()
    for (i in 1:length(split_enrolid)){

      # subset timemap to cases of interest
      temp1 <- time_map %>% filter(enrolid %in% split_enrolid[[i]]) %>%
               inner_join(index_data %>% filter(enrolid %in% split_enrolid[[i]]) %>%
                                            distinct(enrolid, index_date))
      # add first_dx visit
      temp1 <-  temp1 %>% left_join(index_data %>% filter(enrolid %in% split_enrolid[[i]]) %>%
                                            select(enrolid, admdate = index_date, source = first_dx_source, stdplac, key) %>%
                                            mutate(first_dx = 1)) %>%
        mutate(first_dx = ifelse(is.na(first_dx), 0, first_dx))

      # create inpatient, ED, and outpatient indicators
      temp1 <- temp1 %>% filter(!is.na(source)) %>%
        mutate(inpatient = ifelse(source == "inpatient", 1L, 0L)) %>%
        mutate(outpatient = ifelse(source == "outpatient", 1L, 0L)) %>%
        mutate(ED = ifelse(source == "ED", 1L, 0L)) %>%
        select(-source)

      data_temp <- temp1 %>%
        group_by(enrolid) %>%
        arrange(enrolid, admdate) %>%
        mutate(days_since_dx= admdate - index_date) %>%
        mutate(visit_no=row_number()) %>%
        ungroup()

      time_map_temp <- time_map_temp %>% bind_rows(data_temp)

    }

    time_map <- time_map_temp

    #Get SSD keys
    ssd_inds <- build_dx_indicators_delay_large_DB(condition_dx_list = ssd_list, db_path = db_path,
                                       db_con = db_con) %>%
      rename(any_ssd=any_ind)

    # Make final timemap with condition indicators
    # extract names of the condition indicators
    ind_names <- ssd_inds %>%
      select(-key) %>%
      names()

    # add indicators to time_map
    for (i in 1:length(ind_names)) {

     keys_ssd_cat <- ssd_inds %>% filter(ssd_inds[ind_names[i]] == 1) %>% .$key %>%  unique()
     time_map[ind_names[i]] <- ifelse(time_map$key %in% keys_ssd_cat, 1L, 0L)

    }

    # filter timemap to period of interest before diagnosis
    time_map <- time_map %>%
      filter(between(days_since_dx,-duration_prior_to_index,0))

    # get distinct visit (distinct enrolid, days_since_dx, std_place). If on a given "distinct" visit, if it was labeled
    # as both and outpatient and ED, it should be labeled as an ED visit and not an outpatient visit.
      vars_to_summarise <- c("inpatient", "ED", "first_dx", ind_names)

      final_time_map <- tibble()
      for (i in 1:length(split_enrolid)){

        data_temp <- time_map %>% filter(enrolid %in% split_enrolid[[i]]) %>%
          group_by(enrolid, days_since_dx, stdplac) %>%
          summarise_at(vars(vars_to_summarise),.funs = list(~max(.))) %>%
          ungroup() %>%
          mutate(outpatient = ifelse(inpatient == 0 & ED == 0 , 1, 0)) %>%
          mutate(all_visits = 1)

        final_time_map <- final_time_map %>% bind_rows(data_temp)

      }

  return(list(final_time_map = final_time_map,
              index_data = index_data,
              dx_data = dx_data,
              rx_data = rx_data))
}



