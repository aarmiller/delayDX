
#' Gather enrollment details from a single enrollment_detail table from the database
#' @name get_collapsed_enrollment_delay
#' @param table_name The specific enrollment_detail table name in the database to access
#' @param enrolid_list A vector of enrolids to collect enrollment info
#' @param vars A character vector of variables it the core table to return. If no value is provided, enrollment will simply be collapsed into continuous enrollment periods
#' @param db_path Path to the database
#' @param collect_n The number of observations to return
#' @return A tibble with all the specified variables to return for one specific enrollment_detail table
#' @export
#'

get_collapsed_enrollment_delay <- function (table_name, enrolid_list, vars = c(), db_path, collect_n = Inf){

  tbl_name <- table_name
  db_con <- src_sqlite(db_path)

  temp <- tbl(db_con, tbl_name) %>%
          dplyr::filter(.data$enrolid %in% enrolid_list) %>%
          dplyr::select(c("enrolid", "dtstart", "dtend", vars)) %>% dplyr::collect(n = collect_n) %>%
          dplyr::mutate_at(., .vars = dplyr::vars(vars), .funs = list(as.integer))

  return(temp)
}

#' Gather enrollment details information over multiple enrollment_detail tables (in parallel)
#' @name collapse_enrollment_delay
#' @param collect_tab A tibble with the specific setting (i.e. inpatient or outpatient), source (i.e. ccae or mdcr), and year to access.
#' Default is all possible combinations of setting, source, and year
#' @param enrolid_list A vector of enrolids to collect enrollment info
#' @param vars A character vector of variables it the core table to return. If no value is provided, enrollment will simply be collapsed into continuous enrollment periods
#' @param db_path Path to the database
#' @param collect_n The number of observations to return
#' @param num_cores The number of worker cores to use. If not specified will determined the number of cores based on the which ever
#' is the smallest value between number of rows in for collect_tab or detected number of cores - 1
#' @return A tibble with all the specified variables to return over all designated enrollment_detail tables
#'
#' @examples
#' test <- collapse_enrollment_delay(enrolid_list = enrolids$enrolid,
#'                                   db_path = db_path)
#'
#' @export
#'

collapse_enrollment_delay <- function (collect_tab = collect_table(), enrolid_list, vars = c(), db_path,
                                       collect_n = Inf, num_cores = NULL) {

  tab <- collect_tab %>% select(-.data$setting) %>%
    mutate(table = paste0("enrollment_detail_", .data$source, "_", year))

  db_path2 <- db_path
  vars2 <- vars
  collect_n2 <- collect_n
  enrolid_list2 <- enrolid_list

  # set up clusters
  if (is.null(num_cores)) {
    num_cores <- min(nrow(tab), parallel::detectCores() - 1)
  } else {
    num_cores <- num_cores
  }

  cluster <- parallel::makeCluster(num_cores)
  parallel::clusterExport(cluster, varlist = c("get_collapsed_enrollment_delay"))

  parallel::clusterCall(cluster, function() library(tidyverse))
  parallel::clusterCall(cluster, function() library(dplyr))


  tmp <- parallel::parLapply(cl = cluster,
                             1:length(tab$table),
                             function(x){get_collapsed_enrollment_delay(table_name = tab$table[x],
                                                                        enrolid_list = enrolid_list2,
                                                                        vars = vars2,
                                                                        db_path = db_path2,
                                                                        collect_n = collect_n2)})
  parallel::stopCluster(cluster)
  gc()

  out <- tibble()
  for (i in 1:length(tmp)){
    x <- tmp[[i]]
    out <- bind_rows(out, x)
  }

  temp_strata <- out %>% dplyr::select(c("enrolid", vars)) %>%
                 dplyr::distinct() %>%
                 dplyr::mutate(strata = dplyr::row_number())

  temp <- out %>% dplyr::inner_join(temp_strata, by = c("enrolid", vars))

  out1 <- temp %>% dplyr::arrange(.data$enrolid, .data$dtstart) %>%
          dplyr::group_by(.data$enrolid) %>%
    dplyr::mutate(gap = ((.data$dtstart - dplyr::lag(.data$dtend)) > 1) | .data$strata != dplyr::lag(.data$strata),
                  gap = ifelse(is.na(.data$gap), FALSE, .data$gap)) %>%
    dplyr::mutate(period = cumsum(.data$gap)) %>% dplyr::group_by_at(c("enrolid", "period", vars)) %>%
    dplyr::summarise(dtstart = min(.data$dtstart), dtend = max(.data$dtend)) %>%
    dplyr::ungroup()

  return(out1)
}


