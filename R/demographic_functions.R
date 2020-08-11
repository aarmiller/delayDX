
#' Get enrollment data from one specific multiple "enrollees" table
#'
#' @name get_enroll_data
#' @param table A tibble with a single row containing the specific source (i.e. ccae or mdcr) and year to access
#' @param enrolid_list A list of enrolids for which enrollment data will be collected
#' @param db_path Path to the database
#' @param vars Vector of specific variables of interest in the "enrollees" tables (e.g. c("dobyr", "sex")) 
#' @param collect_n The number of observations to return
#' 
#' @return A tibble with information on each enrolid in enrolid_list. The number of columns in the tibble depends on
#' the number of vars selected
#' 
#' @export
#' 
get_enroll_data <- function (table, enrolid_list, collect_n = Inf,
                                vars = c("dobyr", "sex"), db_path) {
  db_con <- src_sqlite(db_path)
   out <- dplyr::tbl(db_con, paste0("enrollees_", table$source, "_",  table$year)) %>%
           dplyr::filter(enrolid %in% enrolid_list) %>% 
           dplyr::select(c("enrolid", vars)) %>% 
           dplyr::collect(n = collect_n) %>% dplyr::mutate(enrolid = as.integer(enrolid)) %>% 
           dplyr::distinct()
   
   return(out)
}



#' Get enrollment data over multiple "enrollees" tables (in parallel)
#'
#' @name gather_enroll_data
#' @param collect_tab A tibble with the specific setting (i.e. inpatient or outpatient), source (i.e. ccae or mdcr), and year to access
#' Default is all possible combinations of setting, source, and year
#' @param enrolid_list A list of enrolids for which enrollment data will be collected
#' @param db_path Path to the database
#' @param vars Vector of specific variables of interest in the "enrollees" tables (e.g. c("dobyr", "sex")) 
#' @param collect_n The number of observations to return
#' @param num_cores The number of worker cores to use. If not specified will determined the number of cores based on the which ever
#' is the smallest value between number of rows in for collect_tab or detected number of cores - 1
#' 
#' @return A tibble with information on each enrolid in enrolid_list. The number of columns in the tibble depends on
#' the number of vars selected
#' 
#' @examples
#' 
#' out <- gather_enroll_data(enrolid_list = final_cohort$enrolid, db_path = db_path, vars = c("dobyr", "sex"))
#' 
#' @export

gather_enroll_data <- function (collect_tab = collect_table(), enrolid_list, collect_n = Inf,
                                vars = c("dobyr", "sex"), db_path, num_cores = NULL) {
  
  # require some pacakges
  require(tidyverse)
  require(dplyr)

  db_path2 <- db_path
  vars2 <- vars
  collect_n2 <- collect_n
  enrolid_list2 <- enrolid_list
  
  temp <- collect_tab %>% dplyr::select(-.data$setting)
  
  # set up clusters
  if (is.null(num_cores)) {
    num_cores <- min(nrow(temp), parallel::detectCores() - 1)
  } else {
    num_cores <- num_cores
  }
  
  cluster <- parallel::makeCluster(num_cores)
  parallel::clusterExport(cluster, varlist = c("get_enroll_data"))
  
  parallel::clusterCall(cluster, function() library(tidyverse))
  parallel::clusterCall(cluster, function() library(dplyr))
  
  tmp <- parallel::parLapply(cl = cluster,
                             1:nrow(temp),
                             function(x){get_enroll_data(table = temp[x, ],
                                                         enrolid_list = enrolid_list2,
                                                         vars = vars2,
                                                         db_path = db_path2,
                                                         collect_n = collect_n2)})
  
  parallel::stopCluster(cluster)
  gc()
  
  out <- tibble()
  for (i in 1:length(tmp)){
    x <- tmp[[i]] %>% nest(data = everything())
    out <- bind_rows(out, x)
  }
  out <- out %>% select(data) %>% unnest() %>% distinct()
  
  return(out)
}

#' Get collapsed enrollment data from one specific multiple "enrollment_detail" table
#'
#' @name get_collapse_enrollment
#' @param table A tibble with a single row containing the specific source (i.e. ccae or mdcr) and year to access
#' @param enrolid_list A list of enrolids for which enrollment data will be collected
#' @param db_path Path to the database
#' @param vars Vector of specific variables of interest in the "enrollees" tables (e.g. c("egeoloc", "msa", "plantyp" ,"indstry")) 
#' @param collect_n The number of observations to return
#' 
#' @return A tibble with information on each enrolid in enrolid_list. The number of columns in the tibble depends on
#' the number of vars selected
#' 

#' @export
#' 

get_collapse_enrollment <- function (table, enrolid_list, collect_n = Inf,
                             vars = c("egeoloc", "msa", "plantyp" ,"indstry"), db_path) {
  db_con <- src_sqlite(db_path)
  
  temp <- dplyr::tbl(db_con, paste0("enrollment_detail_", table$source, "_",  table$year)) %>%
    dplyr::filter(enrolid %in% enrolid_list) %>% 
    dplyr::select(c("enrolid", "dtstart", "dtend", vars)) %>% 
    dplyr::collect(n = collect_n) %>% 
    dplyr::mutate(enrolid = as.integer(.data$enrolid))

  out <- temp %>% dplyr::mutate_at(.vars = dplyr::vars(vars), .funs = list(as.integer))
  
  return(out)
}


#' Get collapsed enrollment data over multiple "enrollment_detail" tables (in parallel)
#'
#' @name gather_collapse_enrollment
#' @param collect_tab A tibble with the specific setting (i.e. inpatient or outpatient), source (i.e. ccae or mdcr), and year to access
#' Default is all possible combinations of setting, source, and year
#' @param enrolid_list A list of enrolids for which enrollment data will be collected
#' @param db_path Path to the database
#' @param vars Vector of specific variables of interest in the "enrollees" tables (e.g. c("dobyr", "sex")) 
#' @param collect_n The number of observations to return
#' @param num_cores The number of worker cores to use. If not specified will determined the number of cores based on the which ever
#' is the smallest value between number of rows in for collect_tab or detected number of cores - 1
#' 
#' @return A tibble with information on each enrolid in enrolid_list. The number of columns in the tibble depends on
#' the number of vars selected
#' 
#' @examples
#' 
#' out <- gather_collapse_enrollment(enrolid_list = final_cohort$enrolid, db_path = db_path, vars = c("egeoloc", "msa", "plantyp" ,"indstry"))
#' 
#' @export

gather_collapse_enrollment <- function (collect_tab = collect_table(), enrolid_list, collect_n = Inf,
                                 vars = c("egeoloc", "msa", "plantyp" ,"indstry"), 
                                 db_path, num_cores = NULL) {
  # require some pacakges
  require(tidyverse)
  require(dplyr)
  
  db_path2 <- db_path
  vars2 <- vars
  collect_n2 <- collect_n
  enrolid_list2 <- enrolid_list
  
  temp <- collect_tab %>% dplyr::select(-.data$setting)
  
  # set up clusters
  if (is.null(num_cores)) {
    num_cores <- min(nrow(temp), parallel::detectCores() - 1)
  } else {
    num_cores <- num_cores
  }
  
  cluster <- parallel::makeCluster(num_cores)
  parallel::clusterExport(cluster, varlist = c("get_collapse_enrollment"))
  
  parallel::clusterCall(cluster, function() library(tidyverse))
  parallel::clusterCall(cluster, function() library(dplyr))
  
  tmp <- parallel::parLapply(cl = cluster,
                             1:nrow(temp),
                             function(x){get_collapse_enrollment(table = temp[x, ],
                                                                 enrolid_list = enrolid_list2,
                                                                 vars = vars2,
                                                                 db_path = db_path2,
                                                                 collect_n = collect_n2)})
  
  parallel::stopCluster(cluster)
  gc()
  
  out <- tibble()
  for (i in 1:length(tmp)){
    x <- tmp[[i]] %>% nest(data = everything())
    out <- bind_rows(out, x)
  }
  temp <- out %>% select(data) %>% unnest() 
  
  temp_strata <- temp  %>% dplyr::select(c("enrolid", vars)) %>% 
    dplyr::distinct() %>% dplyr::mutate(strata = dplyr::row_number())
  
  temp <- temp %>% dplyr::inner_join(temp_strata, by = c("enrolid", vars))
  
  out <- temp %>% dplyr::arrange(.data$enrolid, .data$dtstart) %>% 
    dplyr::group_by(.data$enrolid) %>% 
    dplyr::mutate(gap =((.data$dtstart - dplyr::lag(.data$dtend)) > 1) | .data$strata != dplyr::lag(.data$strata), 
                  gap = ifelse(is.na(.data$gap), FALSE, .data$gap)) %>% 
    dplyr::mutate(period = cumsum(.data$gap)) %>% 
    dplyr::group_by_at(c("enrolid", "period", vars)) %>%
    dplyr::summarise(dtstart = min(.data$dtstart), 
                     dtend = max(.data$dtend)) %>% 
    dplyr::ungroup()
  return(out)
}
