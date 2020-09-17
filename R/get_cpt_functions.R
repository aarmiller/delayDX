
#' Return outpatient core information for a specific list of cpt codes
#' @name gether_proc_data_outpatient
#' @param table_name The specific outpatient core table to access
#' @param cpt_codes The cpt codes of interest
#' @param proc_vars List of variables to return
#' @param db_path Path to the database
#' @param collect_n The number of observations to return. Default in Inf
#' @return A tibble with cpt code of interest for the specified list of variables
#' @export

gether_proc_data_outpatient <- function(table_name, cpt_codes, proc_vars  =c(), db_path, collect_n = Inf) {
  db_con <- src_sqlite(db_path)
  tbl_name <- table_name
  get_vars <- dplyr::tbl(db_con, tbl_name) %>% dplyr::tbl_vars()
  if (is.null(proc_vars)) {
    get_vars <- get_vars
  }
  else {
    get_vars <- proc_vars[proc_vars %in% get_vars]
  }
  out <- db_con %>% dplyr::tbl(tbl_name) %>% dplyr::filter(.data$proc1 %in%
                                                             cpt_codes) %>%
    dplyr::select(get_vars) %>%
    dplyr::collect(n = collect_n)
  return(out)
}

#' Return inpatient core information for a specific list of cpt codes
#' @name gether_proc_data_inpatient
#' @param table_name The specific inpatient  core table to access
#' @param cpt_codes The cpt codes of interest
#' @param proc_vars List of variables to return
#' @param db_path Path to the database
#' @param collect_n The number of observations to return. Default in Inf
#' @return A tibble with cpt code of interest for the specified list of variables
#' @export

gether_proc_data_inpatient <- function(table_name, cpt_codes, proc_vars = c(), db_path, collect_n = Inf) {
  db_con <- src_sqlite(db_path)

  split_table_name <- stringr::str_split(table_name, "_")[[1]]
  tbl_name1 <- paste0(split_table_name[1], "_proc_", split_table_name[3], "_", split_table_name[4])
  out1 <- db_con %>% dplyr::tbl(tbl_name1) %>% dplyr::filter(.data$proc %in% cpt_codes) %>%
    dplyr::collect(n = collect_n)

  tbl_name <- table_name
  get_vars <- dplyr::tbl(db_con, tbl_name) %>% dplyr::tbl_vars()

  if (is.null(proc_vars)) {
    get_vars <- get_vars
  }
  else {
    get_vars <- proc_vars[proc_vars %in% get_vars]
  }
  out <- db_con %>% dplyr::tbl(tbl_name) %>%
    dplyr::select(c("caseid",get_vars)) %>%
    dplyr::collect(n = collect_n)

  out <- out %>% inner_join(out1) %>% rename(proc1=proc) %>% select(-caseid, -proc_num)
  return(out)
}

#' Return inpatient and outpatient core information for a specific list of cpt codes over multiple core tables (in parallel)
#' @name gather_proc_data
#' @param collect_tab A tibble with the specific setting (i.e. inpatient or outpatient), source (i.e. ccae or mdcr), and year to access.
#' Default is all possible combinations of setting, source, and year
#' @param cpt_codes The cpt codes of interest
#' @param proc_vars List of variables to return
#' @param db_path Path to the database
#' @param collect_n The number of observations to return
#' @param num_cores The number of worker cores to use. If not specified will determined the number of cores based on the which ever
#' is the smallest value between number of rows in for collect_tab or detected number of cores - 1
#' @return A tibble with cpt code of interest for the specified list of variables
#' @export
gather_proc_data <- function (collect_tab = collect_table(), cpt_codes,
                              proc_vars=c("enrolid", "proc1", "svcdate", "admdate", "proc","disdate"),
                              db_path, collect_n = Inf, num_cores = NULL) {

  tab <- collect_tab %>% mutate(table = paste0(setting, "_core_", source, "_", year))
  out_tab <- tab %>% filter(setting == "outpatient")
  in_tab <- tab %>% filter(setting == "inpatient")

  db_path2 <- db_path
  cpt_codes2 <- cpt_codes
  proc_vars2 <- proc_vars
  collect_n2 <- collect_n

  # set up clusters
  if (is.null(num_cores)) {
    num_cores <- min(nrow(tab), parallel::detectCores() - 1)
  } else {
    num_cores <- num_cores
  }

  cluster <- parallel::makeCluster(num_cores)
  parallel::clusterExport(cluster, varlist = c("gether_proc_data_outpatient",
                                               "gether_proc_data_inpatient"))

  parallel::clusterCall(cluster, function() library(tidyverse))
  parallel::clusterCall(cluster, function() library(dplyr))

  tmp_out <- parallel::parLapply(cl = cluster,
                             1:length(out_tab$table),
                             function(x){gether_proc_data_outpatient(table_name = out_tab$table[x],
                                                                  cpt_codes = cpt_codes2,
                                                                  proc_vars = proc_vars2,
                                                                  db_path = db_path2,
                                                                  collect_n = collect_n2)})

  tmp_in <- parallel::parLapply(cl = cluster,
                                 1:length(in_tab$table),
                                 function(x){gether_proc_data_inpatient(table_name = in_tab$table[x],
                                                                      cpt_codes = cpt_codes2,
                                                                      proc_vars = proc_vars2,
                                                                      db_path = db_path2,
                                                                      collect_n = collect_n2)})
  parallel::stopCluster(cluster)
  gc()


  out_outpatient <- tibble()
  for (i in 1:length(tmp_out)){
    x <- tmp_out[[i]] %>% nest(cpt_data = everything())
    out_outpatient <- bind_rows(out_outpatient, x)
  }

  outpatient <-  bind_cols(out_tab %>% select(-table), out_outpatient)

  out_inpatient <- tibble()
  for (i in 1:length(tmp_in)){
    x <- tmp_in[[i]] %>% nest(cpt_data = everything())
    out_inpatient <- bind_rows(out_inpatient, x)
  }
  inpatient <-  bind_cols(in_tab %>% select(-table), out_inpatient)

  out <- bind_rows(outpatient, inpatient)
  return(out)
}
