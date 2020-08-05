
#' Sets up a collection tibble for holding and distributing queries across a rand of tables in the database
#' @name collect_table
#' @param settings "inpatient", "outpatient" or c("inpatient","outpatient")
#' @param sources "ccae" or "mdcr" or c("ccae","mdcr")
#' @param years vector of years to collect. Note: if no argument is provided all years will be selected
#' @return A tibble with all possible combinations of setting, source, and year.
#' @export
#'

collect_table <- function (settings = c("inpatient", "outpatient"),
                           sources = c("ccae", "mdcr"),
                           years = NULL)
{
  if (is.null(years)) {
    years <- stringr::str_pad(c(1:17), 2, pad = "0")
  }
  tibble::tibble(setting = settings) %>% dplyr::mutate(source = purrr::map(.data$setting,
                                                                           ~sources)) %>% tidyr::unnest() %>% dplyr::mutate(year = purrr::map(source,
                                                                                                                                              ~years)) %>% tidyr::unnest()
}

#' Builds a time map for a specific database
#' @name build_time_map_delay
#' @param collect_tab A tibble with the specific setting (i.e. inpatient or outpatient), source (i.e. ccae or mdcr), and year to access.
#' Default is all possible combinations of setting, source, and year.
#' @param db_con The database connection
#' @param db_path Path to the database.
#' @return A tibble with a time map containing visit level information from the database. Includes admission date, year, setting, enrolid,
#' standard place of care, key, discharge date, and location of care (i.e. inpatient, outpatient, or ED)
#' @export
#'

require(parallel)

build_time_map_delay <- function (db_con, db_path, collect_tab = collect_table())
{
  if (!any(dplyr::src_tbls(db_con) %in% c("outpatient_keys",
                                          "inpatient_keys"))) {
    warning("Database contains no visit keys. Temporary visit keys were generated using the collection table specified.")
    add_time_map_keys_delay(collect_tab = collect_table(), db_con = db_con, db_path = db_path,
                       temporary = TRUE)
  }
  dat <- bind_rows(db_con %>% dplyr::tbl("outpatient_keys") %>%
                 dplyr::collect(n = Inf) %>% dplyr::mutate(disdate = .data$svcdate) %>% dplyr::select(admdate = "svcdate", everything()),
                 db_con %>%
                 dplyr::tbl("inpatient_keys") %>%
                 dplyr::select(-.data$caseid) %>% dplyr::collect(n = Inf) %>%
                 dplyr::mutate(stdplac = -1L) %>% dplyr::select(.data$key, dplyr::everything())) %>% dplyr::arrange(.data$enrolid,
                                                                                                                         .data$admdate, .data$source)
  return(dat)
}

#' Generates outpatient and inpatient visit keys and stores them in the database.
#' @name add_time_map_keys_delay
#' @param collect_tab A tibble with the specific setting (i.e. inpatient or outpatient), source (i.e. ccae or mdcr), and year to access.
#' Default is all possible combinations of setting, source, and year.
#' @param db_con The database connection
#' @param db_path Path to the database.
#' @param overwrite Option to overwrite outpatient and inpatient key datasets if already present in database. Default is FALSE.
#' @param temporary Option to store the outpatient and inpatient key datasets in the database temporarily or permanently. Default is temporary.
#' @export
#'

add_time_map_keys_delay <- function (collect_tab = collect_table(), db_con, db_path, overwrite = FALSE,
                                temporary = TRUE)
{
  if (any(dplyr::src_tbls(db_con) %in% c("outpatient_keys",
                                         "inpatient_keys")) & overwrite == FALSE) {
    warning("Database contains keys and overwrite set to FALSE")
  }
  else {
    temp_keys <- build_time_map_keys_delay(collect_tab = collect_tab, db_path = db_path,
                                      db_con = db_con)
    dplyr::copy_to(dest = db_con, df = temp_keys$out_keys,
                   name = "outpatient_keys", temporary = temporary,
                   overwrite = overwrite)
    dplyr::copy_to(dest = db_con, df = temp_keys$in_keys,
                   name = "inpatient_keys", temporary = temporary, overwrite = overwrite)
  }
}

#' Gather variables from core information for a specific table.
#' @name get_core_data_delay
#' @param table_name The specific core table name in the database to access.
#' @param vars Vector of variables it the core table to return.
#' @param db_path Path to the database.
#' @param collect_n The number of observations to return.
#' @return A tibble with all the specified variables to return. Number of rows of tibble returned is determined by the collect_n argument.
#' @export
#'

get_core_data_delay <- function (table_name, vars = c(), db_path, collect_n = Inf) {
  tbl_name <- table_name
  db_con <- src_sqlite(db_path)

  get_vars <- dplyr::tbl(db_con, tbl_name) %>% dplyr::tbl_vars()
  if (is.null(vars)) {
    get_vars <- get_vars
  }
  else {
    get_vars <- vars[vars %in% get_vars]
  }
  out <- db_con %>% dplyr::tbl(tbl_name) %>% dplyr::select(get_vars) %>%
    dplyr::collect(n = collect_n)
  return(out)
}

#' Gather variables from core information over multiple core tables (in parallel).
#' @name gether_core_data_delay
#' @param collect_tab A tibble with the specific setting (i.e. inpatient or outpatient), source (i.e. ccae or mdcr), and year to access.
#' Default is all possible combinations of setting, source, and year.
#' @param vars Vector of variables it the core table to return.
#' @param db_path Path to the database.
#' @param collect_n The number of observations to return.
#' @param num_cores The number of worker cores to use. If not specified will determined the number of cores based on the which ever
#' is the smallest value between number of rows in for collect_tab or detected number of cores - 1.
#' @return A tibble with all the specified variables to return over all designated core tables.
#' @export
#'

gether_core_data_delay <- function (collect_tab = collect_table(), vars = c(), db_path,
                               collect_n = Inf, num_cores = NULL) {
  tab <- collect_tab %>% mutate(table = paste0(setting, "_core_", source, "_", year))

  db_path2 <- db_path
  vars2 <- vars
  collect_n2 <- collect_n

  # set up clusters
  if (is.null(num_cores)) {
    num_cores <- min(nrow(tab), parallel::detectCores() - 1)
  } else {
    num_cores <- num_cores
  }

  cluster <- parallel::makeCluster(num_cores)
  parallel::clusterExport(cluster, varlist = c("get_core_data_delay"))

  parallel::clusterCall(cluster, function() library(tidyverse))
  parallel::clusterCall(cluster, function() library(dplyr))

  tmp <- parallel::parLapply(cl = cluster,
                             1:length(tab$table),
                             function(x){get_core_data_delay(table_name = tab$table[x],
                                                        vars = vars2,
                                                        db_path = db_path2,
                                                        collect_n = collect_n2)})
  parallel::stopCluster(cluster)
  gc()

  out <- tibble()
  for (i in 1:length(tmp)){
    x <- tmp[[i]] %>% nest(core_data = everything())
    out <- bind_rows(out, x)
  }
  tab <- bind_cols(tab %>% select(-table), out)
  return(tab)
}

#' Build inpatient and outpatient (also includes ED) key datasets
#' @name build_time_map_keys_delay
#' @param collect_tab A tibble with the specific setting (i.e. inpatient or outpatient), source (i.e. ccae or mdcr), and year to access.
#' Default is all possible combinations of setting, source, and year.
#' @param db_con The database connection
#' @param db_path Path to the database.
#' @export
#'

build_time_map_keys_delay <-
  function (collect_tab = collect_table(), db_con, db_path) {
    if (any(collect_tab$setting == "inpatient")) {
      temp.in <- collect_tab %>% dplyr::filter(.data$setting ==
                                                 "inpatient") %>%
        gether_core_data_delay(vars = c("caseid", "enrolid", "admdate", "disdate", "los"),
                          db_path = db_path)

      temp.in <- temp.in %>% dplyr::mutate(ccae = ifelse(source == "ccae", 1L, 0L)) %>%
        dplyr::select("year", "ccae", "core_data") %>% tidyr::unnest() %>%
        dplyr::mutate(disdate = ifelse(is.na(.data$disdate), .data$admdate + .data$los,
                                       .data$disdate),
                      inpatient = 1L,
                      source = "inpatient") %>%
        dplyr::select(-.data$los)
    }
    else {
      temp.in <- tibble::tibble(year = character(), ccae = integer(),
                                caseid = integer(), enrolid = integer(), source = character(),
                                admdate = integer(), disdate = integer(), inpatient = integer())
    }
    if (any(collect_tab$setting == "outpatient")) {
      # in vars list add extra variables to identify ED visits
      temp.out <- collect_tab %>% dplyr::filter(.data$setting == "outpatient") %>%
        gether_core_data_delay(vars = c("enrolid", "stdplac", "svcdate", "stdprov", "svcscat",
                                   "procgrp", "revcode", "proc1"),
                          db_path = db_path) %>%
        dplyr::mutate(core_data = purrr::map(.data$core_data, ~dplyr::distinct(.)))

      temp.out <- temp.out %>% dplyr::mutate(ccae = ifelse(source == "ccae", 1L, 0L)) %>%
        dplyr::select("year", "ccae", "core_data") %>%
        mutate(core_data = map(core_data, ~mutate(., procgrp = as.character(procgrp),
                                                     stdprov = as.character(stdprov)))) %>%
        tidyr::unnest() %>%
        dplyr::mutate(disdate = .data$svcdate, admdate = .data$svcdate, inpatient = 0L) %>%
        dplyr::select(-.data$svcdate)

      temp.out <- temp.out %>% mutate(source = ifelse((stdplac==23 |
                                                         ((stdplac %in% c(19,21,22,28)) &
                                                            (stdprov %in% c("220","428"))) |
                                                         ((stdplac %in% c(19,21,22,28)) &
                                                            svcscat %in% c("10120","10220","10320","10420","10520",
                                                                           "12220","20120","20220","21120","21220",
                                                                           "22120","22320","30120","30220","30320",
                                                                           "30420","30520","30620","31120","31220",
                                                                           "31320","31420","31520","31620")) |
                                                         (procgrp %in% c("110","111","114")) |
                                                         (revcode %in% c("450","451","452","453","454",
                                                                         "455","456","457","458","459")) |
                                                         (proc1 %in% c("99281","99282","99283","99284","99285"))), "ED", "outpatient")) %>%
        distinct(year, ccae, enrolid, admdate, disdate, source, stdplac, inpatient)
    }
    else {
      temp.out <- tibble::tibble(year = character(), ccae = integer(),
                                 enrolid = integer(),
                                 stdplac = integer(),
                                 source = character(),
                                 admdate = integer(), disdate = integer(), inpatient = integer())
    }
    temp_time_map <- dplyr::bind_rows(temp.in, temp.out) %>%
      dplyr::arrange(.data$enrolid, .data$admdate, .data$inpatient) %>%
      dplyr::mutate(key = dplyr::row_number())

    out_keys <- temp_time_map %>% dplyr::filter(.data$inpatient ==
                                                  0)  %>% dplyr::select("year", "ccae", "enrolid", "stdplac", "source",
                                                                        svcdate = "admdate", "key")

    in_keys <- temp_time_map %>% dplyr::filter(.data$inpatient ==
                                                 1) %>% dplyr::select("year", "ccae", "enrolid", "admdate", "source",
                                                                      "disdate", "caseid", "key")
    return(list(time_map = temp_time_map, out_keys = out_keys,
                in_keys = in_keys))
  }
