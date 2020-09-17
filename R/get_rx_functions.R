#' Return rx information for a specific list of ndc codes
#' @name gether_rx_data
#' @param table_name The specific rx core table to access
#' @param ndc_codes The ndc codes of interest
#' @param rx_vars List of variables to return
#' @param db_path Path to the database
#' @param collect_n The number of observations to return. Default in Inf
#' @return A tibble with ndc code of interest for the specified list of variables
#' @export

gether_rx_data <- function (table_name, ndc_codes, rx_vars = c(), db_path, collect_n = Inf) {
  tbl_name <- table_name
  db_con<- src_sqlite(db_path)
  get_vars <- dplyr::tbl(db_con, tbl_name) %>% dplyr::tbl_vars()
  if (is.null(rx_vars)) {
    get_vars <- get_vars
  }
  else {
    get_vars <- rx_vars[rx_vars %in% get_vars]
  }
  out <- db_con %>% dplyr::tbl(tbl_name) %>% dplyr::filter(.data$ndcnum %in%
                                                             ndc_codes) %>%
    dplyr::select(get_vars) %>% dplyr::collect(n = collect_n)
  return(out)
}


#' Return information for a specific list of rx codes over multiple core tables (in parallel)
#' @name gather_rx_data
#' @param collect_tab A tibble with the specific setting (i.e. inpatient or outpatient), source (i.e. ccae or mdcr), and year to access.
#' Default is all possible combinations of setting, source, and year
#' @param ndc_codes The rx codes of interest
#' @param proc_vars List of variables to return
#' @param rx_vars Path to the database
#' @param collect_n The number of observations to return
#' @param num_cores The number of worker cores to use. If not specified will determined the number of cores based on the which ever
#' is the smallest value between number of rows in for collect_tab or detected number of cores - 1
#' @return A tibble with cpt code of interest for the specified list of variables
#' @export
#'
gather_rx_data <- function (collect_tab = collect_table(), ndc_codes,
                            rx_vars = c("enrolid", "ndcnum", "svcdate"),
                            db_path, collect_n = Inf, num_cores = NULL) {

  tab <- collect_tab %>% mutate(table = paste0("rx_core_", source, "_", year))

  db_path2 <- db_path
  ndc_codes2 <- ndc_codes
  rx_vars2 <- rx_vars
  collect_n2 <- collect_n

  # set up clusters
  if (is.null(num_cores)) {
    num_cores <- min(nrow(tab), parallel::detectCores() - 1)
  } else {
    num_cores <- num_cores
  }

  cluster <- parallel::makeCluster(num_cores)
  parallel::clusterExport(cluster, varlist = c("gether_rx_data"))

  parallel::clusterCall(cluster, function() library(tidyverse))
  parallel::clusterCall(cluster, function() library(dplyr))

  tmp <- parallel::parLapply(cl = cluster,
                                 1:length(tab$table),
                                 function(x){gether_rx_data(table_name = tab$table[x],
                                                            ndc_codes = ndc_codes2,
                                                            rx_vars = rx_vars2,
                                                            db_path = db_path2,
                                                            collect_n = collect_n2)})

  parallel::stopCluster(cluster)
  gc()


  out1<- tibble()
  for (i in 1:length(tmp)){
    x <- tmp[[i]] %>% nest(rx_data = everything())
    out1 <- bind_rows(out1, x)
  }

  out <-  bind_cols(tab %>% select(-table), out1)

  return(out)
}



