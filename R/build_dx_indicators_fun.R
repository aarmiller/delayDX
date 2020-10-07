
#' Gather all visit keys containing specific diagnosis codes
#' @name gether_dx_keys_delay
#' @param collect_tab A tibble with the specific setting (i.e. inpatient or outpatient), source (i.e. ccae or mdcr), and year to access.
#' Default is all possible combinations of setting, source, and year
#' @param dx_list A list of specific diagnosis codes that are of interest. The diagnosis codes need to be seperated into into ICD 9 and
#' ICD 10 specific codes. The list elements need to be labled as icd9_codes and icd10_codes
#' @param db_path Path to the database
#' @param inpatient_keys An object containing all the inpatient keys in the database
#' @param outpatient_keys An object containing all the outpatient keys in the database
#' @return A tibble with all the specified diagnosis codes and the corresponding visit key where the diagnosis codes appeared
#' @export
#'

require(parallel)

gether_dx_keys_delay <- function (collect_tab = collect_table(), dx_list, db_path, inpatient_keys, outpatient_keys) {
  db_con <- src_sqlite(db_path)
  icd_9_codes <- dx_list$icd9_codes
  icd_10_codes <- dx_list$icd10_codes
  if(collect_tab$setting == "inpatient"){
    if(as.integer(collect_tab$year) < 15){
    in_temp <- collect_tab %>% dplyr::filter(as.integer(.data$year) <
                                                15) %>% dplyr::mutate(data = purrr::map2(.data$source,
                                                                                         .data$year, ~dplyr::tbl(db_con, paste0("inpatient_dx_",
                                                                                                                                .x, "_", .y)) %>% dplyr::filter(.data$dx %in% icd_9_codes) %>%
                                                                                           dplyr::distinct(.data$caseid, .data$dx) %>% dplyr::inner_join(dplyr::tbl(db_con,
                                                                                                                                                                    paste0("inpatient_core_", .x, "_", .y)) %>% dplyr::select(.data$caseid,
                                                                                                                                                                                                                              .data$enrolid), by = "caseid") %>% dplyr::distinct(.data$enrolid,
                                                                                                                                                                                                                                                                                 .data$caseid, .data$dx) %>% dplyr::collect(n = Inf) %>%
                                                                                           dplyr::mutate(enrolid = as.integer(.data$enrolid))))
    } else {
    in_temp1 <- collect_tab %>% dplyr::filter(as.integer(.data$year) >
                                                14) %>% dplyr::mutate(data = purrr::map2(.data$source,
                                                                                         .data$year, ~dplyr::tbl(db_con, paste0("inpatient_dx9_",
                                                                                                                                .x, "_", .y)) %>% dplyr::filter(.data$dx %in% icd_9_codes) %>%
                                                                                           dplyr::distinct(.data$caseid, .data$dx) %>% dplyr::inner_join(dplyr::tbl(db_con,
                                                                                                                                                                    paste0("inpatient_core_", .x, "_", .y)) %>% dplyr::select(.data$caseid,
                                                                                                                                                                                                                              .data$enrolid), by = "caseid") %>% dplyr::distinct(.data$enrolid,
                                                                                                                                                                                                                                                                                 .data$caseid, .data$dx) %>% dplyr::collect(n = Inf) %>%
                                                                                           dplyr::mutate(enrolid = as.integer(.data$enrolid))))

    in_temp2 <- collect_tab %>% dplyr::filter(as.integer(.data$year) >
                                                14) %>% dplyr::mutate(data = purrr::map2(source, .data$year,
                                                                                         ~dplyr::tbl(db_con, paste0("inpatient_dx10_", .x, "_",
                                                                                                                    .y)) %>% dplyr::filter(.data$dx %in% icd_10_codes) %>%
                                                                                           dplyr::distinct(.data$caseid, .data$dx) %>% dplyr::inner_join(dplyr::tbl(db_con,
                                                                                                                                                                    paste0("inpatient_core_", .x, "_", .y)) %>% dplyr::select(.data$caseid,
                                                                                                                                                                                                                              .data$enrolid), by = "caseid") %>% dplyr::distinct(.data$enrolid,
                                                                                                                                                                                                                                                                                 .data$caseid, .data$dx) %>% dplyr::collect(n = Inf) %>%
                                                                                           dplyr::mutate(enrolid = as.integer(.data$enrolid))))
    in_temp <- dplyr::bind_rows(in_temp1, in_temp2)
    rm(in_temp1, in_temp2)
    }

    if (nrow(in_temp %>% tidyr::unnest()) > 0){
      in_temp <- in_temp%>%
      dplyr::select(-.data$setting) %>% tidyr::unnest() %>%
      dplyr::group_by(source, .data$year) %>% tidyr::nest()

      dx_keys <- inpatient_keys %>% dplyr::mutate(enrolid = as.integer(.data$enrolid)) %>%
        dplyr::select(.data$ccae, .data$year, .data$caseid, .data$key) %>%
        dplyr::inner_join(in_temp %>% dplyr::mutate(ccae = ifelse(source ==
                                                                    "ccae", 1L, 0L)) %>% tidyr::unnest(), by = c("ccae",
                                                                                                                 "year", "caseid")) %>% dplyr::select(.data$dx, .data$key)
      dx_keys <- dx_keys %>%
        dplyr::distinct()
    } else {
      dx_keys <- NULL
    }
  }
  if(collect_tab$setting == "outpatient"){
    if(as.integer(collect_tab$year) < 15){
    out_temp <- collect_tab %>% dplyr::filter(as.integer(.data$year) <
                                                 15) %>% dplyr::mutate(data = purrr::map2(source, .data$year,
                                                                                          ~dplyr::tbl(db_con, paste0("outpatient_dx_", .x, "_",
                                                                                                                     .y)) %>% dplyr::filter(.data$dx %in% icd_9_codes) %>%
                                                                                            dplyr::distinct(seqnum_o, .data$enrolid, .data$svcdate,
                                                                                                            .data$dx) %>% dplyr::inner_join(dplyr::tbl(db_con,
                                                                                                                                                       paste0("outpatient_core_", .x, "_", .y)) %>% dplyr::select(seqnum_o,
                                                                                                                                                                                                                  .data$stdplac), by = "seqnum_o") %>% dplyr::distinct(.data$enrolid,
                                                                                                                                                                                                                                                                       .data$svcdate, .data$stdplac, .data$dx) %>% dplyr::collect(n = Inf) %>%
                                                                                            dplyr::mutate(enrolid = as.integer(.data$enrolid))))
    } else {
    out_temp1 <- collect_tab %>% dplyr::filter(as.integer(.data$year) >
                                                 14) %>% dplyr::mutate(data = purrr::map2(source, .data$year,
                                                                                          ~dplyr::tbl(db_con, paste0("outpatient_dx9_", .x, "_",
                                                                                                                     .y)) %>% dplyr::filter(.data$dx %in% icd_9_codes) %>%
                                                                                            dplyr::distinct(seqnum_o, .data$enrolid, .data$svcdate,
                                                                                                            .data$dx) %>% dplyr::inner_join(dplyr::tbl(db_con,
                                                                                                                                                       paste0("outpatient_core_", .x, "_", .y)) %>% dplyr::select(seqnum_o,
                                                                                                                                                                                                                  .data$stdplac), by = "seqnum_o") %>% dplyr::distinct(.data$enrolid,
                                                                                                                                                                                                                                                                       .data$svcdate, .data$stdplac, .data$dx) %>% dplyr::collect(n = Inf) %>%
                                                                                            dplyr::mutate(enrolid = as.integer(.data$enrolid))))
    out_temp2<- collect_tab %>% dplyr::filter(as.integer(.data$year) >
                                                 14) %>% dplyr::mutate(data = purrr::map2(source, .data$year,
                                                                                          ~dplyr::tbl(db_con, paste0("outpatient_dx10_", .x, "_",
                                                                                                                     .y)) %>% dplyr::filter(.data$dx %in% icd_10_codes) %>%
                                                                                            dplyr::distinct(seqnum_o, .data$enrolid, .data$svcdate,
                                                                                                            .data$dx) %>% dplyr::inner_join(dplyr::tbl(db_con,
                                                                                                                                                       paste0("outpatient_core_", .x, "_", .y)) %>% dplyr::select(seqnum_o,
                                                                                                                                                                                                                  .data$stdplac), by = "seqnum_o") %>% dplyr::distinct(.data$enrolid,
                                                                                                                                                                                                                                                                       .data$svcdate, .data$stdplac, .data$dx) %>% dplyr::collect(n = Inf) %>%
                                                                                            dplyr::mutate(enrolid = as.integer(.data$enrolid))))
    out_temp <- dplyr::bind_rows(out_temp1, out_temp2)
    rm(out_temp1, out_temp2)
    }

    if (nrow(out_temp %>% tidyr::unnest()) > 0){
      out_temp <- out_temp%>%
      dplyr::select(-.data$setting) %>% tidyr::unnest() %>%
      dplyr::group_by(source, .data$year) %>% tidyr::nest()

      dx_keys <- outpatient_keys  %>% dplyr::mutate(enrolid = as.integer(.data$enrolid)) %>%
        dplyr::select(.data$enrolid, .data$stdplac, .data$svcdate,
                      .data$key) %>% dplyr::inner_join(out_temp %>% dplyr::select(.data$data) %>%
                                                         tidyr::unnest(), by = c("enrolid", "stdplac", "svcdate")) %>%
        dplyr::select(.data$key, .data$dx)

      dx_keys <- dx_keys %>%
        dplyr::distinct()
    } else {
      dx_keys <- NULL
    }
  }
  return(dx_keys)
}

#' Gather all visit keys containing specific diagnosis codes over multiple combinations of setting, source, and year (in parallel)
#' @name build_dx_indicators_delay
#' @param condition_dx_list A list of specific diagnosis codes that are of interest. The diagnosis codes need to be seperated into
#' diagnosis categories (e.g. cough, fever, ect.) and within the categories diagnosis codes should be seperated into ICD 9 and
#' ICD 10 specific codes, with list elements labled as icd9_codes and icd10_codes
#' @param db_path Path to the database
#' @param db_con The database connection
#' @param collect_tab A tibble with the specific setting (i.e. inpatient or outpatient), source (i.e. ccae or mdcr), and year to access.
#' Default is all possible combinations of setting, source, and year
#' @param num_cores The number of worker cores to use. If not specified will determined the number of cores based on the which ever
#' is the smallest value between number of rows in for collect_tab or detected number of cores - 1
#' @param return_keys_only Logical to return only the  visit keys containing specific diagnosis codes
#' @return A tibble with visit keys and indicators for the diagnosis codes categories supplied to the condition_dx_list argument
#' @export
#'

build_dx_indicators_delay <- function (condition_dx_list, db_con, db_path, collect_tab = collect_table(), num_cores = NULL,
                                       return_keys_only = FALSE) {
  if (!any(dplyr::src_tbls(db_con) %in% c("outpatient_keys",
                                          "inpatient_keys"))) {
    warning("Database contains no visit keys. Temporary visit keys were generated using the collection table specified.")
    add_time_map_keys_delay(collect_tab = collect_tab, db_con = db_con, db_path = db_path,
                      temporary = TRUE)
  }

  if (return_keys_only == FALSE){
  all_cond_codes <- list(icd9_codes = purrr::map(condition_dx_list,
                                                 ~.$icd9_codes) %>% unlist(use.names = F),
                         icd10_codes = purrr::map(condition_dx_list, ~.$icd10_codes) %>% unlist(use.names = F))
  } else {
    all_cond_codes <- condition_dx_list
  }

  inpatient_keys <- db_con %>% dplyr::tbl("inpatient_keys") %>%
    dplyr::collect(n = Inf)

  outpatient_keys <- db_con %>% dplyr::tbl("outpatient_keys") %>%
    dplyr::collect(n = Inf)

  db_path2 <- db_path
  collect_tab2 <- collect_tab

  # set up clusters
  if (is.null(num_cores)) {
    num_cores <- min(nrow(collect_tab2), parallel::detectCores() - 1)
  } else {
    num_cores <- num_cores
  }

  cluster <- parallel::makeCluster(num_cores)
  parallel::clusterExport(cluster, varlist = c("gether_dx_keys_delay"))
  parallel::clusterCall(cluster, function() library(tidyverse))
  parallel::clusterCall(cluster, function() library(dplyr))

  #give each worker only a row of the collect_tab
  #gether_dx_keys_delay will evaluate specificrow of the collect_tab for the worker
  tmp <- parallel::parLapply(cl = cluster,
                             1:nrow(collect_tab2),
                             function(x){gether_dx_keys_delay(collect_tab = collect_tab2[x, ],
                                                         dx_list = all_cond_codes,
                                                         db_path = db_path2,
                                                         inpatient_keys = inpatient_keys,
                                                         outpatient_keys = outpatient_keys)})
  parallel::stopCluster(cluster)
  gc()

  rm(inpatient_keys, outpatient_keys)

  cond_keys <- tibble()
  for (i in 1:length(tmp)){
    x <- tmp[[i]]
    if (!is.null(x)){
      cond_keys <- bind_rows(cond_keys, x)
    }
  }

  if (return_keys_only == TRUE){
    return(cond_keys)
  }

  cond_keys_name <- tibble::tibble(name = names(condition_dx_list)) %>%
    dplyr::mutate(dx = purrr::map(.data$name, ~condition_dx_list[[.]] %>%
                                    unlist())) %>% tidyr::unnest() %>% dplyr::inner_join(cond_keys,
                                                                                         by = "dx")
  cond_inds <- cond_keys_name %>% dplyr::distinct(.data$name,
                                                  .data$key) %>%
    dplyr::mutate(dx_ind = 1L) %>% tidyr::spread(key = .data$name, value = .data$dx_ind) %>%
    dplyr::inner_join(cond_keys_name %>%dplyr::distinct(.data$key) %>%
                        dplyr::mutate(any_ind = 1L), by = "key") %>%
    dplyr::mutate_at(.vars = dplyr::vars(-.data$key), .funs = list(~ifelse(is.na(.), 0L, .)))
  return(cond_inds)
}
