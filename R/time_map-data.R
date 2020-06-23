#' Example time_map for running simulations
#'
#' This file contains an example of the structure of a time_map required
#' for running simulations. Each line of a time_map dataset contains a distinct
#' visit (i.e., visit to a distinct care location on a given day).
#' At least five different variables are required in a time_map dataset: the enrolid of the patient
#' (this is just a unique integer value identifying each patient), an inticator for inpatient
#' visits, an indicator for ed visits, an integer value denoting the days since the diagnosis
#' occured (this value is negative for visits prior to the index diagnosis), and an indicator
#' if the visit contained a particular SSD. Note: this time_map can also contain multiple different
#' SSD indicators. The SSD indicator to be used in the simulation will be specified in the
#' prep_sim_data() function.
#'
#' @docType data
#'
#' @usage data(time_map)
#'
#' @keywords datasets
#'
#' @references ???
#'
#' @source created
#'
#' @examples
#' data(time_map)
"time_map"
