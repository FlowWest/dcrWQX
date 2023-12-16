#' @title Fix Characteristic Names
#' @description when needed it transforms the characteristic name as reported from the lab
#' to the WQX compliant version
#' @export
fix_characteristic_name <- function(name) {
  fixed_name <- characteristic_names_lookup %>%
    filter(raw_name == name) %>%
    pull(wqx_name)

  if (length(fixed_name) == 0) {
    return(name)
  } else {
    return(fixed_name)
  }

}


# This function will simply take variations of units and covert
# them to the appropriate format. I will add more entries to
# this function as more issues come up
# by default the function will return the input if it cannot
# find the appropriate transformation
#' @export
to_wqx_units <- function(unit) {

  converted_unit <- measuremnt_units_lookup %>%
    filter(raw_data_units == unit) %>%
    pull(wqx_units)

  if (length(converted_unit) == 0) {
    return(unit)
  } else {
    # throw a message ?
    return(converted_unit)
  }
}

#' @title Result Detection Condition
#' @description construct detection condition based on other columns in the data. Rules from
#' WQX state that if there is a Result in the `Result Value` then this value should not be provided.
#' In the case when `Result Value` is missing then we must specify `Result Detection Condition`
#' @export
get_results_detection_condition <- function(result) {

  if (!is.na(result)) {
    return(NA_character_)
  } else {
    return("Not Detected")
  }

}

#' @export
get_method_context <- function(method_name) {
  method_id_and_context %>%
    filter(METHODNAME == method_name) %>%
    pull(`Result Analytical Method Context`)
}

#' @export
get_method_id <- function(method_name) {
  method_id_and_context %>%
    filter(METHODNAME == method_name) %>%
    pull(`Result Analytical Method ID`)
}


