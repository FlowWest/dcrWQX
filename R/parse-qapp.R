# Fix location names to be consistent across all of the datasets
#' @export
fix_location_names <- function(loc) {
  if (stringr::str_detect(tolower(loc), "site 1")) "SITE_1_DCR"
  else if (stringr::str_detect(tolower(loc), "site 2")) "SITE_2_DCR"
  else if (stringr::str_detect(tolower(loc), "site 3")) "SITE_3_DCR"
  else if (stringr::str_detect(tolower(loc), "site 4")) "SITE_4_DCR"
  else if (stringr::str_detect(tolower(loc), "site 5")) "SITE_5_DCR"
  else if (stringr::str_detect(tolower(loc), "site 6")) "SITE_6_DCR"
  else if (stringr::str_detect(tolower(loc), "site 7")) "SITE_7_DCR"
  else loc
}

#' @export
read_qapp <- function(file, results_status = "Final") {
  # read in the raw file from excel sheet
  raw_data <- readxl::read_excel(file, sheet = "SAMPDATA")

  if (is.null(raw_data$RESULT)) {
    stop("There was an error in parsing qapp file, it looks like the result columns is not
         labelled 'RESULT', please change this to 'RESULT'")
  }

  data <-
    raw_data %>%
    transmute(
      "Project ID" = "SWQM_DCR", # for sample data
      "Monitoring Location ID" = purrr::map_chr(SAMPLENAME, ~fix_location_names(.)),
      "Activity ID" = paste(`Monitoring Location ID`,lubridate::as_date(lubridate::mdy_hms(SAMPDATE)),
                            "SR", sep = ":"),
      "Activity Type" = "Sample-Routine",
      "Activity Media Name" = "Water", # sample are always taken from water
      "Activity Start Date" = lubridate::as_date(lubridate::mdy_hms(SAMPDATE)),
      # "Activity Start Time" = NA_character_,
      # "Activity Start Time Zone" = NA_character_,
      # "Activity Depth/Height Measure" = NA_character_,
      # "Activity Depth/Height Unit" = NA_character_,
      "Sample Collection Method ID" = "DCR-QAPP",
      "Sample Collection Equipment Name" = "Water Bottle",
      "Sample Collection Equipment Comment" = NA_character_,
      "Characteristic Name" = purrr::map_chr(ANALYTE, fix_characteristic_name),
      "Method Speciation" = NA_character_,
      "Result Value" = readr::parse_number(RESULT), # some spreadsheets use RESULT others Result
      "Result Detection Condition" = purrr::map_chr(`Result Value`, get_results_detection_condition),
      "Result Unit" = purrr::map_chr(UNITS, to_wqx_units),
      "Result Sample Fraction" = NA_character_, # not needed for any of DCR samples
      "Result Status ID" = results_status,
      "Statistical Base Code" = NA_character_, # for sampling there is no summary
      "Result Value Type" = "Actual", # based on existing dasta
      "Result Analytical Method ID" = purrr::map_chr(METHODNAME, get_method_id),
      "Result Analytical Method Context" = purrr::map_chr(METHODNAME, get_method_context),
      "Analysis Start Date" = lubridate::as_date(lubridate::mdy_hms(ANADATE)),
      "Analysis Start Time" = format(lubridate::mdy_hms(ANADATE), "%H:%M:%S"),
      "Analysis Start Time Zone" = "PST",
      # "Result Detection Limit Type" = NA_character_,
      # "Result Detection Limit Value" = NA_character_,
      # "Result Detection Limit Unit" = NA_character_,
      # "Result Comment" = NA_character_
    )

  list(data = data)
}
