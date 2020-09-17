# Fix location names to be consistent across all of the datasets
fix_location_names <- function(loc) {
  if (stringr::str_detect(tolower(loc), "site 1")) "SITE 1"
  else if (stringr::str_detect(tolower(loc), "site 2")) "SITE 2"
  else if (stringr::str_detect(tolower(loc), "site 3")) "SITE 3"
  else if (stringr::str_detect(tolower(loc), "site 4")) "SITE 4"
  else if (stringr::str_detect(tolower(loc), "site 5")) "SITE 5"
  else if (stringr::str_detect(tolower(loc), "site 6")) "SITE 6"
  else if (stringr::str_detect(tolower(loc), "site 7")) "SITE 7"
  else loc
}

# This function will simply take variations of units and covert
# them to the appropriate format. I will add more entries to
# this function as more issues come up
# by default the function will return the input if it cannot
# find the appropriate transformation
fix_units <- function(unit) {
  switch (tolower(unit),
    "mpn/100ml" = "MPN/100 ml",
    unit
  )
}

#' @export
read_qapp <- function(file, result_col = "Result") {
  # read in the raw file from excel sheet
  raw_data <- readxl::read_excel(file)


  data <- raw_data %>%
    transmute(
      "Project ID" = "RCS", # needs to be filled out
      "Monitoring Location ID" = purrr::map_chr(SAMPLENAME, ~fix_location_names(.)),
      "Activity ID" = paste(`Monitoring Location ID`,lubridate::as_date(lubridate::mdy_hms(SAMPDATE)),
                            "FM", sep = ":"),
      "Activity Type" = "Field Measure/Obs",
      "Activity Media Name" = RPTMATRIX,
      "Activity Start Date" = lubridate::as_date(lubridate::mdy_hms(SAMPDATE)),
      "Activity Start Time" = NA_character_,
      "Activity Start Time Zone" = NA_character_,
      "Activity Depth/Height Measure" = NA_character_,
      "Activity Depth/Height Unit" = NA_character_,
      "Sample Collection Method ID" = "DCR-QAPP",
      "Sample Collection Equipment Name" = NA_character_,
      "Sample Collection Equipment Comment" = NA_character_,
      "Characteristic Name" = ANALYTE,
      "Method Speciation" = NA_character_,
      "Result Detection Condition" = NA_character_,
      "Result Value" = readr::parse_number(result_col), # some spreadsheets use RESULT others Result
      "Result Unit" = purrr::map_chr(UNITS, ~fix_units(.)),
      "Result Qualifier" = case_when(
        result_col == "ND" ~ "ND",
        stringr::str_detect(result_col, ">") ~ ">",
        TRUE ~ ""
      ),
      "Result Sample Fraction" = NA_character_,
      "Result Status ID" = NA_character_,
      "Statistical Base Code" = NA_character_,
      "Result Value Type" = NA_character_,
      "Result Analytical Method ID" = NA_character_,
      "Result Analytical Method Context" = NA_character_,
      "Analysis Start Date" = lubridate::as_date(lubridate::mdy_hms(SAMPDATE)),
      "Analysis Start Time Zone" = "PST",
      "Analysis Start Time" = format(lubridate::mdy_hms(SAMPDATE), "%H:%M:%S"),
      "Result Detection Limit Type" = NA_character_,
      "Result Detection Limit Value" = NA_character_,
      "Result Detection Limit Unit" = NA_character_,
      "Result Comment" = NA_character_
    )

  list(data = data)


}
