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
read_qapp <- function(file) {
  # read in the raw file from excel sheet
  raw_data <- readxl::read_excel(file)


  data <- raw_data %>%
    transmute(
      "Project ID" = "",
      "Monitoring Location ID" = purrr::map_chr(SAMPLENAME, ~fix_location_names(.)),
      "Activity ID" = paste(`Monitoring Location ID`,lubridate::as_date(lubridate::mdy_hms(SAMPDATE)),
                            format(lubridate::mdy_hms(SAMPDATE), "%H:%M:%S"),
                            "FM", sep = ":"),
      "Activity Type" = "Field Measure/Obs",
      "Activity Media Name" = RPTMATRIX,
      "Activity Start Date" = lubridate::as_date(lubridate::mdy_hms(SAMPDATE)),
      "Activity Start Time" = format(lubridate::mdy_hms(SAMPDATE), "%H:%M:%S"),
      "Activity Start Time Zone" = "PST",
      "Activity Depth/Height Measure" = "",
      "Activity Depth/Height Unit" = "",
      "Sample Collection Method ID" = "",
      "Sample Collection Equipment Name" = "",
      "Sample Collection Equipment Comment" = "",
      "Characteristic Name" = ANALYTE,
      "Method Speciation" = "",
      "Result Detection Condition" = "",
      "Result Value" = readr::parse_number(Result), # some spreadsheets use RESULT others Result
      "Result Unit" = purrr::map_chr(UNITS, ~fix_units(.)),
      "Result Qualifier" = case_when(
        Result == "ND" ~ "ND",
        stringr::str_detect(Result, ">") ~ ">",
        TRUE ~ ""
      ),
      "Result Sample Fraction" = "",
      "Result Status ID" = "",
      "Statistical Base Code" = "",
      "Result Value Type" = "",
      "Result Analytical Method ID" = "",
      "Result Analytical Method Context" = "",
      "Analysis Start Date" = "",
      "Result Detection Limit Type" = "",
      "Result Detection Limit Value" = "",
      "Result Detection Limit Unit" = "",
      "Result Comment" = ""
    )

  list(data = data)


}
