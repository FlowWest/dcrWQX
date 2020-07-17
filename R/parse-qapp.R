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

fix_units <- function(char, unit) {
  if (char == "E. Coli" & unit == "MPN/100 ml")
    return("MPN/100mL")
  else if(char == "Total Coliforms" & unit == "MPN/100 ml")
    return("MPN/100mL")
  else if(char == "Fecal Coliforms" & unit == "MPN/100 ml")
    return("MPN/100mL")
  else
    return(unit)
}

read_quapp <- function(file) {
  samp_data %>%
    transmute(
      "Project ID" = "",
      "Monitoring Location ID" = map_chr(SAMPLENAME, ~fix_location_names(.)),
      "Activity ID" = paste(`Monitoring Location ID`,as_date(mdy_hms(SAMPDATE)),
                            format(mdy_hms(SAMPDATE), "%H:%M:%S"),
                            "FM", sep = ":"),
      "Activity Type" = "Field Measure/Obs",
      "Activity Media Name" = RPTMATRIX,
      "Activity Start Date" = as_date(mdy_hms(SAMPDATE)),
      "Activity Start Time" = format(mdy_hms(SAMPDATE), "%H:%M:%S"),
      "Activity Start Time Zone" = "PST",
      "Activity Depth/Height Measure" = "",
      "Activity Depth/Height Unit" = "",
      "Sample Collection Method ID" = "",
      "Sample Collection Equipment Name" = "",
      "Sample Collection Equipment Comment" = "",
      "Characteristic Name" = ANALYTE,
      "Method Speciation" = "",
      "Result Detection Condition" = "",
      "Result Value" = readr::parse_number(Result),
      "Result Unit" = map2_chr(`Characteristic Name`, UNITS, ~fix_units(.x, .y)),
      "Result Qualifier" = case_when(
        Result == "ND" ~ "ND",
        str_detect(Result, ">") ~ ">",
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


}
