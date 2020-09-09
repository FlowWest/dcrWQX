# this script exports several functions for parsing
# raw transducer data ready for integration into the dcr wqx
# local database and then to the wqx

# these transducer files are typically filled with metadata
# for the first ~70 lines, this is then followed by data records

#' @title Read transducer data
#' @description read transducer raw data and restructure to conform
#' with DCR and WQX compliant schema
#' @param file a csv file with transducer output
#' @export
read_transducer <- function(file) {
  header_chunk <- readr::read_lines(file, n_max = 100)
  skip_n <- which(stringr::str_detect(header_chunk, "^Date and Time"))[2]
  device_props <- transducer_properties(header_chunk)

  # use serial number for the device to determine the processing, as of now
  # there is just one transducer that reports only Air temperature
  col_info <- transducer_col_info(device_props$serial)

  data <- read_delim(file,
                     delim = ",",
                     trim_ws = TRUE,
                     skip = skip_n,
                     col_names = col_info$col_names,
                     col_types = col_info$col_types,
                     na = ""
  ) %>%
    dplyr::select(-dummy)

  wqx_data <- transducer_to_wqx(data, device_props)

  list(
    "data" = wqx_data,
    "metadata" = device_props)
}

#' @title Transducer Properties
#' @description Get transducer device properties from the text file
#' @param v a vector whose elements are each line in the text file
transducer_properties <- function(v) {
  device_props <- c("Report Date:,",
                    "Report User Name:,",
                    "Report Computer Name:,",
                    "^Device,", "^Site,", "^Device Name,", "^Serial Number,",
                    "Firmware Version,", "Used Memory,")
  device_props %>%
    purrr::map(~stringr::str_replace(v[stringr::str_detect(v, .)], ., "")) %>%
    purrr::set_names(c("report_date", "username", "computer_name",
                       "type", "site", "name", "serial", "firmware",
                       "used_memory"))

}

device_name_to_location_id <- c(
  "DCR Baro" = "Baro1",
  "DCR ED1" = "EPO1",
  "DCR VR1" = "VY1"
)

transducer_to_wqx <- function(data, metadata) {

  if (metadata$serial == "440478") {
    # this just needs to parse for air tempearture
    dplyr::bind_rows(
      parse_transducer_air_temp(data, metadata)
    )
  } else if (metadata$serial == "440965") {
    dplyr::bind_rows(
      # parse_transducer_stage(data, metadata),
      parse_transducer_water_temp(data, metadata)
    )
  } else if (metadata$serial == "440966") {
    bind_rows(
      # parse_transducer_stage(data, metadata),
      parse_transducer_water_temp(data, metadata)
    )
  } else {
    NULL
  }
}

#' @title Structure Transducer for WQX Air Temp
#' @param data Transducer data ready to be restructured to WQX
parse_transducer_air_temp <- function(data, metadata) {
  data %>%
    dplyr::transmute(
      "Project ID" = "RCS",
      "Monitoring Location ID" = device_name_to_location_id[metadata$name], # need to transform from metadata
      "Activity ID" = paste(
        `Monitoring Location ID`,
        format(lubridate::as_date(dateTime), "%Y%m%d"),
        "Air",
        "FM",
        sep = ":"),
      "Activity Type" = "Field Msr/Obs",
      "Activity Media Name" = "Air",
      "Activity Start Date" = lubridate::as_date(dateTime), # need to obtain from data
      "Activity Start Time" = NA_character_, # need to ontain fromd data
      "Activity Start Time Zone" = NA_character_,
      "Activity Depth/Height Measure" = NA_character_, # none here
      "Activity Depth/Height Unit" = NA_character_, # none
      "Sample Collection Method ID" = "DCR-QAPP",
      "Sample Collection Equipment Name" = "Probe/Sensor",
      "Sample Collection Equipment Comment" = metadata$type, # get this from the metadata
      "Characteristic Name" = "Temperature, air",
      "Method Speciation" = NA_character_, # nothing
      "Result Detection Condition" = NA_character_, # nothing
      "Result Value" = temperature_f, # from the data
      "Result Unit" = "deg F", # from data
      "Result Qualifier" = NA_character_, # nothing
      "Result Sample Fraction" = NA_character_, # nothing
      "Result Status ID" = "Final",
      "Statistical Base Code" = NA_character_, # nothing
      "Result Value Type" = "Actual",
      "Result Analytical Method ID" = NA_character_,
      "Result Analytical Method Context" = NA_character_,
      "Analysis Start Date" = lubridate::as_date(dateTime),
      "Analysis Start Time Zone" = "PST",
      "Analysis Start Time" = format(dateTime, "%H:%M:%S"),
      "Result Detection Limit Type" = NA_character_,
      "Result Detection Limit Value" = NA_real_,
      "Result Detection Limit Unit" = NA_character_,
      "Result Comment" = NA_character_,
      "Record_ID" = paste(`Activity ID`, `Analysis Start Time`, `Characteristic Name`,
                          sep = ":")
    ) %>%
    dplyr::distinct(Record_ID, `Characteristic Name`, .keep_all = TRUE)
}

#' @title Structure Transducer for WQX Stage
#' @param data Transducer data ready to be restructured to WQX
#' @export
parse_transducer_water_temp <- function(data, metadata) {
  data %>%
    dplyr::transmute(
      "Project ID" = "RCS",
      "Monitoring Location ID" = device_name_to_location_id[metadata$name], # need to transform from metadata
      "Activity ID" = paste(
        `Monitoring Location ID`,
        format(lubridate::as_date(dateTime), "%Y%m%d"),
        "Water",
        "FM",
        sep = ":"),
      "Activity Type" = "Field Msr/Obs", #
      "Activity Media Name" = "Water",
      "Activity Start Date" = lubridate::as_date(dateTime), # need to obtain from data
      "Activity Start Time" = NA_character_, # need to ontain fromd data
      "Activity Start Time Zone" = NA_character_,
      "Activity Depth/Height Measure" = NA_character_, # none here
      "Activity Depth/Height Unit" = NA_character_, # none
      "Sample Collection Method ID" = "DCR-QAPP",
      "Sample Collection Equipment Name" = "Probe/Sensor",
      "Sample Collection Equipment Comment" = metadata$type, # get this from the metadata
      "Characteristic Name" = "Temperature, water",
      "Method Speciation" = NA_character_, # nothing
      "Result Detection Condition" = ifelse(depth_ft <= .1, "Not Detected at Reporting Limit", NA_character_), # nothing
      "Result Value" = ifelse(depth_ft <= .1, NA_real_, temperature_f), # from the data
      "Result Unit" = "deg F", # from data
      "Result Qualifier" = NA_character_, # nothing
      "Result Sample Fraction" = NA_character_, # nothing
      "Result Status ID" = "Final",
      "Statistical Base Code" = NA_character_, # nothing
      "Result Value Type" = "Actual",
      "Result Analytical Method ID" = NA_character_,
      "Result Analytical Method Context" = NA_character_,
      "Analysis Start Date" = lubridate::as_date(dateTime),
      "Analysis Start Time Zone" = "PST",
      "Analysis Start Time" = format(dateTime, "%H:%M:%S"),
      "Result Detection Limit Type" = NA_character_,
      "Result Detection Limit Value" = NA_real_,
      "Result Detection Limit Unit" = NA_character_,
      "Result Comment" = NA_character_,
      "Record_ID" = paste(`Activity ID`, `Analysis Start Time`, `Characteristic Name`,
                          sep = ":")
    ) %>%
    dplyr::distinct(Record_ID, `Characteristic Name`, .keep_all = TRUE)
}

#' @title Column info needed to read data
#' @description return a list with the column headers and column
#' types needed to read in in the data using read_csv
transducer_col_info <- function(serial) {
  col_names <-
    switch (serial,
            "440478" = c("dateTime", "seconds", "pressure_psi", "temperature_f", "dummy"),
            "440965" = c("dateTime", "seconds", "pressure_psi", "temperature_f", "depth_ft", "barometric_pressure_psi", "dummy"),
            "440966" = c("dateTime", "seconds", "pressure_psi", "temperature_f", "depth_ft", "barometric_pressure_psi", "dummy"),
            as.character(NA)
    )

  col_types <-
    switch (serial,
            "440478" = cols(
              dateTime = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
              seconds = col_double(),
              pressure_psi = col_double(),
              temperature_f = col_double(),
              dummy = col_logical()
            ),
            "440965" = cols(
              dateTime = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
              seconds = col_double(),
              pressure_psi = col_double(),
              temperature_f = col_double(),
              depth_ft = col_double(),
              barometric_pressure_psi = col_double(),
              dummy = col_logical()
            ),
            "440966" = cols(
              dateTime = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
              seconds = col_double(),
              pressure_psi = col_double(),
              temperature_f = col_double(),
              depth_ft = col_double(),
              barometric_pressure_psi = col_double(),
              dummy = col_logical()
            ),
            as.character(NA)
    )

  list(
    col_names = col_names,
    col_types = col_types
  )
}

