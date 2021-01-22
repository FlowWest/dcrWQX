# this script exports several functions for parsing
# raw transducer data ready for integration into the dcr wqx
# local database and then to the wqx

# FROM WQX regarding uploading continuous data:
# Our recommendation for continuous monitoring data is to report to WQX a single
# activity in which you store the actual probe readings using every (4 hours, 2 hours or evenly distributed)
# intervals and to which you attach at the activity level a binary object formatted as text (".txt")
# file or compressed (".zip") file format for archiving the complete raw set of probe readings
# downloaded from the data logger.

scale_down_to_wqx <- function(data) {
  dts_to_keep <- tibble(dateTime=seq(min(data$dateTime), max(data$dateTime), by="2 hours"))
  dts_to_keep %>%
    left_join(data, by=c("dateTime"="dateTime"))
}

# these transducer files are typically filled with metadata
# for the first ~70 lines, this is then followed by data records

#' @title Read transducer data
#' @description read transducer raw data and restructure to conform
#' with DCR and WQX compliant schema
#' @param file a csv file with transducer output
#' @param downscale whether or not to downscale the data to 2 hour intervals (for wqx upload)
#' @export
read_transducer <- function(file, downslace = FALSE) {
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

  if (downslace) {
    data <- scale_down_to_wqx(data)
  }

  wqx_data <- transducer_to_wqx(data, device_props) %>%
    filter(!is.na(`Result Value`))

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
        format(min(lubridate::as_date(dateTime)), "%Y-%m"),
        format(max(lubridate::as_date(dateTime)), "%Y-%m"),
        "Air",
        "FM",
        sep = ":"),
      "Activity Type" = "Field Msr/Obs", # data logger
      "Activity Media Name" = "Air",
      "Activity Start Date" = format(min(lubridate::as_date(dateTime)), "%Y/%m/%d"), # need to obtain from data
      "Activity End Date" = format(max(lubridate::as_date(dateTime)), "%Y/%m/%d"), # need to obtain from data
      "Sample Collection Method ID" = "DCR-QAPP",
      "Sample Collection Equipment Name" = "Probe/Sensor",
      "Sample Collection Equipment Comment" = metadata$type, # get this from the metadata
      "Characteristic Name" = "Temperature, air",
      "Result Value" = temperature_f, # from the data
      "Result Unit" = "deg F", # from data
      "Result Depth/Height Measure" = NA_real_,
      "Result Depth/Height Unit" = NA_character_,
      "Result Status ID" = "Final",
      "Result Value Type" = "Actual",
      "Analysis Start Date" = format(lubridate::as_date(dateTime), "%Y/%m/%d"),
      "Analysis Start Time" = format(dateTime, "%H:%M:%S"),
      "Analysis Start Time Zone" = "PST",
      "Result Comment" = NA_character_
    )
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
        format(min(lubridate::as_date(dateTime)), "%Y-%m"),
        format(max(lubridate::as_date(dateTime)), "%Y-%m"),
        "Water",
        "FM",
        sep = ":"),
      "Activity Type" = "Field Msr/Obs", # data logger
      "Activity Media Name" = "Water",
      "Activity Start Date" = format(min(lubridate::as_date(dateTime)), "%Y/%m/%d"), # need to obtain from data
      "Activity End Date" = format(max(lubridate::as_date(dateTime)), "%Y/%m/%d"), # need to obtain from data
      "Sample Collection Method ID" = "DCR-QAPP",
      "Sample Collection Equipment Name" = "Probe/Sensor",
      "Sample Collection Equipment Comment" = metadata$type, # get this from the metadata
      "Characteristic Name" = "Temperature, water",
      "Result Value" = temperature_f, # from the data
      "Result Unit" = "deg F", # from data
      "Result Depth/Height Measure" = depth_ft,
      "Result Depth/Height Unit" = "ft",
      "Result Status ID" = "Final",
      "Result Value Type" = "Actual",
      "Analysis Start Date" = format(lubridate::as_date(dateTime), "%Y/%m/%d"),
      "Analysis Start Time" = format(dateTime, "%H:%M:%S"),
      "Analysis Start Time Zone" = "PST",
      "Result Comment" = NA_character_
    )
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

