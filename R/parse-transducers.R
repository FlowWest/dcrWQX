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
                     col_types = col_info$col_types
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

#' @title Transducer Column Info
#' @description based on a transducer's serial number determine
#' the column names and column needed to read in the csv
#' @param serial the serial number for the transducer
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


