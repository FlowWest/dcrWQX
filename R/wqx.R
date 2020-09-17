# this script handles the communication from the
# local database to the WQX using wqxWeb R package

#' Upload Data to WQX
#' @description function uploads
#' @param file path to file to upload
#' @param type what type of file is this
#' @param config path to config file
#' @param username username
#' @param pk private key
#' @export
upload_data <- function(file, type, config, username, pk) {

  # 1. upload data to the wqx server
  # this will use the wqx_upload() function
  upload_response <- wqxWeb::wqx_upload(file = file, config = config,
                     username = username, pk = pk)

  # 2. push the uploaded dataset into the staging phase
  # this will use the wqx_start_import() function
  upload_response
}


#' WQX CSV
#' @description Create a csv to be uploaded to the WQX
#' @name wqx_csv
NULL

#' @rdname wqx_csv
#' @param db_connection a connection to the database
#' @param location location to produce csv for
#' @param characteristic name to produce csv for
#' @param file_path a file path including name to write to the csv to
#' @export
wqx_csv_results <- function(db_connection, location, characteristic, file_path) {
  raw_data <- db_connection %>%
    tbl("results") %>%
    filter(`Characteristic Name` == characteristic,
            `Monitoring Location ID` == location) %>%
    collect()

  # add a datetime object for easy manipulation and right join
  # to the desired two hour intervals
  results_two_hour <- raw_data %>%
    mutate_if(is.character, trimws) %>%
    mutate(datetime = lubridate::as_datetime(paste(`Analysis Start Date`,
                                                   `Analysis Start Time`))) %>%
    right_join(transducer_subsets_to_keep, by = c("datetime" = "datetime")) %>%
    filter(!is.na(Record_ID)) %>%
    select(-Record_ID, -datetime)

  write_csv(results_two_hour, path = file_path, na = "")

  invisible(results_two_hour)

}
