# script creates the connection between the R
# session and the local database

#' @title Connect to database
#' @description connect the current r sesssion to the local
#' ms express wqx database
#' @export
connect <- function() {
  DBI::dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "localhost\\SQLEXPRESS",
                 Database = "wqx",
                 Trusted_Connection = "True")
}

#' @title Append Data
#' @description add data to an existing table on the database
#' @param db_connection a connection the local database
#' @param table_name name of the table to append to
#' @param data data to add
#' @export
append_data <- function(db_connection,
                        table_name,
                        data) {
    DBI::dbAppendTable(conn = db_connection,
                                name = table_name,
                                value = data)
}

#' @title Update Data
#' @description
update_data <- function(db_connection, table_name, data) {

  # TODO this should be simplified using glue
  q_template <-
    switch(table_name,
           "monitoring_locations" = 'UPDATE monitoring_locations SET "Monitoring Location Name" = ?, "Monitoring Location Type" = ?, "HUC 8" = ?, "Tribal Land Indicator" = ?, "Tribal Land Name" = ?, "Monitoring Location Latitude" = ?, "Monitoring Location Longitude" = ?, "Monitoring Location Horizontal Collection Method" = ?, "Monitoring Location Horizontal Reference Datum" = ?, "Monitoring Location State" = ?, "Monitoring Location County" = ? WHERE "Monitoring Location ID" = ?',
           "results"= "",
           "projects" = "")

  if (!validate_col_types(data, table_name)) {
    stop("column types of new data do not match with database",
         call. = FALSE)
  }

  update_q <- DBI::dbSendQuery(conn = db_connection, statement = q_template)
  DBI::dbBind(update_q, data)
  DBI::dbClearResult(update_q)

}



results_table <- function(db_connection) {
  dplyr::tbl(db_connection, "results")
}

monitoring_locations_table <- function(db_connection) {
  dplyr::tbl(db_connection, "monitoring_locations")
}

projects_table <- function(db_connection) {
  dplyr::tbl(db_connection, "projects")
}

validate_col_types <- function(data, table_name) {
  col_types_should_be <-
    switch(table_name,
    "monitoring_locations" = c("character", "character", "character", "character", "character",
                               "character", "numeric", "numeric", "character", "character",
                               "character", "character"),
    "results" = "",
    "projcets" = "")

  col_types_are <- as.character(sapply(data, class))

  all(col_types_are == col_types_should_be)
}
