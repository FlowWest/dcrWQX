% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wqx.R
\name{wqx_csv}
\alias{wqx_csv}
\alias{wqx_csv_results}
\title{WQX CSV}
\usage{
wqx_csv_results(db_connection, location, characteristic, file_path)
}
\arguments{
\item{db_connection}{a connection to the database}

\item{location}{location to produce csv for}

\item{characteristic}{name to produce csv for}

\item{file_path}{a file path including name to write to the csv to}
}
\description{
Create a csv to be uploaded to the WQX
}
