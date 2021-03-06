% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{get_results_detection_condition}
\alias{get_results_detection_condition}
\title{Result Detection Condition}
\usage{
get_results_detection_condition(result)
}
\description{
construct detection condition based on other columns in the data. Rules from
WQX state that if there is a Result in the `Result Value` then this value should not be provided.
In the case when `Result Value` is missing then we must specify `Result Detection Condition`
}
