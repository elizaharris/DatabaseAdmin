% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adminfunctions.R
\name{fixOldMetadata}
\alias{fixOldMetadata}
\title{Function to unify the names between the old and new micromet}
\usage{
fixOldMetadata()
}
\description{
This function gets the names from the adminstuff metadata table and unifies them from the old
micromet data so that they match the new micromet metadata scheme
\enumerate{
\item create the x_micromet table with the unedited data
\item split into a data and an uncertainty (std) table
\item unify the data table names with the naming scheme for the newer metadata
\item add to the database
}
}
