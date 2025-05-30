% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-2.R
\name{ExtractRawData}
\alias{ExtractRawData}
\title{Extract Raw Data from Stat Object}
\usage{
ExtractRawData(object)
}
\arguments{
\item{object}{An object of class 'Stat' which contains a slot named 'raw.data'.
This should be a valid Stat object.}
}
\value{
Returns the 'raw.data' slot of the Stat object if it exists.
If the object does not have a 'raw.data' slot or the slot is empty,
it returns NULL.
}
\description{
This function extracts the 'raw.data' slot from an object of class 'Stat'.
If the object is not of class 'Stat' or does not contain a 'raw.data' slot,
it will return NULL.
}
\examples{
# Assuming 'stat_object' is a valid Stat object
raw_data <- ExtractRawData(stat_object)

# If the object does not have raw.data, it will return NULL
missing_data <- ExtractRawData(non_stat_object)
}
