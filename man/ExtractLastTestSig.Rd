% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-4.R
\name{ExtractLastTestSig}
\alias{ExtractLastTestSig}
\title{Extract Last Significant Test Results}
\usage{
ExtractLastTestSig(object)
}
\arguments{
\item{object}{An object of class \code{Stat}. The function attempts to extract the \code{last_test_sig} from the \code{var.result} slot.}
}
\value{
Returns the last significant test results stored in the \code{last_test_sig} slot of the \code{var.result} list.
If the \code{last_test_sig} is not found, it returns \code{NULL}.
}
\description{
This function extracts the last significant test results stored in the \code{var.result} slot of a \code{Stat} object. If
the \code{last_test_sig} result is not available, it returns \code{NULL}.
}
\examples{
# Example of extracting the last significant test results from a 'Stat' object
last_sig <- ExtractLastTestSig(stat_object)
print(last_sig)
}
