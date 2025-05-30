% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-11.R
\name{ExtracBest_model}
\alias{ExtracBest_model}
\title{Extract the Best Model from a Model_data Object}
\usage{
ExtracBest_model(object)
}
\arguments{
\item{object}{An object of class \code{Model_data} that contains the best model results in the \verb{@best.model.result} slot.}
}
\value{
The extracted best model object if successful; otherwise, returns \code{NULL} and prints an error message.
}
\description{
This function extracts the best trained model stored within a \code{Model_data} object.
It safely retrieves the model from the \verb{@best.model.result$model} slot using \code{tryCatch}
to handle any potential errors during extraction.
}
\examples{
\dontrun{
# Assuming `model_obj` is a Model_data object:
best_model <- ExtracBest_model(model_obj)
}
}
