% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-5-normalize.R
\name{boxcox_transform}
\alias{boxcox_transform}
\title{Box-Cox Transformation for Positive Data}
\usage{
boxcox_transform(x)
}
\arguments{
\item{x}{A numeric vector. Values must be positive and non-missing for transformation. Missing or non-positive values will be ignored in lambda estimation.}
}
\value{
A list containing:
\describe{
\item{scaled_data}{A numeric vector of transformed data, with the same length as \code{x}.}
\item{normalize_method}{A character string indicating the method used: \code{"boxcox_transform"}.}
\item{normalize_info}{A list containing the lambda used in the Box-Cox transformation.}
}
}
\description{
Applies a Box-Cox transformation to a numeric vector to stabilize variance and make the data more normally distributed.
If the Box-Cox transformation fails or the optimal lambda is close to 0, a log transformation is applied instead.
}
\examples{
set.seed(123)
x <- abs(rnorm(100))
result <- boxcox_transform(x)
}
