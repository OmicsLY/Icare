% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-5-normalize.R
\name{log_transform}
\alias{log_transform}
\title{Logarithmic Transformation of Data}
\usage{
log_transform(x)
}
\arguments{
\item{x}{A numeric vector or matrix containing non-negative values to be transformed.
Negative values will cause an error.}
}
\value{
A list containing three components:
\item{scaled_data}{The log-transformed data (log(x + 1e-8))}
\item{normalize_method}{A character string indicating the transformation method ("log_transform")}
\item{normalize_info}{A list containing transformation details including the offset value used (1e-8)}
}
\description{
Applies a logarithmic transformation to the input data with a small offset to handle zeros.
This transformation is commonly used to stabilize variance and make the data more normally distributed.
}
\examples{
# Basic usage with positive values
x <- c(1, 10, 100, 1000)
result <- log_transform(x)
print(result$scaled_data)

# Handling zeros
y <- c(0, 1, 10, 100)
result <- log_transform(y)

\dontrun{
# Will throw an error due to negative values
z <- c(-1, 0, 1, 2)
log_transform(z)
}
}
