% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-5.R
\name{SplitDatModel}
\alias{SplitDatModel}
\title{Split Model Data Object or Data Frame into Training and Testing Sets}
\usage{
SplitDatModel(
  object = NULL,
  train_ratio = 0.7,
  test_ratio = 0.3,
  seed = 123,
  group_col = NULL
)
}
\arguments{
\item{object}{An input object. Either a \code{Model_data} S4 object containing the dataset in the \code{clean.df} slot,
or a plain data frame.}

\item{train_ratio}{A numeric value between 0 and 1 specifying the proportion of data to be used for training. Default is 0.7.}

\item{test_ratio}{A numeric value between 0 and 1 specifying the proportion of data to be used for testing. Default is 0.3.
The sum of \code{train_ratio} and \code{test_ratio} must equal 1.}

\item{seed}{An integer value to set the random seed for reproducibility. Default is 123.}

\item{group_col}{Optional. A character string specifying the name of the column used for stratified sampling.
If \code{NULL}, random sampling without stratification is applied. If a \code{Model_data} object is provided,
this parameter is overridden by the object's \code{group_col} slot.}
}
\value{
If a \code{Model_data} object is supplied, the same object is returned with its \code{split.data} slot updated.
Otherwise, returns a list with two data frames:
\describe{
\item{\code{training}}{The training subset of the data.}
\item{\code{testing}}{The testing subset of the data.}
}
}
\description{
This function splits either a \code{Model_data} object or a regular data frame into training and testing sets,
according to the specified proportions. If a \code{Model_data} object is provided, the function will update its
\code{split.data} slot with the resulting subsets.
}
\examples{
# Example with a regular data frame
split_result <- SplitDatModel(iris, train_ratio = 0.8, test_ratio = 0.2, group_col = "Species")

# Example with a Model_data object (assuming `model_obj` is a valid Model_data object)
# updated_obj <- SplitDatModel(model_obj)
}
