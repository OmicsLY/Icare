% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-2.R
\name{convert_factors_to_binary}
\alias{convert_factors_to_binary}
\title{Convert Factors to Binary or Dummy Variables}
\usage{
convert_factors_to_binary(data)
}
\arguments{
\item{data}{A data frame containing the variables to be converted. The function applies the conversion to all columns in the data frame that are factors or characters.}
}
\value{
A data frame with the factor or character columns converted into binary or dummy variables. The original row names are preserved.
}
\description{
This function converts factor or character variables in a data frame to binary or dummy variables. If a factor has exactly two levels, it is converted into a binary variable (0/1). If a factor has more than two levels, it is converted into a set of dummy variables.
}
\examples{
\dontrun{
# Example data frame with factors
df <- data.frame(
  group = factor(c("A", "B", "A", "B")),
  outcome = factor(c("Yes", "No", "Yes", "Yes"))
)

# Convert factors to binary or dummy variables
converted_df <- convert_factors_to_binary(df)
}
}
