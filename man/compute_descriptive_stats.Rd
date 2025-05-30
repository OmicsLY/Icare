% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-2.R
\name{compute_descriptive_stats}
\alias{compute_descriptive_stats}
\title{Compute Descriptive Statistics}
\usage{
compute_descriptive_stats(
  data,
  count_feature = TRUE,
  group_col = "group",
  max_unique_values = 5
)
}
\arguments{
\item{data}{A data frame containing the dataset to analyze.}

\item{count_feature}{A logical value indicating whether to compute counts for categorical variables (default is TRUE).}

\item{group_col}{The column name representing the grouping variable (default is "group").}

\item{max_unique_values}{The maximum number of unique values a variable can have to be considered categorical (default is 5).}
}
\value{
A list containing the following components:
\item{Group_Counts}{The counts of each group if a \code{group_col} is provided.}
\item{Count_Results}{A list of counts for each categorical variable.}
\item{Num_Results}{A list of descriptive statistics for numeric variables, including separate statistics for normal and non-normal distributions.}
\item{Normality_Test}{A list of p-values and normality test results for numeric variables.}
}
\description{
This function computes various descriptive statistics for the input dataset, including counts for categorical variables,
and summary statistics (mean, median, standard deviation, min, max) for numeric variables. It also checks for normality
of numeric variables and computes normality tests (Shapiro-Wilk or Anderson-Darling test), based on the number of unique
values and sample size.
}
\examples{
# Example 1: Compute descriptive statistics for a data frame
result <- compute_descriptive_stats(data = my_data, count_feature = TRUE, group_col = "group")

# Example 2: Compute overall descriptive statistics without considering group
result <- compute_descriptive_stats(data = my_data, count_feature = FALSE)
}
