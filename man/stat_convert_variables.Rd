% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-2.R
\name{stat_convert_variables}
\alias{stat_convert_variables}
\title{Convert Variables in a 'Stat' Object or Data Frame}
\usage{
stat_convert_variables(
  object,
  group_col = "group",
  max_unique_values = 5,
  save_dir = here::here("StatObject", "Data"),
  save_data = TRUE,
  csv_filename = "clean_data.csv"
)
}
\arguments{
\item{object}{An object of class 'Stat' or a data frame. If a 'Stat' object is
provided, the function will update its \code{clean.data} slot.}

\item{group_col}{A string representing the column name that groups the data.
Default is "group". This column is used to determine the type of variables.}

\item{max_unique_values}{The maximum number of unique values allowed for a variable
to be considered as numeric. Default is 5.}
}
\value{
If the input is a 'Stat' object, it returns the updated 'Stat' object.
If the input is a data frame, it returns the converted data frame.
}
\description{
This function converts variables in a given object (either of class 'Stat' or a
data frame) to numeric or factor types based on the information provided by
\code{diagnose_variable_type}. If the input is a 'Stat' object, the conversion will
update its \code{clean.data} slot. If the input is a data frame, it will directly
return the converted data frame.
}
\examples{
# Example of usage
data <- data.frame(a = c(1, 2, 3), b = c("low", "medium", "high"))
stat_object <- Stat$new(clean.data = data, group_col = "group")
updated_stat_object <- stat_convert_variables(stat_object)

}
