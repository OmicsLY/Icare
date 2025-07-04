% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-11-filtered.R
\name{FilterDataFeatures}
\alias{FilterDataFeatures}
\title{Filter and Subset Data Features}
\usage{
FilterDataFeatures(
  object,
  feature_subset_name = "best_features_subset",
  group_col = "group",
  data_type = "clean",
  use_feature_subset = TRUE,
  save_data = TRUE,
  train_filename = "train_data.csv",
  test_filename = "test_data.csv",
  val_filename = "val_data.csv",
  ext_val_filename = "ext_val_data.csv",
  save_dir = here::here("ModelData", "Data")
)
}
\arguments{
\item{object}{A \code{Train_Model} object containing the datasets to be filtered}

\item{feature_subset_name}{Character name of the feature subset to use (default: "best_features_subset").
Must correspond to a named list element in object@feature.result}

\item{group_col}{Character name of the grouping/response variable column (default: "group")}

\item{data_type}{Character specifying data type to use: "clean" for raw data or "scale" for
standardized data (default: "clean")}

\item{use_feature_subset}{Logical indicating whether to filter using feature subset (default: TRUE)}

\item{save_data}{Logical indicating whether to save filtered data to CSV files (default: TRUE)}

\item{train_filename}{Character filename for training data CSV output (default: "train_data.csv")}

\item{test_filename}{Character filename for test data CSV output (default: "test_data.csv")}

\item{val_filename}{Character filename for validation data CSV output (default: "val_data.csv")}

\item{ext_val_filename}{Character filename for external validation data CSV output (default: "ext_val_data.csv")}

\item{save_dir}{Character directory path where CSV files will be saved
(default: here::here("ModelData", "Data"))}
}
\value{
Returns the modified \code{Train_Model} object with filtered datasets stored in the
\verb{@filtered.set} slot. When \code{save_data=TRUE}, also saves CSV files to disk.
}
\description{
Filters datasets (training, testing, validation) based on selected feature subsets and optionally
exports the filtered data to CSV files. Works with objects of class 'Train_Model'.
}
\details{
This function performs the following operations:
\enumerate{
\item Validates input object and data structure
\item Extracts datasets based on specified data_type ("clean" or "scale")
\item Optionally filters datasets using specified feature subset
\item Ensures grouping variable is included in filtered data
\optionally saves filtered datasets as CSV files
\item Updates object@filtered.set with filtered datasets
}
}
\section{File Output}{

When \code{save_data=TRUE}, the function saves these files (if datasets exist):
\itemize{
\item Training data (train_data.csv)
\item Testing data (test_data.csv)
\item Validation data (val_data.csv)
\item External validation data (ext_val_data.csv)
}
}

\examples{
\dontrun{
# Custom parameters example
object_model <- FilterDataFeatures(
  object = object_model,
  feature_subset_name = "selected_features",
  data_type = "scale",
  save_dir = "output/filtered_data"
)
}

}
