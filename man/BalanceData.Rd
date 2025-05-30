% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-3.R
\name{BalanceData}
\alias{BalanceData}
\title{Balance and visualize data distribution with updates to Model_data object.}
\usage{
BalanceData(
  object,
  group_col = "group",
  method = "both",
  N = NULL,
  palette_name = "Royal1",
  seed = 123,
  imbalance_threshold = 0.15,
  sample_size_threshold = 1500,
  force_balance = FALSE,
  save_plots = TRUE,
  save_dir = here("ModelData", "balacing_info"),
  plot_width = 5,
  plot_height = 5,
  base_size = 14
)
}
\arguments{
\item{object}{An object of class \code{Model_data} or a data frame. If a \code{Model_data} object is provided, the function operates on the \code{clean.df} slot.}

\item{group_col}{A string specifying the column name that represents the class labels.}

\item{method}{A string specifying the balancing method. Can be "over", "under", or "both". Default is "both".}

\item{N}{A numeric value specifying the number of samples to generate after balancing. If \code{NULL}, it is automatically calculated based on the chosen method.}

\item{palette_name}{A string specifying the color palette name for the plot. Default is "Royal1".}

\item{seed}{An integer to set the random seed for reproducibility. Default is 123.}

\item{imbalance_threshold}{A numeric value representing the threshold for class imbalance. If the class ratio is below this threshold, balancing is triggered. Default is 0.15.}

\item{sample_size_threshold}{A numeric value representing the threshold for sample size. If the number of samples is below this threshold, balancing is triggered. Default is 1500.}

\item{force_balance}{A logical value indicating whether to force balancing regardless of the class distribution. Default is \code{FALSE}.}

\item{save_plots}{A logical value indicating whether to save the plot as a PDF. Default is \code{TRUE}.}

\item{save_dir}{A string specifying the directory where the plot will be saved. Default is "ModelData/balacing_info" using \code{here()}.}

\item{plot_width}{A numeric value specifying the width of the plot when saved. Default is 5.}

\item{plot_height}{A numeric value specifying the height of the plot when saved. Default is 5.}

\item{base_size}{An integer specifying the base font size for the plot. Default is 14.}
}
\value{
If a \code{Model_data} object is provided, the updated object with the balanced data and balance information slots updated. If a data frame is provided, a list containing the balanced data and visualization results.
}
\description{
This function detects class imbalance in a dataset and balances it using specified methods. It also visualizes the class distribution before and after balancing. The function can handle both \code{Model_data} objects and data frames. If a \code{Model_data} object is provided, the balanced data and associated information are stored back into the object.
}
\examples{
# Balance data from a Model_data object
updated_model <- BalanceData(object = model_data_object, group_col = "group")

# Balance data from a data frame
results <- BalanceData(object = dataset, group_col = "group")
}
