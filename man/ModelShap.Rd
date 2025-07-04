% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-16-shap.R
\name{ModelShap}
\alias{ModelShap}
\title{Generate and Store SHAP Visualizations for a Best_Model Object}
\usage{
ModelShap(
  object,
  group_col = "group",
  palette_name = "AsteroidCity1",
  save_plots = TRUE,
  save_dir = here("ModelData", "best_model_result"),
  plot_width = 5,
  plot_height = 5,
  base_size = 14,
  seed = 123
)
}
\arguments{
\item{object}{An object of class \code{Best_Model}, typically generated by the Icare package. Must contain training models and result metrics.}

\item{group_col}{Character. The name of the column in the dataset that contains group labels. This is usually set automatically from the object. Default is \code{"group"}.}

\item{palette_name}{Character. Name of the color palette used in visualizations (from \code{wesanderson} package). Default is \code{"AsteroidCity1"}.}

\item{save_plots}{Logical. Whether to save the generated plots as PDF files. Default is \code{TRUE}.}

\item{save_dir}{Character. Directory path where the plots will be saved. Default is \code{here("ModelData", "best_model_result")}.}

\item{plot_width}{Numeric. Width of each plot in inches. Default is 5.}

\item{plot_height}{Numeric. Height of each plot in inches. Default is 5.}

\item{base_size}{Numeric. Base font size used in the plots. Default is 14.}

\item{seed}{Integer. Random seed for reproducibility. Default is 123.}
}
\value{
The modified \code{Best_Model} object with SHAP visualizations stored in the \code{shap.result} slot.
}
\description{
This function extracts the best-performing model from a \code{Best_Model} object,
computes SHAP values on the training dataset, generates three SHAP plots (beeswarm, force, and waterfall),
and stores the results back into the \code{shap.result} slot of the \code{Best_Model} object.
}
\examples{
\dontrun{
data("object_model")
object_model <- ModelShap(object = object_model)
object_model@shap.result$beeswarm_plot
}
}
