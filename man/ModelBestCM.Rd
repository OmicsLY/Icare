% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-17-cm.R
\name{ModelBestCM}
\alias{ModelBestCM}
\title{Generate Confusion Matrix for Best Model on Specified Dataset}
\usage{
ModelBestCM(
  object,
  group_col = "group",
  palette_name = "AsteroidCity1",
  save_plots = TRUE,
  save_dir = here("ModelData", "best_cutoff"),
  plot_width = 5,
  plot_height = 5,
  best_threshold = NULL,
  set_type = "train"
)
}
\arguments{
\item{object}{A Best_Model object containing trained models and data}

\item{group_col}{Name of the grouping variable column (default="group")}

\item{palette_name}{Color palette name for plots (default="AsteroidCity1")}

\item{save_plots}{Logical indicating whether to save plots (default=TRUE)}

\item{save_dir}{Directory to save plots (default=here("ModelData","best_model_result"))}

\item{plot_width}{Plot width in inches (default=6)}

\item{plot_height}{Plot height in inches (default=6)}

\item{best_threshold}{Decision threshold (default=NULL will use model's default)}

\item{set_type}{Dataset to use: "train", "test" or "validation" (default="train")}
}
\value{
Updated Best_Model object with confusion matrix results
}
\description{
Generate Confusion Matrix for Best Model on Specified Dataset
}
