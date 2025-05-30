% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-8.R
\name{plot_best_model_roc}
\alias{plot_best_model_roc}
\title{Plot ROC Curves for the Best Model and Compare AUCs of Training and Testing Sets}
\usage{
plot_best_model_roc(
  best_model,
  training_data = NULL,
  testing_data = NULL,
  group_col = "group",
  palette_name = "AsteroidCity1",
  base_size = 14,
  save_plots = TRUE,
  save_dir = here("ModelData", "best_model_result"),
  plot_width = 5,
  plot_height = 5,
  alpha = 1,
  subtitle = "Training and Testing Datasets"
)
}
\arguments{
\item{best_model}{The best trained model, typically a classification model.}

\item{training_data}{Training dataset containing features and labels.}

\item{testing_data}{Testing dataset containing features and labels.}

\item{group_col}{The name of the label column, default is "group".}

\item{palette_name}{The name of the color palette, default is "AsteroidCity1".}

\item{base_size}{The base font size for the plot, default is 14.}

\item{save_plots}{Logical value indicating whether to save the plot. Default is TRUE.}

\item{save_dir}{The directory where the plot will be saved, default is "ModelData/best_model_result".}

\item{plot_width}{The width of the plot when saved, default is 5.}

\item{plot_height}{The height of the plot when saved, default is 5.}

\item{alpha}{The transparency of the plot elements, default is 1 (completely opaque).}

\item{subtitle}{The subtitle of the plot, default is "Training and Testing Datasets".}
}
\value{
A list containing the ROC plot data for the training and testing datasets.
}
\description{
This function generates ROC curves for the best model using training and testing data,
and compares the AUC values for both datasets. The ROC curves are visualized and optionally saved as a PDF.
}
\examples{
plot_best_model_roc(best_model = trained_model, training_data = train_data, testing_data = test_data)
}
