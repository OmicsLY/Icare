% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-19-cutoff.R
\name{visualize_youden}
\alias{visualize_youden}
\title{Visualize ROC Curve with Youden's Optimal Threshold}
\usage{
visualize_youden(
  true_labels,
  pred_prob,
  positive_class = NULL,
  palette_name = "AsteroidCity1",
  base_size = 14,
  save_plots = TRUE,
  save_dir = here::here("ModelData", "best_cutoff"),
  plot_width = 5,
  plot_height = 5,
  target_value = NULL
)
}
\arguments{
\item{true_labels}{A vector of true class labels (factor or character)}

\item{pred_prob}{A vector of predicted probabilities for the positive class}

\item{positive_class}{The class to consider as positive (defaults to first level if NULL)}

\item{palette_name}{Name of Wes Anderson color palette to use (default: "AsteroidCity1")}

\item{base_size}{Base font size for the plot (default: 14)}

\item{save_plots}{Logical indicating whether to save the plot (default: TRUE)}

\item{save_dir}{Directory path to save plots (default: here("ModelData", "best_cutoff"))}

\item{plot_width}{Width of the saved plot in inches (default: 5)}

\item{plot_height}{Height of the saved plot in inches (default: 5)}

\item{target_value}{Optional target value for Youden's Index (currently unused)}
}
\value{
A list containing:
\itemize{
\item plot - The ggplot object showing ROC curve with optimal point
\item best_threshold - The optimal probability threshold
\item sensitivity - Sensitivity at optimal threshold
\item specificity - Specificity at optimal threshold
\item youden_index - Youden's Index value at optimal threshold
\item roc_object - The full ROC curve object from pROC
}
}
\description{
This function creates a ROC curve plot and identifies the optimal threshold using
Youden's Index (J = Sensitivity + Specificity - 1). The optimal threshold maximizes
the difference between true positive rate and false positive rate.
}
\details{
The function calculates the following:
\itemize{
\item ROC curve using pROC package
\item Optimal threshold using Youden's Index (maximizes Sensitivity + Specificity - 1)
\item Plots ROC curve with optimal point marked
\item Includes performance metrics in plot annotation
}
}
\examples{
\dontrun{
# Create sample data
set.seed(123)
data <- data.frame(
  true = factor(sample(c("Disease", "Healthy"), 100, replace = TRUE, 
                      prob = c(0.3, 0.7))),
  prob = runif(100)
)

# Generate ROC plot with Youden's optimal threshold
result <- visualize_youden(
  true_labels = data$true,
  pred_prob = data$prob,
  positive_class = "Disease"
)

# Access results
print(result$best_threshold)
print(result$youden_index)

# View plot
print(result$plot)
}
}
