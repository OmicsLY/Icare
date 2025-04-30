#' Clinical Prediction Using a Trained Model_data Object
#'
#' Applies a trained model from a `Model_data` object to a new clinical dataset, performs classification based on predicted probabilities,
#' and visualizes the results using boxplots and jitter plots. Automatically handles missing features and factor conversion based on training data.
#'
#' @import ggplot2
#' @import wesanderson
#' @import methods
#' @import stats
#' @param object An object of class `Model_data` containing the best model and training information.
#' @param new_data A `data.frame` or tibble containing new clinical samples to be predicted. Column names should match model features.
#' @param group_col Character. Column name used for grouping in the output plot (optional, not directly used in this function). Default is `"group"`.
#' @param palette_name Character. Name of the color palette used for the visualization. Default is `"Royal1"` from the `wesanderson` package.
#' @param save_dir Character. Path to the directory where the prediction plot will be saved. Default is `here("ModelData", "clinical_predictions")`.
#' @param plot_width Numeric. Width of the saved plot in inches. Default is 6.
#' @param plot_height Numeric. Height of the saved plot in inches. Default is 6.
#' @param alpha Numeric. Transparency level for the jitter points in the plot. Default is 1.
#' @param base_size Numeric. Base font size used in the plot. Default is 14.
#' @param best_threshold Numeric. Optional. Threshold for classifying predicted probabilities. If NULL, it will be extracted from the model. Default is `NULL`.
#'
#' @returns A list containing:
#' \item{predictions}{A data frame with sample names, predicted class labels, and predicted probabilities.}
#' \item{best_threshold}{The threshold used for classification.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data("example_model_data")
#' new_clinical_data <- read.csv("new_patient_data.csv")
#' prediction_result <- ModelClinicalPrediction(
#'   object = example_model_data,
#'   new_data = new_clinical_data
#' )
#' head(prediction_result$predictions)
#' }

ModelClinicalPrediction <- function(object,
                                    new_data,
                                    group_col = "group",
                                    palette_name = "Royal1",
                                    save_dir = here("ModelData", "clinical_predictions"),
                                    plot_width = 6,
                                    plot_height = 6,
                                    alpha = 1,
                                    base_size = 14,
                                    best_threshold =NULL) {

  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }

  if (is.null(new_data)) {
    stop("New clinical data is missing.")
  }

  if (!inherits(object, "Model_data")) {
    stop("Input must be an object of class 'Model_data'.")
  }

  cat("Input is of class 'Model_data'. Extracting datasets...\n")
  training_data<- object@filtered.set[["training"]]
  best_model <- object@best.model.result[["model"]]
  if (is.null(best_threshold)) {
    best_threshold <- object@best.model.result[["best_threshold"]]
    if (is.null(best_threshold)) {
      best_threshold <- 0.5
    }
  }
  cat("Best threshold determined:", best_threshold, "\n")

  important_vars <- best_model$coefnames
  missing_vars <- setdiff(important_vars, names(new_data))
  prediction_data <- new_data[, intersect(important_vars, colnames(new_data)), drop = FALSE]

  for (var in missing_vars) {
    if (is.numeric(training_data[[var]])) {
      prediction_data[[var]] <- median(training_data[[var]], na.rm = TRUE)
    }
  }
  for (var in missing_vars) {
    if (is.factor(training_data[[var]])) {
      most_common <- names(which.max(table(training_data[[var]])))
      prediction_data[[var]] <- factor(most_common,
                                       levels = levels(training_data[[var]]))
    }
  }
  prediction_data <- convert_factors_to_binary(prediction_data)

  predictions <- predict(best_model, newdata = prediction_data, type = "prob")[, 2]

  predicted_labels <- ifelse(predictions > best_threshold, 1, 0)

  final_result <- data.frame(
    Sample = rownames(prediction_data),
    Classification = as.factor(ifelse(predicted_labels == 1, "Positive", "Negative")),
    Probability = predictions
  )
  Classification = unique(final_result$Classification)
  colors <- wes_palette(n = length(Classification), name = palette_name, type = "discrete")
  names(colors) <- Classification



  p <- ggplot(final_result, aes(x = Classification, y = Probability, fill = Classification)) +
    geom_boxplot(outlier.shape = 19, outlier.colour = colors[1], outlier.size = 1) +
    geom_jitter(width = 0.2, size = 2, aes(color = Classification), alpha = 0.6) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    labs(title = "Visualization of Predicted Group and Probabilities",
         x = "Group",
         y = "Predicted Probability") +
    theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = base_size + 2),
      axis.text.x = element_text(angle = 45, hjust = 1, size = base_size - 2),
      axis.text.y = element_text(size = base_size - 2),
      axis.title.x = element_text(size = base_size),
      axis.title.y = element_text(size = base_size),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 2)
    )

  print(p)
  ggsave(file.path(save_dir, "prediction_visualization.pdf"), p, width = plot_width,
         height = plot_height,
         device = "pdf")
  cat("Prediction visualization saved to:", file.path(save_dir, "prediction_visualization.pdf"), "\n")

  return(list(
    predictions = final_result,
    best_threshold = best_threshold
  ))
}
