#' Generate and Plot Confusion Matrix for the Best Model
#'
#' This function evaluates a classification model on a given testing dataset,
#' computes the confusion matrix, and visualizes it using a heatmap-style plot.
#' It optionally saves the plot to a specified directory.
#'
#'
#' @import caret
#' @import stats
#' @param best_model A trained classification model that supports the `predict()` method. Typically an object returned from training (e.g., via `caret`, `glm`, `randomForest`, etc.).
#' @param testing_data A data frame containing testing observations. Must include all predictor variables required by the model and a column for true labels.
#' @param group_col A character string specifying the name of the column in `testing_data` that contains the true class labels. Default is `"group"`.
#' @param save_plots Logical. If `TRUE`, the confusion matrix plot will be saved as a PDF file. Default is `TRUE`.
#' @param save_dir A character string indicating the path to the directory where the plot should be saved. Default is `here("ModelData", "best_model_result")`.
#' @param plot_width Numeric value specifying the width of the output PDF plot in inches. Default is `6`.
#' @param plot_height Numeric value specifying the height of the output PDF plot in inches. Default is `6`.
#' @param palette_name A character string specifying the color palette name to be used from the `wesanderson` package for the heatmap. Default is `"AsteroidCity1"`.
#' @param seed An integer used to set the random seed for reproducibility. Default is `123`.
#'
#' @returns A list containing:
#' \itemize{
#'   \item `cm_results`: The result of `caret::confusionMatrix()`, containing classification metrics.
#'   \item `cm_plot`: A `ggplot2` object representing the confusion matrix plot.
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' model <- readRDS("trained_model.rds")
#' test_data <- read.csv("test_data.csv")
#' result <- generate_best_model_confusion_matrix(model, test_data, group_col = "group")
#' print(result$cm_results)
#' }

generate_best_model_confusion_matrix <- function(best_model,
                                                 testing_data,
                                                 group_col = "group",
                                                 save_plots = TRUE,
                                                 save_dir = here("ModelData", "best_model_result"),
                                                 plot_width = 6,
                                                 plot_height = 6,
                                                 palette_name = "AsteroidCity1",
                                                 seed =123) {
  set.seed(seed)
  testing_predictions <- predict(best_model, newdata = testing_data, type = "raw")

  testing_predictions <- as.numeric(as.character(factor(testing_predictions,
                                                        levels = c("X0", "X1"),
                                                        labels = c(0, 1))))

  true_labels <- testing_data[[group_col]]

  true_labels <- factor(true_labels)

  testing_predictions <- factor(testing_predictions, levels = levels(true_labels))

  if (!all(levels(testing_predictions) == levels(true_labels))) {
    stop("The levels of predictions and true labels are not consistent.")
  }

  cm <- caret::confusionMatrix(testing_predictions, true_labels)
  selected_colors <-as.vector(wes_palette(palette_name))

  print(cm)

  cm_plot <- ggplot(as.data.frame(cm$table), aes(Reference, Prediction)) +
    geom_tile(aes(fill = Freq), color = "white") +
    geom_text(aes(label = Freq), vjust = 1, size = 5, color = "black", fontface = "bold") +
    scale_fill_gradient(low = selected_colors[1], high = selected_colors[2]) +
    labs(title = "Confusion Matrix",
         x = "True Label",
         y = "Predicted Label",
         subtitle = paste("Accuracy: ", round(cm$overall['Accuracy'], 4),
                          " | Kappa: ", round(cm$overall['Kappa'], 4))) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12, color = "black"),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
      legend.position = "none",
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_line(color = "grey95")
    )

  print(cm_plot)

  if (save_plots) {
    ggsave(filename = file.path(save_dir, "confusion_matrix_plot.pdf"),
           plot = cm_plot, width = plot_width, height = plot_height,
           device = "pdf")
    cat("Plot saved to:", file.path(save_dir, "confusion_matrix_plot.pdf"), "\n")
  }

  return(list(cm_results = cm, cm_plot = cm_plot))
}

#' Generate and Save Confusion Matrix for the Best Model in a `Model_data` Object
#'
#' This function extracts the best classification model from a `Model_data` object,
#' generates the confusion matrix using the test dataset, and updates the object with the result.
#' Optionally, the confusion matrix plot can be saved as a PDF file.
#'
#' @import methods
#' @import stats
#' @import here 
#' @param object An object of class `Model_data`, which should contain a trained model and test dataset.
#' @param group_col A character string specifying the name of the column that contains the true class labels.
#'        This argument is automatically overwritten by the value stored in the `object@group_col` slot. Default is `"group"`.
#' @param palette_name A character string specifying the name of the color palette used for plotting (from `wesanderson` package). Default is `"AsteroidCity1"`.
#' @param save_plots Logical. If `TRUE`, saves the confusion matrix plot as a PDF file. Default is `TRUE`.
#' @param save_dir A character string specifying the directory to save the plot. Default is `here("ModelData", "best_model_result")`.
#' @param plot_width Numeric. Width of the saved plot in inches. Default is `6`.
#' @param plot_height Numeric. Height of the saved plot in inches. Default is `6`.
#'
#' @returns
#' An updated object of class `Model_data` with the confusion matrix result stored in the `best.model.result` slot under the key `"confusion_matrix_results"`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming `model_obj` is an object of class `Model_data`:
#' model_obj <- ModelBestCM(model_obj)
#' print(model_obj@best.model.result[["confusion_matrix_results"]])
#' }
ModelBestCM <- function(object,
                        group_col = "group",
                        palette_name = "AsteroidCity1",
                        save_plots = TRUE,
                        save_dir = here("ModelData", "best_model_result"),
                        plot_width = 6,
                        plot_height = 6) {

  if (inherits(object, "Model_data")) {
    cat("Input is of class 'Model_data'. Extracting datasets...\n")
    group_col <-object@group_col
    best_model <- object@best.model.result[["model"]]

    data_sets <- Extract_filtered.set(object)
    test_data <- data_sets$testing

    cat("Generating confusion matrix for the best model...\n")

    confusion_matrix_results <- generate_best_model_confusion_matrix(
      best_model = best_model,
      testing_data = test_data,
      group_col = group_col,
      save_plots = save_plots,
      save_dir = save_dir,
      plot_width = plot_width,
      plot_height = plot_height,
      palette_name = palette_name
    )

    object@best.model.result[["confusion_matrix_results"]] <- confusion_matrix_results

    cat("Updating 'Model_data' object...\n")
    cat("The 'Model_data' object has been updated with the following slots:\n")
    cat("- 'best.model.result' slot updated with confusion matrix results.\n")

    return(object)

  } else {
    stop("Input must be an object of class 'Model_data'.")
  }
}
