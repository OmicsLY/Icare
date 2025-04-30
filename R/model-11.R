#' Extract and Plot Feature Importance from the Best Model
#'
#' This function extracts feature importance from a trained classification model, ranks the top features,
#' and visualizes them in a horizontal bar chart. Optionally, the plot can be saved to a PDF file.
#' @import methods
#' @import stats
#' @import here 
#' @import caret
#' @param best_model A trained model object supported by the `varImp` function (e.g., models trained via the `caret` package).
#' @param top_n Integer. Number of top features to display. If `NULL` or greater than the number of features, all features will be displayed. Default is `15`.
#' @param palette_name Character. Name of the color palette to use for the bar chart (from the `wesanderson` package). Default is `"AsteroidCity1"`.
#' @param save_plots Logical. Whether to save the plot to a PDF file. Default is `TRUE`.
#' @param save_dir Character. Path to the directory where the plot will be saved. Default is `here("ModelData", "best_model_result")`.
#' @param plot_width Numeric. Width of the saved plot in inches. Default is `5`.
#' @param plot_height Numeric. Height of the saved plot in inches. Default is `5`.
#' @param base_size Numeric. Base font size for the plot theme. Default is `14`.
#' @param seed Integer. Random seed for reproducibility. Default is `123`.
#'
#' @returns A `ggplot` object representing the feature importance plot.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming `model` is a trained model object:
#' get_feature_importance(model)
#' }

get_feature_importance  <- function(
    best_model,
    top_n = 15,
    palette_name = "AsteroidCity1",
    save_plots = TRUE,
    save_dir =  here("ModelData", "best_model_result"),
    plot_width = 5,
    plot_height = 5,
    base_size = 14,
    seed =123

) {

  set.seed(seed)

  feature_importance <- varImp(best_model)
  importance_df <- as.data.frame(feature_importance$importance)

  if (is.null(rownames(importance_df))) {
    stop("Row names are missing in the importance dataframe. Ensure that feature names are set.")
  }

  importance_df <- data.frame(Importance = importance_df[, 1])
  rownames(importance_df) <- rownames(feature_importance$importance)

  sorted_importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), , drop = FALSE]
  bar_colors <-as.vector(wes_palette(palette_name))

  if (is.null(top_n)) {
    top_n <- nrow(sorted_importance_df)
  } else if (top_n > nrow(sorted_importance_df)) {
    warning("top_n is greater than the number of features, displaying all features\n")
    top_n <- nrow(sorted_importance_df)
  }

  top_importance <- sorted_importance_df$Importance[1:top_n]
  feature_names <- rownames(sorted_importance_df)[1:top_n]

  feature_names <- as.character(feature_names)

  cat("Top features:\n")
  print(feature_names)
  cat("Top importance values:\n")
  print(top_importance)


  p <- ggplot(data = data.frame(Feature = feature_names, Importance = top_importance), aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = rep(bar_colors, length.out = top_n)) +
    labs(x = "Features", y = "Importance", title = paste("Top", top_n, "Feature Importance")) +
    theme_prism(base_size = base_size) +
    coord_flip()

  print(p)

  if (save_plots) {
    ggsave(filename = file.path(save_dir, "Feature_Importance.pdf"),
           plot = p,
           width = plot_width,
           height = plot_height,
           device = "pdf")
    cat("Plot saved to:", file.path(save_dir, "Feature_Importance.pdf"), "\n")
  }

  return(p)
}

#' Extract the Best Model from a Model_data Object
#'
#' This function extracts the best trained model stored within a `Model_data` object.
#' It safely retrieves the model from the `@best.model.result$model` slot using `tryCatch`
#' to handle any potential errors during extraction.
#'
#' @param object An object of class `Model_data` that contains the best model results in the `@best.model.result` slot.
#'
#' @returns The extracted best model object if successful; otherwise, returns `NULL` and prints an error message.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming `model_obj` is a Model_data object:
#' best_model <- ExtracBest_model(model_obj)
#' }

ExtracBest_model <- function(object) {
  best_model <- tryCatch(object@best.model.result$model,
                         error = function(e) {
                           cat("Error extracting best model:", e$message, "\n")
                           return(NULL)
                         })
  return(best_model)
}

#' Generate and Visualize Feature Importance for the Best Model
#'
#' This function calculates and visualizes the feature importance of the best model
#' stored within a `Model_data` object or from a model object directly. It supports
#' saving the plot and storing the result in the object for further use.
#'
#' @import methods
#' @import stats
#' @import here 
#' @import caret
#' @param object An object of class `Model_data`, or a trained model object directly (e.g., from caret or randomForest).
#' @param top_n Integer specifying the number of top features to display. Default is 15.
#' @param palette_name Character string specifying the color palette name from `wesanderson`. Default is `"AsteroidCity1"`.
#' @param save_plots Logical indicating whether to save the plot to file. Default is `TRUE`.
#' @param save_dir Directory path where plots will be saved. Default is `here("ModelData", "best_model_result")`.
#' @param plot_width Width of the saved plot in inches. Default is 5.
#' @param plot_height Height of the saved plot in inches. Default is 5.
#' @param base_size Base font size for the plot theme. Default is 14.
#'
#' @returns If `object` is of class `Model_data`, returns the updated object with a feature importance plot stored in the `@best.model.result$top_features_plot` slot.
#' If a model object is provided directly, returns a `ggplot` object of the importance plot.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # If `model_data` is a Model_data object
#' updated_object <- FeatureImportance(model_data)
#'
#' # If `rf_model` is a trained random forest model
#' FeatureImportance(rf_model, top_n = 10, save_plots = FALSE)
#' }

FeatureImportance <- function(object,
                              top_n = 15,
                              palette_name = "AsteroidCity1",
                              save_plots = TRUE,
                              save_dir =  here("ModelData", "best_model_result"),
                              plot_width = 5,
                              plot_height = 5,
                              base_size = 14) {
  if (is.null(object)) {
    stop("Invalid input: 'object' should be provided")
  }

  if (inherits(object, "Model_data")) {
    cat("Input is of class 'Model_data'. Extracting the best model...\n")

    best_model <- ExtracBest_model(object)

    if (is.null(best_model)) {
      stop("Invalid input: 'object' should contain a valid model")
    }

    cat("Calculating feature importance for the best model extracted from 'Model_data'...\n")
    result <- get_feature_importance(best_model,
                                     top_n = top_n,
                                     palette_name = palette_name,
                                     save_plots = save_plots,
                                     save_dir =  save_dir,
                                     plot_width = plot_width,
                                     plot_height = plot_height,
                                     base_size = base_size)

    if (!"best.model.result" %in% slotNames(object)) {
      stop("'Model_data' object does not have 'best.model.result' slot.")
    }
    object@best.model.result[["top_features_plot"]] <- result
    cat("Updating 'Model_data' object...\n")
    cat("The 'Model_data' object has been updated with the following slots:\n")
    cat("- 'best.model.result' slot updated.\n")
    return(object)

  } else {
    cat("Object is provided directly. Calculating feature importance...\n")
    result <- FeatureImportance(object, top_n = top_n)
    cat("Feature importance calculated for the provided model.\n")
    return(result)
  }
}
