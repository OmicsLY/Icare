#' Extract External Validation Data and Update Model
#'
#' This function extracts an external validation dataset either from a provided data frame or from an existing 'Stat' object,
#' processes it (e.g., handles missing values and converts factors), and updates the provided model object with the validation data.
#'
#' @param data A data frame containing the external validation data. Must not have missing values.
#' @param object_stats An object of class 'Stat' from which clean validation data will be extracted. Must not have missing values.
#' @param object_model A 'Model_data' object that will be updated with the external validation data.
#' @param group_col The column name in the validation data that indicates the group or outcome. (Optional)
#' @param ... Additional parameters that may be passed to other functions (if any).
#'
#' @returns The updated 'Model_data' object, with the 'filtered.set' slot containing the external validation data.
#' @export
#'
#' @examples
#' updated_model <- Extract_external_validata(data = validation_data,
#'                                           object_model = model_object)
Extract_external_validata <- function(
    data = NULL,
    object_stats= NULL,
    object_model = NULL,
    group_col =NULL,
    ...
) {
  if (!is.null(data) && !is.null(object_stats)) {
    stop("Only one of 'data' and 'object_stats' should be provided.")
  }

  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop("The 'data' parameter must be a data frame.")
    }

    if (any(is.na(data))) {
      stop("The data frame contains missing values. Please handle them before proceeding.")
    }

    data.df <- data
  } else if (!is.null(object_stats)) {
    if (!inherits(object_stats, "Stat")) {
      stop("The 'object_stats' parameter must be an instance of class 'Stat'.")
    }
    data.df <- ExtractCleanData(object= object_stats)
    if (is.null(data.df)) {
      stop("Failed to extract clean data from the provided 'Stat' object_stats.")
    }

    if (any(is.na(data.df))) {
      stop("The extracted data frame contains missing values. Please handle them before proceeding.")
    }
  } else {
    stop("At least one of 'data' and 'object_stats' must be provided.")
  }

  data.df <- convert_factors_to_binary(data.df)
  object_model@filtered.set[["validation"]]<-data.df
  cat("The independent validation set has been added.\n")
  cat("Updating 'Model_data' object...\n")
  cat("The 'Model_data' object has been updated with the following slots:\n")
  cat("- 'filtered.set' slot updated.\n")
  return(object_model)
}


#' Plot ROC Curve for External Validation Data
#'
#' This function generates and optionally saves a ROC curve for an external validation dataset
#' using the best-performing classification model. It calculates the AUC and its confidence interval,
#' and produces a publication-ready ROC plot.
#'
#' @importFrom pROC roc auc ci.auc
#' @param best_model A trained classification model object that supports probability prediction via `predict(..., type = "prob")`.
#' @param validation_data A data frame containing the validation dataset. Must include the true class labels in `group_col`.
#' @param group_col A string specifying the name of the column in `validation_data` that contains the true class labels. Default is "group".
#' @param palette_name A string specifying the color palette name used for plotting. Must be a valid palette in `wesanderson::wes_palette()`. Default is "AsteroidCity1".
#' @param base_size A numeric value controlling the base font size for the plot. Default is 14.
#' @param save_plots Logical; whether to save the plot as a PDF file. Default is TRUE.
#' @param save_dir A character string specifying the directory path where the plot should be saved. Default is `here("ModelData", "best_model_result")`.
#' @param plot_width Numeric value specifying the width (in inches) of the saved plot. Default is 5.
#' @param plot_height Numeric value specifying the height (in inches) of the saved plot. Default is 5.
#' @param alpha Numeric value (between 0 and 1) indicating the transparency level of the ROC curve line. Default is 1 (fully opaque).
#' @param subtitle A string to be displayed as the subtitle of the ROC plot. Default is "Validation Dataset".
#'
#' @returns A data frame containing the coordinates of the ROC curve for plotting purposes, including specificity, sensitivity, and AUC information.
#' @export
#'
#' @examples
#' # Assuming `model` is a trained classifier and `val_data` is a data frame for validation:
#' plot_validation_model_roc(
#'   best_model = model,
#'   validation_data = val_data,
#'   group_col = "outcome"
#' )
plot_validation_model_roc <- function(best_model,
                                      validation_data,
                                      group_col = "group",
                                      palette_name = "AsteroidCity1",
                                      base_size = 14,
                                      save_plots = TRUE,
                                      save_dir = here("ModelData", "best_model_result"),
                                      plot_width = 5,
                                      plot_height = 5,
                                      alpha = 1,
                                      subtitle = "Validation Dataset") {

  if (is.null(validation_data)) {
    stop("Validation dataset is missing.")
  }

  validation_data[[group_col]] <- as.factor(validation_data[[group_col]])

  validation_predictions <- predict(best_model,
                                    newdata = validation_data, type = "prob")[, 2]

  roc_raw <- roc(validation_data[[group_col]], validation_predictions, levels = c("0", "1"), direction = ">")
  if (auc(roc_raw) < 0.5) {
    cat("Warning: Model predictions are inverted. Reversing prediction probabilities.\n")
    validation_predictions <- 1 - validation_predictions
    roc_validation <- roc(validation_data[[group_col]], validation_predictions, levels = c("0", "1"), direction = ">")
  } else {
    roc_validation <- roc_raw
  }

  auc_validation <- auc(roc_validation)
  auc_ci_validation <- ci.auc(roc_validation)

  validation_plot_data <- data.frame(
    Specificity = 1 - roc_validation$specificities,
    Sensitivity = roc_validation$sensitivities,
    Dataset = paste0("Validation Set (AUC = ", round(auc_validation, 3),
                     ", CI = [", round(auc_ci_validation[1], 3), ", ",
                     round(auc_ci_validation[3], 3), "])")
  )
  validation_plot_data$Specificity <- as.numeric(validation_plot_data$Specificity)
  validation_plot_data$Sensitivity <- as.numeric(validation_plot_data$Sensitivity)
  validation_plot_data$Dataset <- as.factor(validation_plot_data$Dataset)

  p <- ggplot(validation_plot_data, aes(x = Specificity, y = Sensitivity, color = Dataset)) +
    geom_line(size = 1.25, alpha = alpha) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
    scale_color_manual(values = wes_palette(palette_name)) +
    labs(title = "ROC Curve for Best Model",
         subtitle = subtitle,
         x = "1 - Specificity",
         y = "Sensitivity",
         color = "Dataset (AUC and CI)") +
    theme_minimal(base_size = base_size) +
    theme(
      legend.position = c(0.95, 0.05),
      legend.justification = c(1, 0),
      legend.background = element_rect(fill = alpha("white", 0.8)),
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 8),
      panel.grid.major = element_line(color = "grey90")
    )

  print(p)

  if (save_plots) {
    ggsave(filename = file.path(save_dir, "validation_roc_plot.pdf"), plot = p, width = plot_width, height = plot_height,
           device = "pdf")
    cat("Plot saved to:", file.path(save_dir, "validation_roc_plot.pdf"), "\n")
  }

  return(validation_plot_data)
}


#' Validate Best Model on External Dataset and Generate ROC Curve
#'
#' This function takes a `Model_data` object, extracts the best model and validation dataset,
#' preprocesses the data (including imputation of missing predictor variables), evaluates the model performance,
#' generates the ROC curve on the validation data, and updates the original object with the validation results.
#'
#' @import methods
#' @import stats
#' @import here
#' @param object An object of class `Model_data`, containing slots for the filtered training and validation datasets, as well as the best trained model (either as an object or file path).
#' @param group_col A character string specifying the name of the column indicating class labels in the validation dataset. Default is `"group"`.
#' @param palette_name A character string specifying the color palette used in ROC plotting. Default is `"AsteroidCity1"` from the `wesanderson` package.
#' @param base_size Numeric value controlling the base font size for the ROC plot. Default is `14`.
#' @param save_plots Logical. If `TRUE`, saves the generated ROC curve as a PDF file in `save_dir`. Default is `TRUE`.
#' @param save_dir A character string specifying the directory to which the plot should be saved. Default is `here("ModelData", "best_model_result")`.
#' @param plot_width Numeric value specifying the width (in inches) of the saved plot. Default is `5`.
#' @param plot_height Numeric value specifying the height (in inches) of the saved plot. Default is `5`.
#' @param alpha Numeric value (between 0 and 1) specifying the transparency level of the ROC curve. Default is `0.05`.
#'
#' @returns Returns an updated `Model_data` object with validation results added to the `best.model.result` slot. Specifically, it includes:
#' \itemize{
#'   \item `roc_plot`: A data frame with ROC curve data.
#'   \item `validation_result`: The output from the `evaluate_model_performance()` function, typically including metrics like accuracy, sensitivity, specificity, AUC, etc.
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming `model_obj` is a Model_data object with model and validation data:
#' updated_obj <- ModelValidation(model_obj)
#' }
ModelValidation <- function(object,
                            group_col = "group",
                            palette_name = "AsteroidCity1",
                            base_size = 14,
                            save_plots = TRUE,
                            save_dir = here("ModelData", "best_model_result"),
                            plot_width = 5,
                            plot_height = 5,
                            alpha = 0.05) {

  if (inherits(object, "Model_data")) {
    cat("Input is of class 'Model_data'. Extracting datasets...\n")

    validation <- object@filtered.set[["validation"]]
    best_model <- object@best.model.result[["model"]]
    training_data<- object@filtered.set[["training"]]
    if (is.null(validation) || nrow(validation) == 0) {
      stop("Validation dataset is empty or not found.")
    }
    if (is.null(best_model)) {
      stop("Best model not found in the object.")
    }

    if (is.character(best_model)) {
      cat("Loading model from path:", best_model, "\n")
      best_model <- readRDS(best_model)
    }

    important_vars <- best_model$coefnames
    missing_vars <- setdiff(important_vars, names(validation))
    prediction_data <- validation[, intersect(important_vars, colnames(validation)), drop = FALSE]
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

    prediction_data[[group_col]] <- validation[[group_col]]

    cat("Evaluating the best model on the validation dataset...\n")
    validation_result <- evaluate_model_performance(data = prediction_data,
                                                    model_result = best_model,
                                                    group_col = group_col)

    prediction_data$group <- factor(prediction_data$group, levels = c(0, 1))

    cat("Generating ROC curves on validation data...\n")
    roc_results <- plot_validation_model_roc(
      best_model = best_model,
      validation_data = prediction_data,
      subtitle = "Validation Datasets")

    object@best.model.result[["validation_result"]] <- list(roc_plot = roc_results,
                                                            validation_result = validation_result
    )
    cat("Updating 'Model_data' object...\n")
    cat("The 'Model_data' object has been updated with the following slots:\n")
    cat("- 'best.model.result' slot updated.\n")

    return(object)
  } else {
    stop("Input must be an object of class 'Model_data'")
  }
}
