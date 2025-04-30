#' Filter Features from a Model Data Object
#'
#' This function filters the training and testing datasets in a 'Model_data' object
#' to retain only the features that are part of the best feature subset. The
#' best feature subset is determined based on the 'feature_subset_name' parameter,
#' which points to a list of selected feature names stored in the 'feature.result'
#' slot of the object.
#'
#' @import methods
#' @param object An object of class 'Model_data'. The object must contain
#'   a 'split.data' slot with 'training' and 'testing' datasets and a
#'   'feature.result' slot containing the best feature subset.
#'
#' @param feature_subset_name A character string specifying the name of the feature
#'   subset within the 'feature.result' slot. Default is "best_features_subset".
#'
#' @param group_col A character string specifying the name of the column that
#'   contains the response variable (group labels). Default is "group".
#'
#' @param data_type A character string indicating the type of data to extract,
#'   either "clean" or "scale". Default is "clean".
#'
#' @returns A 'Model_data' object with an updated 'filtered.set' slot containing
#'   the filtered training and testing datasets. If no valid features are found,
#'   the 'filtered.set' slot will be updated with the full dataset.
#'
#' @export
#'
#' @examples
#' # Example usage of FilterDataFeatures
#' # Assuming 'model_data' is an existing Model_data object
#' filtered_model_data <- FilterDataFeatures(
#'   object = model_data,
#'   feature_subset_name = "best_features_subset",  # Can be adjusted
#'   group_col = "group",  # The response column name
#'   data_type = "clean"   # Choose between "clean" or "scale"
#' )
FilterDataFeatures <- function(
    object,
    feature_subset_name = "best_features_subset",
    group_col = "group",
    data_type = "clean"
) {
  if (!inherits(object, 'Model_data')) {
    stop("Input must be an object of class 'Model_data'.")
  }
  group_col <- object@group_col
  if (!is.list(object@split.data) ||
      !all(c("training", "testing") %in% names(object@split.data))) {
    stop("The 'split.data' slot in the 'Model_data' object must be a list containing 'training' and 'testing' datasets.")
  }

  if (data_type == "scale") {
    cat("Extracting scaled data...\n")
    train_data <- slot(object, "split.sacle.data")[["training"]]
    test_data <- slot(object, "split.sacle.data")[["testing"]]
  } else if (data_type == "clean") {
    cat("Extracting cleaned data...\n")
    train_data <- slot(object, "split.data")[["training"]]
    test_data <- slot(object, "split.data")[["testing"]]
  } else {
    stop("Invalid 'data_type'. Use 'clean' or 'scale'.")
  }
  cat("Data extracted based on data_type:", data_type, "\n")

  if (is.null(train_data) || !is.data.frame(train_data) || nrow(train_data) == 0) {
    stop("No valid training data found in the 'Model_data' object.")
  }

  if (is.null(test_data) || !is.data.frame(test_data) || nrow(test_data) == 0) {
    stop("No valid test data found in the 'Model_data' object.")
  }

  best_features <- object@feature.result[[feature_subset_name]]

  if (is.null(best_features) || length(best_features) == 0) {
    cat(sprintf("No valid feature names found in the 'Model_data' object under '%s'.\n", feature_subset_name))
    cat("Falling back to full dataset based on data_type.\n")
    object@filtered.set <- list(
      training = train_data,
      testing = test_data
    )
    cat("The 'filtered.set' slot has been updated with the full dataset.\n")
    return(object)
  }

  if (group_col %in% names(train_data)) {
    best_features <- unique(c(best_features, group_col))
  } else {
    warning(sprintf("Response column '%s' not found in the training data.", group_col))
  }

  missing_features <- setdiff(best_features, names(train_data))
  if (length(missing_features) > 0) {
    stop(sprintf("The following features are missing from the training data: %s", paste(missing_features, collapse = ", ")))
  }

  filtered_train <- train_data[, best_features, drop = FALSE]
  filtered_test <- test_data[, best_features, drop = FALSE]
  cat("Data filtered to retain best features.\n")

  object@filtered.set <- list(
    training = filtered_train,
    testing = filtered_test
  )
  cat("Updating 'Model_data' object...\n")
  cat("The 'Model_data' object has been updated with the following slots:\n")
  cat("- 'filtered.set' slot updated.\n")

  return(object)
}
