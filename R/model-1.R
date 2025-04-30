#' Model_data S4 Class
#'
#' An S4 class to store cleaned data, grouping information, and various results from the
#' modeling pipeline, including feature selection, model training, and SHAP interpretation.
#'
#' @import methods
#' @slot data.df A `data.frame` containing the original input data.
#' @slot clean.df A `data.frame` containing cleaned and preprocessed data.
#' @slot group_col A column name or identifier indicating group labels.
#' @slot balance.info A `list` storing information about class balancing.
#' @slot split.data A `list` of training/testing data splits.
#' @slot split.sacle.data A `list` of scaled training/testing data.
#' @slot feature.selection A `list` of selected features for modeling.
#' @slot feature.result A `list` containing feature evaluation results.
#' @slot filtered.set A `list` of data or variables filtered by some criteria.
#' @slot train.models A `list` of trained models.
#' @slot all.results A `list` storing evaluation results from all models.
#' @slot best.model.result A `list` containing the best model and its performance.
#' @slot shap.result A `list` of SHAP (SHapley Additive exPlanations) interpretation results.
#'
#' @export
#'
#' @examples
#' model_obj <- new("Model_data")
Model_data <- setClass(
  Class = 'Model_data',
  slots = c(
    data.df = 'data.frame',
    clean.df = 'data.frame',
    group_col = "ANY",
    balance.info = 'list',
    split.data = 'list',
    split.sacle.data = 'list',
    feature.selection = 'list',
    feature.result = 'list',
    filtered.set = 'list',
    train.models = 'list',
    all.results = 'list',
    best.model.result = 'list',
    shap.result = 'list'
  ),
  prototype = list(
    data.df = data.frame(),
    group_col = NULL,
    clean.df = data.frame(),
    balance.info = list(),
    split.data = list(),
    split.sacle.data = list(),
    feature.result = list(),
    filtered.set = list(),
    all.results = list(),
    feature.selection = list(),
    train.models = list(),
    best.model.result = list(),
    shap.result = list()
  )
)

#' Extract Clean Data from Stat Object
#'
#' This function extracts the `clean.data` slot from an object of class `Stat`.
#'
#' @param object An object of class `Stat`.
#'
#' @return A `data.frame` containing the cleaned data from the `Stat` object,
#' or `NULL` if extraction fails.
#'
#' @export
#'
#' @examples
#' clean_df <- ExtractCleanData(my_stat_object)

ExtractCleanData <- function(object) {
  if (!inherits(object, "Stat")) {
    stop("The input object must be of class 'Stat'.")
  }

  data <- tryCatch(
    slot(object, "clean.data"),
    error = function(e) {
      warning("Error extracting 'clean.data' from the object: ", e$message)
      return(NULL)
    }
  )
  return(data)
}

#' Create a Model_data Object
#'
#' This function creates a `Model_data` S4 object based on either a raw data frame or a preprocessed `Stat` object. It is designed to provide a structured object for subsequent modeling analyses.
#'
#' @param data A data.frame containing raw input data. It must not contain missing values. If this parameter is provided, the `object` parameter should be NULL.
#' @param object A `Stat` class object containing preprocessed data. If this parameter is provided, the function will extract `clean.data` and `group_col` from it.
#' @param group_col The column name for the grouping variable. If the input is a `Stat` object, this value will be extracted from the object.
#' @param ... Additional parameters for future extensions (currently unused).
#'
#' @returns A `Model_data` S4 object, which contains the data and grouping information for further modeling.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a model object from a data frame
#' df <- data.frame(a = c(1, 2), b = c(3, 4))
#' model_obj <- CreateModelObject(data = df, group_col = "a")
#'
#' # Create a model object from a Stat object (assuming a Stat object exists)
#' model_obj <- CreateModelObject(object = stat_obj)
#' }
CreateModelObject <- function(
    data = NULL,
    object = NULL,
    group_col =NULL,
    ...
) {
  if (!is.null(data) && !is.null(object)) {
    stop("Only one of 'data' and 'object' should be provided.")
  }

  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop("The 'data' parameter must be a data frame.")
    }

    if (any(is.na(data))) {
      stop("The data frame contains missing values. Please handle them before proceeding.")
    }

    data.df <- data
  } else if (!is.null(object)) {
    if (!inherits(object, "Stat")) {
      stop("The 'object' parameter must be an instance of class 'Stat'.")
    }
    data.df <- ExtractCleanData(object = object)
    group_col <- object@group_col
    if (is.null(data.df)) {
      stop("Failed to extract clean data from the provided 'Stat' object.")
    }

    if (any(is.na(data.df))) {
      stop("The extracted data frame contains missing values. Please handle them before proceeding.")
    }
  } else {
    stop("At least one of 'data' and 'object' must be provided.")
  }

  model_data_instance <- new(
    'Model_data',
    data.df = data.df,
    group_col =group_col
  )
  cat("Model object created.\n")
  return(model_data_instance)
}
