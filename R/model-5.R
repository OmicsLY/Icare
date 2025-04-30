#' Split Dataset into Training and Testing Sets
#'
#' This function splits a given dataset into training and testing sets based on specified ratios.
#' It supports both simple random sampling and stratified sampling using a grouping column.
#'
#' @import caret
#' @param data A data frame to be split. Must be non-empty.
#' @param train_ratio A numeric value between 0 and 1 indicating the proportion of data to allocate to the training set. Default is 0.7.
#' @param test_ratio A numeric value between 0 and 1 indicating the proportion of data to allocate to the testing set. Default is 0.3.
#' The sum of \code{train_ratio} and \code{test_ratio} must be exactly 1.
#' @param seed An integer used to set the random seed for reproducibility. Default is 123.
#' @param group_col Optional. A character string specifying the name of the column to use for stratified sampling.
#' If \code{NULL}, the data will be randomly split without stratification.
#'
#' @returns A list containing two data frames:
#' \describe{
#'   \item{\code{training}}{The training subset of the data.}
#'   \item{\code{testing}}{The testing subset of the data.}
#' }
#'
#' @export
#'
#' @examples
#' data(iris)
#' split_result <- split_data(iris, train_ratio = 0.8, test_ratio = 0.2, group_col = "Species")
#' head(split_result$training)
#' head(split_result$testing)

split_data <- function(
    data,
    train_ratio = 0.7,
    test_ratio = 0.3,
    seed = 123,
    group_col = NULL
) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    stop("Invalid input: 'data' must be a non-empty data frame.")
  }

  total_ratio <- train_ratio + test_ratio
  if (abs(total_ratio - 1) > .Machine$double.eps ^ 0.5) {
    stop("The sum of train_ratio and test_ratio must be equal to 1.")
  }

  set.seed(seed)

  if (!is.null(group_col)) {
    if (!group_col %in% colnames(data)) {
      stop(paste("Column", group_col, "not found in the data frame."))
    }
    train_index <- createDataPartition(data[[group_col]], p = train_ratio, list = FALSE)
  } else {
    train_index <- sample(nrow(data), size = round(nrow(data) * train_ratio))
  }

  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]

  n_train <- nrow(train_data)
  n_test <- nrow(test_data)
  if (n_train < 1 || n_test < 1) {
    stop("Invalid ratio values: insufficient data for training or testing.")
  }

  cat("Data split completed.\n")
  cat("Training data rows:", n_train, "\n")
  cat("Testing data rows:", n_test, "\n")

  return(list(training = train_data, testing = test_data))
}

#' Split Model Data Object or Data Frame into Training and Testing Sets
#'
#' This function splits either a \code{Model_data} object or a regular data frame into training and testing sets,
#' according to the specified proportions. If a \code{Model_data} object is provided, the function will update its
#' \code{split.data} slot with the resulting subsets.
#'
#' @import methods
#' @import caret
#' @param object An input object. Either a \code{Model_data} S4 object containing the dataset in the \code{clean.df} slot,
#' or a plain data frame.
#' @param train_ratio A numeric value between 0 and 1 specifying the proportion of data to be used for training. Default is 0.7.
#' @param test_ratio A numeric value between 0 and 1 specifying the proportion of data to be used for testing. Default is 0.3.
#' The sum of \code{train_ratio} and \code{test_ratio} must equal 1.
#' @param seed An integer value to set the random seed for reproducibility. Default is 123.
#' @param group_col Optional. A character string specifying the name of the column used for stratified sampling.
#' If \code{NULL}, random sampling without stratification is applied. If a \code{Model_data} object is provided,
#' this parameter is overridden by the object's \code{group_col} slot.
#'
#' @returns
#' If a \code{Model_data} object is supplied, the same object is returned with its \code{split.data} slot updated.
#' Otherwise, returns a list with two data frames:
#' \describe{
#'   \item{\code{training}}{The training subset of the data.}
#'   \item{\code{testing}}{The testing subset of the data.}
#' }
#'
#' @export
#'
#' @examples
#' # Example with a regular data frame
#' split_result <- SplitDatModel(iris, train_ratio = 0.8, test_ratio = 0.2, group_col = "Species")
#'
#' # Example with a Model_data object (assuming `model_obj` is a valid Model_data object)
#' # updated_obj <- SplitDatModel(model_obj)
SplitDatModel <- function(
    object = NULL,
    train_ratio = 0.7,
    test_ratio = 0.3,
    seed = 123,
    group_col = NULL
) {
  if (inherits(object, 'Model_data')) {
    data <- slot(object, "clean.df")
    group_col<-object@group_col
  } else if (is.data.frame(object)) {
    data <- object
  } else {
    stop("Input must be an object of class 'Model_data' or a data frame.")
  }

  if (is.null(data) || nrow(data) == 0) {
    stop("No valid data found in the input.")
  }

  total_ratio <- train_ratio + test_ratio
  if (abs(total_ratio - 1) > .Machine$double.eps ^ 0.5) {
    stop("The sum of train_ratio and test_ratio must be equal to 1.")
  }
  cat("Train ratio:", train_ratio, "Test ratio:", test_ratio, "\n")

  datalist <- split_data(
    data = data,
    train_ratio = train_ratio,
    test_ratio = test_ratio,
    seed = seed,
    group_col = group_col
  )

  if (inherits(object, 'Model_data')) {
    object@split.data <- datalist
    cat("Updating 'Model_data' object...\n")
    cat("The 'Model_data' object has been updated with the following slots:\n")
    cat("- 'split.data' slot updated.\n")

    return(object)
  }

  return(datalist)
}
