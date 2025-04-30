#' Convert Factors to Binary or Dummy Variables
#'
#' This function converts factor or character variables in a data frame to binary or dummy variables. If a factor has exactly two levels, it is converted into a binary variable (0/1). If a factor has more than two levels, it is converted into a set of dummy variables.
#'
#' @import stats
#' @param data A data frame containing the variables to be converted. The function applies the conversion to all columns in the data frame that are factors or characters.
#'
#' @returns A data frame with the factor or character columns converted into binary or dummy variables. The original row names are preserved.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example data frame with factors
#' df <- data.frame(
#'   group = factor(c("A", "B", "A", "B")),
#'   outcome = factor(c("Yes", "No", "Yes", "Yes"))
#' )
#'
#' # Convert factors to binary or dummy variables
#' converted_df <- convert_factors_to_binary(df)
#' }

convert_factors_to_binary <- function(data) {
  cat("Starting conversion of factors to binary or dummy variables... \n")

  original_row_names <- rownames(data)

  data_converted <- data.frame(lapply(data, function(column) {
    if (is.factor(column) || is.character(column)) {
      unique_values <- unique(column)

      if (length(unique_values) == 2) {
        column <- ifelse(column == unique_values[1], 0, 1)
      } else {
        column <- model.matrix(~ column - 1)
      }
    }
    return(column)
  }))


  colnames(data_converted) <- make.names(colnames(data_converted), unique = TRUE)

  rownames(data_converted) <- original_row_names

  cat("Conversion completed. \n")
  return(data_converted)
}

#' Prepare Data by Converting Factors to Dummy Variables
#'
#' This function prepares the input data by transforming factor or character variables into binary or dummy variables. The function can handle either a 'Model_data' object or a standard data frame. If the input is a 'Model_data' object, the function updates its `clean.df` slot with the transformed data. If the input is a data frame, it directly returns the transformed data.
#' @import stats
#' @import methods
#' @param object An object of class 'Model_data' or a data frame. If a 'Model_data' object is provided, the function extracts the data from the 'data.df' slot. If a data frame is provided, it operates directly on the data frame.
#'
#' @returns If the input is a 'Model_data' object, the function returns the updated 'Model_data' object with the 'clean.df' slot containing the transformed data. If the input is a data frame, the function returns the transformed data frame.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example of using with a 'Model_data' object
#' model_data_obj <- CreateModelObject(data = df)
#' prepared_model_data <- PrepareData(model_data_obj)
#'
#' # Example of using with a data frame
#' df <- data.frame(
#'   group = factor(c("A", "B", "A", "B")),
#'   outcome = factor(c("Yes", "No", "Yes", "Yes"))
#' )
#' prepared_data <- PrepareData(df)
#' }
PrepareData <- function(object = NULL) {
  cat("Starting transformation of factors to dummy variables... \n")

  if (inherits(object, 'Model_data')) {
    cat("Input is a 'Model_data' object. Extracting the data slot... \n")
    data <- slot(object, "data.df")
  } else if (is.data.frame(object)) {
    cat("Input is a data frame.  \n")
    data <- object
  } else {
    stop("Input must be an object of class 'Model_data' or a data frame.")
  }

  if (is.null(data) || nrow(data) == 0) {
    stop("No valid data found in the input.")
  }

  data <- convert_factors_to_binary(data)

  if (inherits(object, 'Model_data')) {
    slot(object, "clean.df") <- data
    cat("Updating 'Model_data' object...\n")
    cat("The 'Model_data' object has been updated with the following slots:\n")
    cat("- 'clean.df' slot updated.\n")


    return(object)
  }

  cat("Returning transformed data frame.  \n")
  return(data)
}
