#' Detect and Visualize Outliers in Numeric Variables
#'
#' This function identifies outliers in numeric variables using the IQR method (with optional custom ranges),
#' marks them in the dataset, and generates boxplot visualizations. Supports grouping by a categorical variable.
#'
#' @param data Data frame containing the variables to analyze
#' @param group_col Name of grouping variable (factor) for colored boxplots (default: "group")
#' @param palette_name Name of color palette for visualization (default: "Royal1")
#' @param save_plots Logical indicating whether to save plots (default: FALSE)
#' @param save_dir Directory path to save plots (default: here("StatObject"))
#' @param plot_display_num Number of variables to display per plot (default: 1)
#' @param sub_var Optional subset of variables to analyze (default: NULL)
#' @param plot_width Plot width in inches (default: 5)
#' @param plot_height Plot height in inches (default: 5)
#' @param base_size Base font size for plots (default: 14)
#' @param custom_ranges Named list of custom ranges for variables (default: NULL)
#' @param max_unique_values Maximum unique values for numeric variable detection (default: 5)
#'
#' @return A list containing:
#' \itemize{
#'   \item data_marked - Original data frame with added outlier indicators (_outlier columns)
#'   \item plots - List of ggplot boxplot objects
#'   \item normal_ranges - Data frame with IQR and boundary information
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' data(mtcars)
#' results <- detect_and_mark_outliers(mtcars, group_col = "cyl")
#'
#' # With custom ranges
#' custom_ranges <- list(mpg = c(10, 30), hp = c(50, 200))
#' results <- detect_and_mark_outliers(mtcars, custom_ranges = custom_ranges)
#' }
#' @importFrom wesanderson wes_palette
#' @importFrom here here
#' @importFrom dplyr select mutate filter bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom ggprism theme_prism
detect_and_mark_outliers <- function(data,
                                     group_col = "group",
                                     palette_name = "Royal1",
                                     save_plots = FALSE,
                                     save_dir = here("StatObject"),
                                     plot_display_num = 1,
                                     sub_var = NULL,
                                     plot_width = 5,
                                     plot_height = 5,
                                     base_size = 14,
                                     custom_ranges = NULL,
                                     max_unique_values = 5) {
  stopifnot(is.data.frame(data))

  original_rownames <- rownames(data)
  variable_type<-diagnose_variable_type(data)
  numeric_vars<-variable_type$numeric_vars
  numeric_data <- data[,numeric_vars]

  detect_outliers_with_range <- function(x, var_name = NULL) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1

    if (!is.null(custom_ranges) && !is.null(var_name) && var_name %in% names(custom_ranges)) {
      lower_bound <- custom_ranges[[var_name]][1]
      upper_bound <- custom_ranges[[var_name]][2]
      custom_flag <- TRUE
    } else {
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      custom_flag <- FALSE
    }

    outliers <- x < lower_bound | x > upper_bound

    return(list(
      outliers = outliers,
      range_info = data.frame(
        variable = ifelse(is.null(var_name), NA, var_name),
        IQR = IQR,
        lower_bound = lower_bound,
        upper_bound = upper_bound,
        is_custom_range = custom_flag
      )
    ))
  }

  range_info_list <- list()
  outlier_vars <- c()

  for (var in names(numeric_data)) {
    result <- detect_outliers_with_range(numeric_data[[var]], var)
    if (any(result$outliers, na.rm = TRUE)) {
      outlier_vars <- c(outlier_vars, var)
      range_info_list[[var]] <- result$range_info
    }
  }

  if (length(outlier_vars) == 0) {
    cat("No variables with outliers found.")
    return(NULL)
  }

  if (!is.null(sub_var)) {
    outlier_vars <- intersect(outlier_vars, sub_var)
    range_info_list <- range_info_list[outlier_vars]
  }

  range_info_df <- bind_rows(range_info_list)

  cat("Detected outlier variables:", paste(outlier_vars, collapse = ","), "\n")

  for (var in outlier_vars) {
    result <- detect_outliers_with_range(data[[var]], var)
    data <- data %>%
      mutate(!!paste0(var, "_outlier") := result$outliers)
  }

  if (!is.null(group_col)) {
    data[[group_col]] <- as.factor(data[[group_col]])
  }
  data_with_outliers <- data %>% dplyr::select(all_of(outlier_vars), all_of(group_col))

  melted_data <- tidyr::pivot_longer(data_with_outliers, cols = all_of(outlier_vars), names_to = "variable", values_to = "value")
  colors<-as.vector(wes_palette(palette_name))
  all_plots <- list()

  for (i in seq(1, length(outlier_vars), by = plot_display_num)) {
    selected_vars <- outlier_vars[i:min(i + plot_display_num - 1, length(outlier_vars))]

    if (!is.null(group_col)) {
      p <- ggplot(melted_data %>% dplyr::filter(variable %in% selected_vars), aes(x = variable, y = value, fill = !!sym(group_col))) +
        geom_boxplot(outlier.shape = 19, outlier.colour = "red", outlier.size = 1) +
        scale_fill_manual(values = wes_palette(palette_name)) +
        labs(title = "Boxplot of Variables with Outliers",
             x = "Variable",
             y = "Value") +
        theme_prism(base_size = base_size) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.position = "top",
          legend.title = element_blank()
        )
    } else {
      p <- ggplot(melted_data %>% dplyr::filter(variable %in% selected_vars), aes(x = variable, y = value)) +
        geom_boxplot(outlier.shape = 19, outlier.colour = "red", outlier.size = 1, fill = colors[1]) +
        labs(title = "Boxplot of Variables with Outliers",
             x = "Variable",
             y = "Value") +
        theme_prism(base_size = base_size) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12)
        )
    }

    all_plots[[length(all_plots) + 1]] <- p
  }
  cat("Generated ", length(all_plots), " plots for outlier visualization.\n")
  print(all_plots[[1]])

  if (save_plots) {
    outlier_dir <- file.path(save_dir)

    if (!is.null(sub_var) && length(sub_var) > 0) {
      sub_var_name <- paste(sub_var, collapse = "_")
      sub_dir <- file.path(outlier_dir, paste("sub_var_", sub_var_name, sep = ""))
      if (!dir.exists(sub_dir)) {
        dir.create(sub_dir, recursive = TRUE)
      }
      outlier_dir <- sub_dir
    } else {
      if (!dir.exists(outlier_dir)) {
        dir.create(outlier_dir, recursive = TRUE)
      }
    }

    for (i in 1:length(all_plots)) {
      ggsave(filename = file.path(outlier_dir, paste0("boxplot_outliers_batch_", i, ".pdf")),
             plot = all_plots[[i]],
             width = plot_width,
             height = plot_height, device = "pdf")
    }
    cat("Saved plots to: ", outlier_dir, "\n")
  }

  rownames(data) <- original_rownames

  return(list(
    data_marked = data,
    plots = all_plots,
    normal_ranges = range_info_df
  ))

}


#' Detect and Handle Outliers in Statistical Data
#'
#' This function identifies outliers in numeric variables using IQR method (with optional custom ranges),
#' marks them in the dataset, provides visualization, and handles them using specified method.
#' Works with both raw data frames and 'Stat' class objects.
#'
#' @param object Either a data frame or an object of class 'Stat' containing the data
#' @param save_outliers Logical indicating whether to keep outlier information (default: TRUE)
#' @param group_col Name of grouping variable for visualization (default: "group")
#' @param palette_name Name of color palette for boxplots (default: "Royal1")
#' @param handle_method Outlier handling method: "replace", "remove", "keep", or "capping" (default: "replace")
#' @param lower_quantile Lower quantile for capping method (default: 0.05)
#' @param upper_quantile Upper quantile for capping method (default: 0.95)
#' @param save_plots Logical indicating whether to save boxplots (default: TRUE)
#' @param save_dir Directory path to save outputs (default: here("StatObject", "pre_outlier_info"))
#' @param plot_display_num Number of variables per plot (default: 1)
#' @param sub_var Optional subset of variables to analyze (default: NULL)
#' @param plot_width Plot width in inches (default: 5)
#' @param plot_height Plot height in inches (default: 5)
#' @param base_size Base font size for plots (default: 14)
#' @param custom_ranges Named list of custom value ranges for variables (default: NULL)
#' @param max_unique_values Max unique values to consider numeric (default: 5)
#'
#' @return Depending on input type:
#' \itemize{
#'   \item For 'Stat' objects: Updated object with outlier info in @process.info slot
#'   \item For data frames: List containing:
#'     \itemize{
#'       \item marked_data - Original data with outlier markers
#'       \item handled_data - Processed data after outlier handling
#'       \item handling_method - Applied handling method
#'       \item plots - Boxplot visualizations
#'       \item normal_ranges - IQR and boundary info
#'       \item handling_details - Additional processing details
#'     }
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # With Stat object
#' object_stat <- stat_detect_and_mark_outliers(object_stat)                                        handle_method = "capping")
#' }

stat_detect_and_mark_outliers <- function(object,
                                          save_outliers = TRUE,
                                          group_col = "group",
                                          palette_name = "Royal1",
                                          handle_method = c("replace", "remove", "keep", "capping"),
                                          lower_quantile = 0.05,
                                          upper_quantile = 0.95,
                                          save_plots = TRUE,
                                          save_dir = here("StatObject", "pre_outlier_info"),
                                          plot_display_num = 1,
                                          sub_var = NULL,
                                          plot_width = 5,
                                          plot_height = 5,
                                          base_size = 14,
                                          custom_ranges = NULL,
                                          max_unique_values = 5) {

  if (inherits(object, "Stat")) {
    data <- slot(object, "clean.data")

    if (is.null(data) || nrow(data) == 0) {
      data <- slot(object, "raw.data")
    }
    group_col <- object@group_col
    if (length(group_col) == 0) {
      group_col <- NULL
    }
  } else if (is.data.frame(object)) {
    data <- object
  } else {
    stop("Input must be an object of class 'Stat' or a data frame")
  }

  if (is.null(data) || nrow(data) == 0) {
    stop("No valid data found in the input")
  }

  cat("Processing data with", nrow(data), "rows.\n")

  outlier_result <- detect_and_mark_outliers(
    data = data,
    group_col = group_col,
    palette_name = palette_name,
    save_plots = save_plots,
    save_dir = save_dir,
    plot_display_num = plot_display_num,
    sub_var = sub_var,
    plot_width = plot_width,
    plot_height = plot_height,
    base_size = base_size,
    custom_ranges = custom_ranges,
    max_unique_values = max_unique_values
  )



  if (inherits(object, "Stat")) {
    cat("Updating 'Stat' object...\n")
    object@process.info[["outlier_info"]][["detect_info"]]<- outlier_result
    cat("The 'Stat' object has been updated with the following slots:\n")
    cat("- 'process.info' slot updated with pre-handling information\n")

    return(object)
  }else{
    return(list(
      marked_data = outlier_result$data_marked,
      handled_data = handled_data$cleaned_data,
      handling_method = handled_data$method,
      plots = outlier_result$plots,
      normal_ranges = outlier_result$normal_ranges,
      handling_details = handled_data[!names(handled_data) %in% "cleaned_data"]
    ))
  }
}


#' Extract Marked Outlier Data from Stat Object
#'
#' Retrieves the dataset with marked outliers from a specified slot in a 'Stat' class object.
#' This is a utility function for accessing pre-processed outlier information stored in S4 objects.
#'
#' @param object An object of class 'Stat' containing outlier information
#' @param slot_name Name of the slot containing outlier data (default: "outlier.marked")
#'
#' @return A data frame containing the original data with outlier markers (columns ending with "_outlier")
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'object_stat' is a Stat object with outlier information
#' object_stat <- extract_outlier_data(object_stat)
#' }
#'
extract_outlier_data <- function(object,
                                 slot_name = "outlier.marked") {
  if (!inherits(object, "Stat")) {
    stop("Input must be an object of class 'Stat'.")
  }

  if (!slot_name %in% slotNames(object)) {
    stop(paste("Slot", slot_name, "does not exist in the object."))
  }

  outlier_info <- slot(object, slot_name)

  if (is.null(outlier_info) || !("data_marked" %in% names(outlier_info))) {
    stop("No marked outlier data found.")
  }

  data_marked <- outlier_info[["data_marked"]]
  return(data_marked)
}


#' Remove Outliers from Dataset
#'
#' Filters out rows marked as outliers (where *_outlier columns are TRUE) from a dataset.
#' This function is typically used after outlier detection to create a cleaned dataset.
#'
#' @param data A data frame containing columns with "_outlier" suffix indicating outlier status
#'
#' @return A list containing:
#' \itemize{
#'   \item cleaned_data - Data frame with outlier rows removed
#'   \item method - Character string indicating the method used ("remove")
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # First mark outliers (assuming detect_outliers() was run)
#' marked_data <- detect_outliers(iris)
#'
#' # Then remove outliers
#' cleaned <- handle_outliers_remove(marked_data)
#'
#' # Compare dimensions
#' dim(marked_data)
#' dim(cleaned$cleaned_data)
#' }
handle_outliers_remove <- function(data) {
  cleaned_data <- data %>%
    dplyr::filter(dplyr::if_all(tidyselect::ends_with("_outlier"), ~ !.))

  return(list(cleaned_data = cleaned_data,
              method = "remove"))
}


#' Replace Outliers with Median Values
#'
#' Replaces values marked as outliers (where *_outlier columns are TRUE) with the median
#' of the corresponding variable. This is a robust method for handling outliers while
#' preserving the sample size.
#'
#' @param data A data frame containing columns with "_outlier" suffix indicating
#'             outlier status and corresponding original variables
#'
#' @return A list containing:
#' \itemize{
#'   \item cleaned_data - Data frame with outliers replaced by medians and outlier columns removed
#'   \item method - Character string indicating the method used ("replace")
#'   \item replaced_values - Named list of median values used for replacement
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # First mark outliers (assuming detect_outliers() was run)
#' marked_data <- detect_outliers(mtcars)
#'
#' # Then replace outliers with medians
#' result <- handle_outliers_replace(marked_data)
#'
#' # View replacement values
#' print(result$replaced_values)
#'
#' # Compare summary statistics
#' summary(marked_data$mpg)
#' summary(result$cleaned_data$mpg)
#' }
handle_outliers_replace <- function(data) {

  replaced_values <- list()
  outlier_cols <- grep("_outlier$", colnames(data), value = TRUE)

  for (var in outlier_cols) {
    original_var <- sub("_outlier$", "", var)

    median_value <- median(data[[original_var]], na.rm = TRUE)

    data[[original_var]][data[[var]]] <- median_value

    replaced_values[[original_var]] <- median_value
  }

  cleaned_data <- data %>% dplyr::select(-ends_with("_outlier"))

  return(list(
    cleaned_data = cleaned_data,
    method = "replace",
    replaced_values = replaced_values
  ))
}

#' Keep Outliers in Dataset (No Processing)
#'
#' A placeholder function that returns the original data without modifying outliers.
#' This is useful for maintaining a consistent pipeline structure when no outlier
#' handling is desired.
#'
#' @param data A data frame (typically containing outlier markers)
#'
#' @return A list containing:
#' \itemize{
#'   \item cleaned_data - The original unmodified data frame
#'   \item method - Character string indicating the method used ("keep")
#' }
#'
#' @export
handle_outliers_keep <- function(data) {
  return(list(cleaned_data=data,
              method ="keep"))}



#' Cap Outliers Using Quantile Thresholds
#'
#' Handles outliers by capping extreme values at specified quantile thresholds. Values below the lower
#' quantile are set to the lower quantile value, and values above the upper quantile are set to the
#' upper quantile value. This method preserves the original data structure while reducing the impact
#' of extreme values.
#'
#' @param data A data frame containing columns with "_outlier" suffix indicating outlier status
#' @param lower_quantile Lower quantile threshold for capping (default: 0.05)
#' @param upper_quantile Upper quantile threshold for capping (default: 0.95)
#'
#' @return A list containing:
#' \itemize{
#'   \item cleaned_data - Data frame with capped values and outlier columns removed
#'   \item method - Character string indicating the method used ("capping")
#'   \item capping_bounds - Named list of the actual capping bounds used for each variable
#'   \item parameters - List containing the quantile parameters used
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # First mark outliers
#' marked_data <- detect_outliers(mtcars)
#'
#' # Cap outliers at 5th and 95th percentiles
#' result <- handle_outliers_capping(marked_data)
#'
#' # View capping bounds
#' print(result$capping_bounds)
#'
#' # Use custom quantiles (1st and 99th percentiles)
#' custom_result <- handle_outliers_capping(marked_data, 0.01, 0.99)
#' }
handle_outliers_capping <- function(data,
                                    lower_quantile = 0.05,
                                    upper_quantile = 0.95) {
  capping_bounds <- list()

  outlier_cols <- grep("_outlier$", colnames(data), value = TRUE)

  for (var in outlier_cols) {
    original_var <- sub("_outlier$", "", var)

    lower_bound <- quantile(data[[original_var]], lower_quantile, na.rm = TRUE)
    upper_bound <- quantile(data[[original_var]], upper_quantile, na.rm = TRUE)

    capping_bounds[[original_var]] <- list(
      lower_bound = lower_bound,
      upper_bound = upper_bound
    )

    data[[original_var]] <- pmax(
      pmin(data[[original_var]], upper_bound),
      lower_bound
    )
  }

  cleaned_data <- data %>% dplyr::select(-ends_with("_outlier"))

  return(list(
    cleaned_data = cleaned_data,
    method = "capping",
    capping_bounds = capping_bounds,
    parameters = list(
      lower_quantile = lower_quantile,
      upper_quantile = upper_quantile
    )
  ))
}

#' Handle Outliers Using Specified Method
#'
#' A wrapper function that applies different outlier handling methods (remove, replace, keep, or capping)
#' to a dataset containing outlier markers. Provides options to save the cleaned data.
#'
#' @param data A data frame containing columns with "_outlier" suffix indicating outlier status
#' @param handle_method Outlier handling method: "replace" (with median), "remove" (rows),
#'        "keep" (no change), or "capping" (quantile-based) (default: "replace")
#' @param lower_quantile Lower quantile threshold for capping method (default: 0.05)
#' @param upper_quantile Upper quantile threshold for capping method (default: 0.95)
#' @param save_dir Directory path to save cleaned data (default: here("StatObject","Data"))
#' @param save_data Logical indicating whether to save cleaned data (default: TRUE)
#' @param csv_filename Filename for saved cleaned data (default: "clean_data.csv")
#'
#' @return A list containing (structure varies by method):
#' \itemize{
#'   \item cleaned_data - Processed data frame
#'   \item method - Applied handling method
#'   \item Additional method-specific elements (replaced_values, capping_bounds etc.)
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # After detecting outliers
#' marked_data <- detect_outliers(mtcars)
#'
#' # Basic usage (default replace method)
#' cleaned <- handle_outliers(marked_data)
#'
#' # Capping method with custom quantiles
#' capped <- handle_outliers(marked_data,
#'                          handle_method = "capping",
#'                          lower_quantile = 0.1,
#'                          upper_quantile = 0.9)
#'
#' # Without saving to file
#' cleaned <- handle_outliers(marked_data, save_data = FALSE)
#' }
handle_outliers <- function(data,
                            handle_method = c("replace", "remove", "keep", "capping"),
                            lower_quantile = 0.05,
                            upper_quantile = 0.95,
                            save_dir = here::here("StatObject","Data"),
                            save_data = F,
                            csv_filename = "clean_data.csv") {

  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  if (!any(grepl("_outlier$", colnames(data)))) {
    stop("No '_outlier' columns found in the data.")
  }

  handle_method <- match.arg(handle_method)

  if (handle_method == "remove") {
    data <- handle_outliers_remove(data)
  } else if (handle_method == "replace") {
    data <- handle_outliers_replace(data)
  } else if (handle_method == "keep") {
    data <- handle_outliers_keep(data)
  } else if (handle_method == "capping") {
    data <- handle_outliers_capping(data,
                                    lower_quantile = lower_quantile,
                                    upper_quantile = upper_quantile)
  } else {
    stop("Unknown method!")
  }

  if (save_data) {
    if (!dir.exists(save_dir)) {
      dir.create(save_dir, recursive = TRUE)}
    full_path <- file.path(save_dir, csv_filename)
    write.csv(data, file = full_path, row.names = FALSE)
    cat("Cleaned data saved to:", full_path, "\n")
  }
  return(data)
}


#' Handle Outliers in Statistical Data Objects
#'
#' Processes outliers in either Stat class objects or data frames using specified method.
#' For Stat objects, updates the object slots with cleaning information. Maintains row names
#' throughout processing.
#'
#' @param object Either a Stat class object or data frame containing outlier markers
#' @param save_cleaned Logical indicating whether to keep cleaned data (default: TRUE)
#' @param handle_method Outlier handling method: "replace" (median), "remove" (rows),
#'        "keep" (no change), or "capping" (quantile-based) (default: "replace")
#' @param lower_quantile Lower quantile for capping method (default: 0.05)
#' @param upper_quantile Upper quantile for capping method (default: 0.95)
#' @param group_col Name of grouping variable column (default: "group")
#' @param save_dir Directory path to save cleaned data (default: here("StatObject","Data"))
#' @param save_data Logical indicating whether to save to CSV (default: TRUE)
#' @param csv_filename Filename for saved cleaned data (default: "clean_data.csv")
#'
#' @return Depending on input:
#' \itemize{
#'   \item For Stat objects: Updated object with:
#'     \itemize{
#'       \item @process.info updated with handling metadata
#'       \item @clean.data updated with processed data
#'     }
#'   \item For data frames: Cleaned data frame with row names preserved
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # With Stat object
#' object_stat <- stat_handle_outliers(object_stat, handle_method = "capping")
#'
#' }
#'
stat_handle_outliers <- function(object,
                                 save_cleaned = TRUE,
                                 handle_method = c("replace", "remove", "keep", "capping"),
                                 lower_quantile = 0.05,
                                 upper_quantile = 0.95,
                                 group_col = "group",
                                 save_dir = here::here("StatObject","Data"),
                                 save_data = TRUE,
                                 csv_filename = "clean_data.csv") {

  handle_method <- match.arg(handle_method, choices = c("replace", "remove", "keep", "capping"))

  if (inherits(object, "Stat")) {
    cat("'object' is of class 'Stat'.\n Extracting outlier data...\n")
    data <-  object@process.info[["outlier_info"]][["detect_info"]][["data_marked"]]
    group_col <- object@group_col
    if (length(group_col) == 0) {
      group_col <- NULL
    }
  } else if (is.data.frame(object)) {
    cat(" 'object' is a data frame.\n")
    data <- object
  } else {
    stop("Input must be an object of class 'Stat' or a data frame.")
  }

  if (is.null(data) || nrow(data) == 0) {
    stop("No valid data found in the input.")
  }

  cat("Handling outliers with method: ", handle_method,"\n")
  original_rownames <- rownames(data)
  handle_outliers_result <- handle_outliers(data,
                                  handle_method = handle_method,
                                  save_dir =save_dir,
                                  save_data = save_data,
                                  csv_filename = csv_filename,
                                  lower_quantile = lower_quantile,
                                  upper_quantile = upper_quantile)

  cleaned_data<-handle_outliers_result[["cleaned_data"]]
  rownames(cleaned_data) <- original_rownames
  cat("Outlier handling completed. Cleaned data has ", nrow(cleaned_data), " rows.","\n")

  if (inherits(object, "Stat")) {
    cat("Updating 'Stat' object...\n")
    object@process.info[["outlier_info"]][["handle_info"]] <- handle_outliers_result
    object@clean.data <- cleaned_data
    cat("The 'Stat' object has been updated with the following slots:\n")
    cat("- 'outlier.marked' slot updated.\n")
    cat("- 'process.info' slot updated.\n")

    return(object)
  }

  return(cleaned_data)
}


