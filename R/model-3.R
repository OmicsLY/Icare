#' Balance the dataset using different sampling methods.
#'
#' @import ROSE
#' @param data A data frame containing the dataset to be balanced.
#' @param group_col A string specifying the column name in the data frame that indicates the group labels (e.g., "group"). Default is "group".
#' @param method A string specifying the sampling method to use. It can be one of the following:
#' - "over": Over-sampling the minority class.
#' - "under": Under-sampling the majority class.
#' - "both": Combination of both over-sampling and under-sampling. Default is "both".
#' @param N An integer specifying the desired number of rows in the balanced dataset. If `NULL`, it is automatically calculated based on the chosen method. Default is `NULL`.
#' @param seed An integer for random number generation to ensure reproducibility of the results. Default is 123.
#'
#' @returns A data frame with balanced data.
#' @export
#'
#' @examples
#' balanced_data <- balance_data_process(data = your_data, group_col = "group", method = "over", N = 1000)
#' balanced_data <- balance_data_process(data = your_data, method = "under")

balance_data_process <- function(data,
                                 group_col = "group",
                                 method = "both",
                                 N = NULL,
                                 seed = 123) {
  data[[group_col]] <- as.factor(data[[group_col]])

  if (!method %in% c("over", "under", "both")) {
    stop("Invalid method. Choose one of 'over', 'under', or 'both'.")
  }

  if (is.null(N)) {
    if (method == "over") {
      majority_class_size <- max(table(data[[group_col]]))
      N <- 2 * majority_class_size
    } else if (method == "under") {
      minority_class_size <- min(table(data[[group_col]]))
      N <- 2 * minority_class_size
    } else {
      N <- nrow(data)
    }
  }

  formula <- as.formula(paste(group_col, "~ ."))

  original_row_names <- rownames(data)
  balanced_process<- ovun.sample(formula,
                                 data = data,
                                 method = method,
                                 N = N, seed = seed)
  balanced_data <- balanced_process$data


  return(balanced_data)
}



#' Visualize the balance of the dataset before and after balancing.
#'
#' This function generates a bar plot to visualize the distribution of class labels in the original and balanced datasets. The plot compares the class distribution between the two datasets and saves it as a PDF if required.
#'
#' @import ggplot2
#' @import wesanderson
#' @importFrom ggprism theme_prism
#' @importFrom scales percent
#' @import here 
#' @param original_data A data frame containing the original (imbalanced) dataset.
#' @param balanced_data A data frame containing the balanced dataset.
#' @param group_col A string specifying the column name that represents the class labels in both datasets. Default is "group".
#' @param palette_name A string specifying the name of the color palette for the plot. Default is "Royal1".
#' @param base_size An integer specifying the base font size for the plot. Default is 14.
#' @param save_plots A logical value indicating whether to save the plot as a PDF. Default is `TRUE`.
#' @param save_dir A string specifying the directory where the plot will be saved. Default is "ModelData/balacing_info" using `here()`.
#' @param plot_width A numeric value specifying the width of the plot when saved. Default is 5.
#' @param plot_height A numeric value specifying the height of the plot when saved. Default is 5.
#'
#' @returns A ggplot object representing the class distribution before and after balancing.
#' @export
#'
#' @examples
#' plot <- visualize_balance(original_data = original_data, balanced_data = balanced_data, group_col = "group")
#' plot <- visualize_balance(original_data = original_data, balanced_data = balanced_data, save_plots = FALSE)

visualize_balance <- function(
    original_data,
    balanced_data,
    group_col = "group",
    palette_name = "Royal1",
    base_size = 14,
    save_plots = TRUE,
    save_dir = here("ModelData", "balacing_info"),
    plot_width = 5,
    plot_height = 5) {


  original_data$dataset <- "Original"
  balanced_data$dataset <- "Balanced"
  combined_data <- rbind(original_data, balanced_data)

  plot <- ggplot(combined_data, aes(x = .data[[group_col]], fill = dataset)) +
    geom_bar(position = "dodge") +
    geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +
    geom_text(stat = 'count', aes(label = scales::percent(..count../sum(..count..), accuracy = 0.1)),
              position = position_dodge(width = 0.9), vjust = 1.5, size = 4) +
    labs(title = "Before and After Balancing",
         x = group_col,
         y = "Count",
         fill = "Dataset") +
    scale_fill_manual(values = wes_palette(palette_name)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12)) +
    theme_prism(base_size = base_size)

  if (save_plots) {
    ggsave(filename = file.path(save_dir, "class_distribution_balance.pdf"),
           plot = plot,
           width = plot_width, height = plot_height,
           device = "pdf")
    cat("Plot saved to:", file.path(save_dir, "class_distribution_balance.pdf"), "\n")
  }

  return(plot)
}


#' Balance and visualize data distribution before and after balancing.
#'
#' This function detects class imbalance in the dataset and balances it if necessary using specified methods. It also visualizes the class distribution before and after balancing, and optionally saves the plot as a PDF.
#'
#' @import ggplot2
#' @import wesanderson
#' @importFrom ggprism theme_prism
#' @importFrom scales percent
#' @import here 
#' @import stats
#' @param data A data frame containing the data to be balanced.
#' @param group_col A string specifying the column name that represents the class labels.
#' @param method A string specifying the balancing method. Can be "over", "under", or "both". Default is "both".
#' @param N A numeric value specifying the number of samples to generate after balancing. If `NULL`, it is automatically calculated based on the chosen method.
#' @param seed An integer to set the random seed for reproducibility. Default is 123.
#' @param palette_name A string specifying the color palette name for the plot. Default is "Royal1".
#' @param imbalance_threshold A numeric value representing the threshold for class imbalance. If the class ratio is below this threshold, balancing is triggered. Default is 0.15.
#' @param sample_size_threshold A numeric value representing the threshold for sample size. If the number of samples is below this threshold, balancing is triggered. Default is 1500.
#' @param force_balance A logical value indicating whether to force balancing regardless of the class distribution. Default is `FALSE`.
#' @param save_plots A logical value indicating whether to save the plot as a PDF. Default is `TRUE`.
#' @param save_dir A string specifying the directory where the plot will be saved. Default is "ModelData/balacing_info" using `here()`.
#' @param plot_width A numeric value specifying the width of the plot when saved. Default is 5.
#' @param plot_height A numeric value specifying the height of the plot when saved. Default is 5.
#' @param base_size An integer specifying the base font size for the plot. Default is 14.
#'
#' @returns A list containing the following elements:
#' - `original_data`: The original (imbalanced) data.
#' - `balanced_data`: The balanced dataset.
#' - `method`: The method used for balancing.
#' - `balanced_plot`: The plot visualizing the class distribution before and after balancing.
#'
#' @export
#'
#' @examples
#' result <- balance_and_visualize_data(data = dataset, group_col = "group")
#' result <- balance_and_visualize_data(data = dataset, group_col = "group", force_balance = TRUE)

balance_and_visualize_data <- function(
    data,
    group_col,
    method = "both",
    N = NULL,
    seed = 123,
    palette_name = "Royal1",
    imbalance_threshold = 0.15,
    sample_size_threshold = 1500,
    force_balance = FALSE,
    save_plots = TRUE,
    save_dir = here("ModelData","balacing_info"),
    plot_width = 5,
    plot_height = 5,
    base_size = 14) {

  class_counts <- table(data[[group_col]])
  class_ratio <- min(class_counts) / sum(class_counts)

  cat("Class counts before balancing:\n")
  print(class_counts)

  cat("\nforce_balance:", force_balance, "\n")
  cat("class_ratio:", class_ratio, "\n")
  cat("class_ratio < imbalance_threshold:", class_ratio < imbalance_threshold, "\n")
  cat("nrow(data):", nrow(data), "\n")
  cat("nrow(data) < sample_size_threshold:", nrow(data) < sample_size_threshold, "\n\n")
  method=method
  if (force_balance || (class_ratio < imbalance_threshold && nrow(data) < sample_size_threshold)) {
    cat("Data imbalance detected or force_balance is TRUE, or sample size is below the threshold. Balancing data.\n")

    balanced_data <- balance_data_process(data=data,
                                          group_col=group_col,
                                          method=method,
                                          N=N, seed=seed)

    class_counts_after <- table(balanced_data[[group_col]])
    class_ratio_after <- min(class_counts_after) / sum(class_counts_after)

    cat("Class ratio after balancing:", round(class_ratio_after, 4), "\n")
    cat("Class counts after balancing:\n")
    print(class_counts_after)

    plot <- visualize_balance(original_data = data,
                              balanced_data = balanced_data,
                              group_col = group_col,
                              palette_name = palette_name,
                              base_size = base_size,
                              save_plots = save_plots,
                              save_dir = save_dir,
                              plot_width = plot_width,
                              plot_height = plot_height)
    print(plot)

    balance_result <- list(original_data = data,
                           balanced_data = balanced_data,
                           method = method,
                           balanced_plot = plot)
    return(balance_result)
  } else {
    cat("Data is balanced within threshold or force_balance is FALSE. No balancing performed.\n")
    cat("Class ratio (below threshold):", round(class_ratio, 4), "\n\n")

    balance_result <- list(balanced_data = data,
                           method = NULL,
                           balanced_plot = NULL)

    return(balance_result)
  }
}

#' Balance and visualize data distribution with updates to Model_data object.
#'
#' This function detects class imbalance in a dataset and balances it using specified methods. It also visualizes the class distribution before and after balancing. The function can handle both `Model_data` objects and data frames. If a `Model_data` object is provided, the balanced data and associated information are stored back into the object.
#' @import methods
#' @import here 
#' @param object An object of class `Model_data` or a data frame. If a `Model_data` object is provided, the function operates on the `clean.df` slot.
#' @param group_col A string specifying the column name that represents the class labels.
#' @param method A string specifying the balancing method. Can be "over", "under", or "both". Default is "both".
#' @param N A numeric value specifying the number of samples to generate after balancing. If `NULL`, it is automatically calculated based on the chosen method.
#' @param palette_name A string specifying the color palette name for the plot. Default is "Royal1".
#' @param seed An integer to set the random seed for reproducibility. Default is 123.
#' @param imbalance_threshold A numeric value representing the threshold for class imbalance. If the class ratio is below this threshold, balancing is triggered. Default is 0.15.
#' @param sample_size_threshold A numeric value representing the threshold for sample size. If the number of samples is below this threshold, balancing is triggered. Default is 1500.
#' @param force_balance A logical value indicating whether to force balancing regardless of the class distribution. Default is `FALSE`.
#' @param save_plots A logical value indicating whether to save the plot as a PDF. Default is `TRUE`.
#' @param save_dir A string specifying the directory where the plot will be saved. Default is "ModelData/balacing_info" using `here()`.
#' @param plot_width A numeric value specifying the width of the plot when saved. Default is 5.
#' @param plot_height A numeric value specifying the height of the plot when saved. Default is 5.
#' @param base_size An integer specifying the base font size for the plot. Default is 14.
#'
#' @returns If a `Model_data` object is provided, the updated object with the balanced data and balance information slots updated. If a data frame is provided, a list containing the balanced data and visualization results.
#'
#' @export
#'
#' @examples
#' # Balance data from a Model_data object
#' updated_model <- BalanceData(object = model_data_object, group_col = "group")
#'
#' # Balance data from a data frame
#' results <- BalanceData(object = dataset, group_col = "group")

BalanceData<- function(
    object,
    group_col = "group",
    method = "both",
    N = NULL,
    palette_name = "Royal1",
    seed = 123,
    imbalance_threshold = 0.15,
    sample_size_threshold = 1500,
    force_balance = FALSE,
    save_plots = TRUE,
    save_dir = here("ModelData", "balacing_info"),
    plot_width = 5,
    plot_height = 5,
    base_size = 14) {

  if (inherits(object, 'Model_data')) {
    data <- slot(object, "clean.df")
    group_col<-slot(object, "group_col")
  } else if (is.data.frame(object)) {
    data <- object
  } else {
    stop("Input must be an object of class 'Model_data' or a data frame.")
  }

  if (is.null(data) || nrow(data) == 0) {
    stop("No valid data found in the input.")
  }
  method=method

  results <- balance_and_visualize_data(
    data,
    group_col = group_col,
    method = method,
    N = N,
    seed = seed,
    palette_name = palette_name,
    imbalance_threshold = imbalance_threshold,
    sample_size_threshold = sample_size_threshold,
    force_balance = force_balance,
    save_plots = save_plots,
    save_dir = save_dir,
    plot_width = plot_width,
    plot_height = plot_height,
    base_size = base_size
  )

  balanced_data <- as.data.frame(results$balanced_data)

  if (inherits(object, 'Model_data')) {
    object@clean.df <- balanced_data
    object@balance.info <- results
    cat("Updating 'Model_data' object...\n")
    cat("The 'Model_data' object has been updated with the following slots:\n")
    cat("- 'clean.df' slot updated.\n")
    cat("- 'balance.info' slot updated.\n")
    return(object)
  }

  return(results)
}
