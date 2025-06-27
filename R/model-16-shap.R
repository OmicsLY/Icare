#' Generate SHAP Analysis Plots from a Trained Model
#'
#' This function fits a LightGBM classification model on the provided training data,
#' calculates SHAP (SHapley Additive exPlanations) values using kernel approximation,
#' and generates three visualizations: beeswarm, force, and waterfall plots.
#'
#'
#' @importFrom recipes recipe
#' @importFrom parsnip boost_tree set_engine set_mode
#' @importFrom workflows workflow add_model add_recipe fit
#' @importFrom kernelshap kernelshap
#' @importFrom shapviz shapviz sv_importance sv_force sv_waterfall
#' @import ggplot2
#' @import lightgbm
#' @import bonsai
#' @importFrom gridExtra grid.arrange
#' @importFrom grid textGrob gpar
#' @param train_data A data frame containing the training features and labels. The response variable should be specified in `group_col`.
#' @param best_model Currently unused. Reserved for potential future compatibility.
#' @param group_col Character. The name of the column in `train_data` representing the target variable. Default is `"group"`.
#' @param palette_name Character. Name of the color palette used for plots from `wesanderson`. Default is `"AsteroidCity1"`.
#' @param save_plots Logical. If `TRUE`, plots will be saved as PDF files in the specified directory. Default is `TRUE`.
#' @param save_dir Character. Path to the directory where plots will be saved. Default is `here("ModelData", "best_model_result")`.
#' @param plot_width Numeric. Width of the plot in inches. Default is 5.
#' @param plot_height Numeric. Height of the plot in inches. Default is 5.
#' @param base_size Numeric. Base font size for plot themes. Default is 14.
#' @param seed Integer. Random seed for reproducibility. Default is 123.
#'
#' @returns A list containing the generated `ggplot` objects:
#' \describe{
#'   \item{beeswarm_plot}{SHAP beeswarm plot for feature importance.}
#'   \item{force_plot}{SHAP force plot showing individual predictions.}
#'   \item{waterfall_plot}{SHAP waterfall plot visualizing cumulative feature effects.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage with training data
#' train_data <- your_training_data_frame
#' shap_result <- generate_shap_plots(train_data, best_model = NULL)
#'
#' # Access plots
#' shap_result$beeswarm_plot
#' shap_result$force_plot
#' shap_result$waterfall_plot
#' }

generate_shap_plots <- function(train_data,
                                best_model,
                                group_col = "group",
                                palette_name = "AsteroidCity1",
                                save_plots = TRUE,
                                save_dir = here("ModelData", "best_model_result"),
                                plot_width = 5,
                                plot_height = 5,
                                base_size = 14,
                                seed = 123) {

  set.seed(seed)

  cat("Converting group column to factor...\n")
  train_data[[group_col]] <- as.factor(train_data[[group_col]])

  cat("Creating recipe for model...\n")
  rec <- recipe(as.formula(paste(group_col, "~ .")), data = train_data)

  cat("Setting up LightGBM model...\n")
  lgb_model <- boost_tree() %>%
    set_engine('lightgbm') %>%
    set_mode('classification')

  lgb_wflow <- workflow() %>%
    add_model(lgb_model) %>%
    add_recipe(rec)

  cat("Fitting the model...\n")
  lgb_fit <- workflows::fit(lgb_wflow, data = train_data)

  pred_fun <- function(model, new_data) {
    predict(model, new_data = new_data, type = "prob")$.pred_1
  }

  bg_sample <- train_data[sample(1:nrow(train_data), min(100, nrow(train_data))), ]

  cat("Calculating SHAP values...\n")
  ks <- kernelshap(pred_fun, object = lgb_fit, X = train_data[, -which(names(train_data) == group_col)], bg_X = bg_sample)

  non_converging <- attr(ks, "non_converging")
  if (!is.null(non_converging) && length(non_converging) > 0) {
    warning("Non-converging rows: ", paste(non_converging, collapse = ", "))
  }

  shp <- shapviz(ks)

  colors <-as.vector(wes_palette(palette_name))

  cat("Generating SHAP plots...\n")
  p1 <- sv_importance(shp, kind = "beeswarm") +
    theme_classic(base_size = base_size) +
    labs(title = "SHAP Feature Importance", x = "SHAP Value", y = "Feature")

  p2 <- sv_force(shp, col = colors[3]) +
    theme_minimal(base_size = base_size) +
    labs(title = "SHAP Force Plot", x = "Feature Contribution", y = "Sample Index")

  p3 <- sv_waterfall(shp, col = colors[4]) +
    theme_minimal(base_size = base_size) +
    labs(title = "SHAP Waterfall Plot", x = "Cumulative SHAP Value", y = "Feature")


  grid.arrange(
    p1, p2, p3,
    ncol = 1,
    top = textGrob("SHAP Analysis Plots", gp = gpar(fontsize = 18, fontface = "bold"))
  )


  if (save_plots) {
    ggsave(filename = file.path(save_dir, "shap_beeswarm_plot.pdf"),
           plot = p1,
           width = plot_width,
           height = plot_height,
           device = "pdf")
    cat("Beeswarm plot saved to:", file.path(save_dir, "shap_beeswarm_plot.pdf"), "\n")

    ggsave(filename = file.path(save_dir, "shap_force_plot.pdf"),
           plot = p2,
           width = plot_width,
           height = plot_height,
           device = "pdf")
    cat("Force plot saved to:", file.path(save_dir, "shap_force_plot.pdf"), "\n")

    ggsave(filename = file.path(save_dir, "shap_waterfall_plot.pdf"),
           plot = p3,
           width = plot_width,
           height = plot_height,
           device = "pdf")
    cat("Waterfall plot saved to:", file.path(save_dir, "shap_waterfall_plot.pdf"), "\n")
  }

  return(list(beeswarm_plot = p1, force_plot = p2, waterfall_plot = p3))
}

#' Generate and Store SHAP Visualizations for a Best_Model Object
#'
#' This function extracts the best-performing model from a `Best_Model` object,
#' computes SHAP values on the training dataset, generates three SHAP plots (beeswarm, force, and waterfall),
#' and stores the results back into the `shap.result` slot of the `Best_Model` object.
#'
#' @param object An object of class `Best_Model`, typically generated by the Icare package. Must contain training models and result metrics.
#' @param group_col Character. The name of the column in the dataset that contains group labels. This is usually set automatically from the object. Default is `"group"`.
#' @param palette_name Character. Name of the color palette used in visualizations (from `wesanderson` package). Default is `"AsteroidCity1"`.
#' @param save_plots Logical. Whether to save the generated plots as PDF files. Default is `TRUE`.
#' @param save_dir Character. Directory path where the plots will be saved. Default is `here("ModelData", "best_model_result")`.
#' @param plot_width Numeric. Width of each plot in inches. Default is 5.
#' @param plot_height Numeric. Height of each plot in inches. Default is 5.
#' @param base_size Numeric. Base font size used in the plots. Default is 14.
#' @param seed Integer. Random seed for reproducibility. Default is 123.
#'
#' @returns The modified `Best_Model` object with SHAP visualizations stored in the `shap.result` slot.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data("object_model")
#' object_model <- ModelShap(object = object_model)
#' object_model@shap.result$beeswarm_plot
#' }

ModelShap <- function(object,
                      group_col = "group",
                      palette_name = "AsteroidCity1",
                      save_plots = TRUE,
                      save_dir = here("ModelData", "best_model_result"),
                      plot_width = 5,
                      plot_height = 5,
                      base_size = 14,
                      seed = 123) {

    if (inherits(object, "Best_Model")) {
      cat("Input is of class 'Best_Model'. Extracting datasets...\n")
      best_model<-object@best.model[[1]]
      best_model_type<-object@best.model.type
      group_col  <-  object@group_col
      data_sets <- object@filtered.set
      train_data <- data_sets$training
      test_data <- data_sets$testing
    }

    cat("Generating SHAP plots...\n")
    shap_plots <- generate_shap_plots(train_data=train_data,
                                      best_model=best_model,
                                      group_col=group_col,
                                      palette_name=palette_name,
                                      save_plots=save_plots,
                                      save_dir=save_dir,
                                      plot_width=plot_width,
                                      plot_height=plot_height,
                                      base_size=base_size, seed=seed)

    object@shap.result <- shap_plots
    cat("Updating 'Best_Model' object...\n")
    cat("The 'Best_Model' object has been updated with the following slots:\n")
    cat("- 'shap.result ' slot updated.\n")

    return(object)
  
}
