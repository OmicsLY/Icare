#' Extract Filtered Training and Testing Data from a Model Data Object
#'
#' This function extracts the filtered training and testing datasets stored in the
#' 'filtered.set' slot of a 'Model_data' object. If the 'filtered.set' slot is
#' not available or there is an error in accessing it, the function returns NULL for
#' both training and testing datasets.
#'
#' @param object An object of class 'Model_data' containing the 'filtered.set' slot,
#'   which holds the filtered training and testing data.
#'
#' @returns A list with two elements:
#'   - `training`: The filtered training dataset (if available), otherwise NULL.
#'   - `testing`: The filtered testing dataset (if available), otherwise NULL.
#'
#' @export
#'
#' @examples
#' # Example usage of Extract_filtered.set
#' # Assuming 'model_data' is an existing Model_data object with a 'filtered.set' slot
#' data_sets <- Extract_filtered.set(object = model_data)
#' training_data <- data_sets$training
#' testing_data <- data_sets$testing
Extract_filtered.set <- function(object) {
  train <- tryCatch(object@filtered.set$training,
                    error = function(e) NULL)
  test <- tryCatch(object@filtered.set$testing,
                   error = function(e) NULL)
  return(list(training = train, testing = test))
}

#' Train and Evaluate Multiple Models Using Caret
#'
#' This function trains and evaluates multiple machine learning models using the
#' `caret` package. It accepts multiple model types, tuning grids, and training control
#' parameters, and returns a list of trained models.
#'
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach registerDoSEQ
#' @import caret
#' @param data A data frame containing the training data. The target variable should
#'   be specified in the `group_col` parameter and must be a factor.
#' @param methods A vector of model names (strings) to train using the `caret` package.
#'   These should match the model names in `caret::modelLookup()`.
#' @param control A list containing control parameters for model training. Should
#'   include at least `method`, `number`, and `repeats` for the `trainControl()` function.
#' @param tune_grids A list where each element corresponds to a tuning grid for
#'   the respective model in `methods`. Each grid is passed to the `tuneGrid` parameter
#'   in the `train()` function.
#' @param classProbs A logical value indicating whether class probabilities should
#'   be computed. Default is `TRUE`.
#' @param verboseIter A logical value indicating whether iteration progress should
#'   be displayed during model training. Default is `FALSE`.
#' @param allowParallel A logical value indicating whether parallel processing
#'   should be used. Default is `TRUE`.
#' @param group_col The name of the target variable (the outcome or response variable)
#'   in the `data` set. Default is `"group"`.
#'
#' @returns A list of trained models, where each element is a model object corresponding
#'   to a method from `methods`. If training fails for a method, a warning is issued
#'   and that model is not included in the results.
#'
#' @export
#'
#' @examples
#' # Example usage:
#' data(iris)
#' methods <- c("rf", "rpart")
#' control <- list(method = "cv", number = 10, repeats = 3)
#' tune_grids <- list(rf = expand.grid(mtry = c(1, 2, 3)), rpart = expand.grid(cp = c(0.01, 0.1)))
#' results <- train_and_evaluate_models(iris, methods, control, tune_grids)
train_and_evaluate_models <- function(data,
                                      methods,
                                      control,
                                      tune_grids,
                                      classProbs = TRUE, verboseIter = FALSE,
                                      allowParallel = TRUE, group_col = "group") {


  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)


  data[[group_col]] <- as.factor(data[[group_col]])
  levels(data[[group_col]]) <- make.names(levels(data[[group_col]]))


  fitControl <- trainControl(method = control$method,
                             number = control$number,
                             repeats = control$repeats,
                             verboseIter = verboseIter,
                             allowParallel = allowParallel,
                             classProbs = classProbs)

  results <- list()

  for (method in methods) {
    if (method %in% caret::modelLookup()$model) {
      tune_grid <- tune_grids[[method]]


      model <- tryCatch({
        train(as.formula(paste(group_col, "~ .")), data = data,
              method = method,
              trControl = fitControl,
              tuneGrid = tune_grid,
              verbose = verboseIter)
      }, error = function(e) {
        warning(paste("Error training model", method, ":", e$message))
        return(NULL)
      })


      if (!is.null(model)) {
        results[[method]] <- model
      }
    } else {
      warning(paste("Model", method, "is not in caret's built-in library"))
    }
  }


  stopCluster(cl)
  registerDoSEQ()

  return(results)
}



#' Evaluate Model Performance
#'
#' This function evaluates the performance of one or multiple models by calculating
#' several classification metrics, including Sensitivity, Specificity, Precision,
#' F1-score, Accuracy, and AUC (Area Under the Curve). The evaluation is based
#' on predicted probabilities and the actual values from the given data.
#'
#' @importFrom pROC roc auc
#' @import stats
#' @param data A data frame containing the features and target variable for model
#'   evaluation. The target variable should be specified in the `group_col` parameter.
#' @param model_result A list of model objects or a single model object. The models
#'   should be trained using the `caret` package.
#' @param group_col The name of the target variable (the outcome or response variable)
#'   in the `data` set. Default is `"group"`.
#'
#' @returns A data frame containing the performance metrics for each model. If
#'   multiple models are provided, the results for each model are stacked in the
#'   resulting data frame. If a model produces a confusion matrix that is not 2x2,
#'   it is skipped, and a message is shown.
#'
#' @export
#'
#' @examples
#' # Example usage:
#' data(iris)
#' # Assume `train_model` is a trained model using `caret`
#' result <- evaluate_model_performance(iris, train_model)
#' print(result)
#'
#' # For multiple models:
#' models <- list(rf = rf_model, svm = svm_model)
#' results <- evaluate_model_performance(iris, models)
#' print(results)
evaluate_model_performance <- function(data, model_result, group_col = "group") {
  results <- list()

  if (!("method" %in% names(model_result))) {
    for (model_name in names(model_result)) {
      model <- model_result[[model_name]]

      pred_prob <- predict(model, newdata = data, type = "prob")[, 2]
      pred <- ifelse(pred_prob > 0.5, 1, 0)
      cf <- table(data[[group_col]], pred)

      if (dim(cf)[1] == 2 && dim(cf)[2] == 2) {
        TN <- cf[1, 1]
        FP <- cf[1, 2]
        FN <- cf[2, 1]
        TP <- cf[2, 2]

        Sensitivity <- TP / (TP + FN)
        Specificity <- TN / (TN + FP)
        Positive_predictive_value <- TP / (TP + FP) * 100
        Negative_predictive_value <- TN / (TN + FN) * 100
        accuracy_score <- sum(diag(cf)) / sum(cf) * 100
        Precision <- TP / (TP + FP) * 100
        f1_score <- 2 * (Precision * Sensitivity) / (Precision + Sensitivity)
        recall_score <- Sensitivity * 100

        roc_data <- roc(data[[group_col]], pred_prob)
        auc_value <- auc(roc_data)

        result <- data.frame(
          Model = model_name,
          Sensitivity = Sensitivity,
          Specificity = Specificity,
          Positive_predictive_value = Positive_predictive_value,
          Negative_predictive_value = Negative_predictive_value,
          accuracy_score = accuracy_score,
          Precision = Precision,
          f1_score = f1_score,
          recall_score = recall_score,
          auc = auc_value
        )

        results[[model_name]] <- result
      } else {
        message(paste("Model", model_name, "produced a confusion matrix that is not 2x2. Skipping this model."))
      }
    }

    all.results <- do.call(rbind, results)
    return(all.results)

  } else {
    model <- model_result

    pred_prob <- predict(model, newdata = data, type = "prob")[, 2]
    pred <- ifelse(pred_prob > 0.5, 1, 0)
    cf <- table(data[[group_col]], pred)

    if (dim(cf)[1] == 2 && dim(cf)[2] == 2) {
      TN <- cf[1, 1]
      FP <- cf[1, 2]
      FN <- cf[2, 1]
      TP <- cf[2, 2]

      Sensitivity <- TP / (TP + FN)
      Specificity <- TN / (TN + FP)
      Positive_predictive_value <- TP / (TP + FP) * 100
      Negative_predictive_value <- TN / (TN + FN) * 100
      accuracy_score <- sum(diag(cf)) / sum(cf) * 100
      Precision <- TP / (TP + FP) * 100
      f1_score <- 2 * (Precision * Sensitivity) / (Precision + Sensitivity)
      recall_score <- Sensitivity * 100

      roc_data <- roc(data[[group_col]], pred_prob)
      auc_value <- auc(roc_data)

      result <- data.frame(
        Model = model$method,
        Sensitivity = Sensitivity,
        Specificity = Specificity,
        Positive_predictive_value = Positive_predictive_value,
        Negative_predictive_value = Negative_predictive_value,
        accuracy_score = accuracy_score,
        Precision = Precision,
        f1_score = f1_score,
        recall_score = recall_score,
        auc = auc_value
      )

      return(result)
    } else {
      message("The confusion matrix produced by the model is not 2x2. Skipping this model.")
      return(NULL)
    }
  }
}


#' Plot ROC Curves and Compute AUC Values
#'
#' This function plots the ROC curves for multiple models and computes the AUC (Area Under the Curve) value along with its confidence interval for each model. The ROC curve is used to evaluate the performance of classification models, where a higher AUC value indicates a better model.
#'
#' @importFrom pROC roc auc ci.auc
#' @import ggplot2
#' @import wesanderson
#' @import here 
#' @import stats
#' @param model_list A list containing multiple models. Each element in the list is a trained model.
#' @param validation_data A dataset used to validate the model performance. The dataset should include both actual labels and features.
#' @param group_col The column name in the validation data representing the actual labels, default is "group".
#' @param palette_name The color palette name used for the plot, default is "AsteroidCity1".
#' @param base_size The base font size for the plot, default is 14.
#' @param save_plots Whether to save the generated plots, default is TRUE.
#' @param save_dir The directory path to save the plot, default is `here("ModelData", "best.model.result")`.
#' @param plot_width The width of the plot in inches, default is 5.
#' @param plot_height The height of the plot in inches, default is 5.
#' @param alpha The transparency level of the curve, default is 1 (fully opaque).
#'
#' @returns A list containing the ROC objects for each model.
#' @export
#'
#' @examples
#' # Call this function with a list of trained models and validation data
#' plot_roc_curve(model_list = my_model_list,
#'                validation_data = my_validation_data,
#'                group_col = "group",
#'                palette_name = "AsteroidCity1",
#'                save_plots = TRUE,
#'                save_dir = "path/to/save",
#'                plot_width = 7,
#'                plot_height = 7)
#'
#' @details
#' This function evaluates the performance of each model by calculating the ROC curve and AUC value. It generates a plot with all the ROC curves of the models and labels each curve with the AUC value and its 95% confidence interval in the legend.
#' The user can choose to save the plot as a PDF file, and the function returns a list containing the ROC objects for further analysis.
plot_roc_curve <- function(model_list,
                           validation_data,
                           group_col = "group",
                           palette_name = "AsteroidCity1",
                           base_size = 14,
                           save_plots = TRUE,
                           save_dir = here("ModelData", "best.model.result"),
                           plot_width = 5,
                           plot_height = 5,
                           alpha = 1) {

  roc_list <- list()
  plot_list <- list()
  auc_results <- numeric()

  for (model_name in names(model_list)) {
    model <- model_list[[model_name]]

    predictions <- predict(model, validation_data, type = "prob")[, 2]

    roc_obj <- roc(validation_data[[group_col]], predictions, levels = c("0", "1"), direction = "<")
    roc_list[[model_name]] <- roc_obj

    auc_value <- auc(roc_obj)
    auc_ci <- ci.auc(roc_obj)
    auc_results[model_name] <- auc_value

    plot_data <- data.frame(
      Specificity = 1 - roc_obj$specificities,
      Sensitivity = roc_obj$sensitivities,
      Dataset = paste0(model_name, " (AUC = ", round(auc_value, 3),
                       ", CI = [", round(auc_ci[1], 3), ", ", round(auc_ci[3], 3), "])")
    )
    plot_list[[model_name]] <- plot_data
  }

  combined_plot_data <- do.call(rbind, plot_list)


  p <- ggplot(combined_plot_data, aes(x = Specificity, y = Sensitivity, color = Dataset)) +
    geom_line(size = 1.25, alpha = alpha) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
    scale_color_manual(values = wes_palette(palette_name)) +
    labs(title = "ROC Curves for Best Model on Training Data",
         subtitle = "Including AUC and 95% Confidence Intervals",
         x = "1 - Specificity",
         y = "Sensitivity",
         color = "Dataset (AUC and CI)") +
    scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1), expand = c(0, 0)) +
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
    ggsave(filename = file.path(save_dir, "roc_curves.pdf"), plot = p,
           width = plot_width, height = plot_height,
           device = "pdf")
    cat("Plot saved to:", file.path(save_dir, "roc_curves.pdf"), "\n")
  }

  print(auc_results)

  return(roc_list)
}


#' Check the Number of Levels in the Factor Variable
#'
#' This function checks if the specified column in the provided dataset has at least two distinct levels. It is used to ensure that the validation data contains more than one class, which is a requirement for classification tasks.
#'
#' @param data A data frame or tibble containing the dataset.
#' @param group_col The name of the column to check, which should represent class labels.
#'
#' @returns TRUE if the column contains at least two levels, otherwise stops with an error message.
#' @export
#'
#' @examples
#' # Check if the 'group' column in the dataset 'my_data' contains at least two levels
#' check_factor_levels(data = my_data, group_col = "group")
#'
#' @details
#' This function ensures that the column representing class labels (`group_col`) has at least two levels, which is essential for classification problems. If the column has fewer than two levels, the function stops and throws an error indicating that the validation data is not suitable for classification.
check_factor_levels <- function(data, group_col) {
  levels_present <- levels(as.factor(data[[group_col]]))

  if (length(levels_present) < 2) {
    stop("Validation data must contain at least two class levels.")
  }

  return(TRUE)
}


#' Train and Evaluate Multiple Models for Classification Tasks
#'
#' This function trains and evaluates multiple classification models on a given dataset. It supports various machine learning algorithms and provides performance evaluation including ROC curves. The function also handles model tuning, parallel processing, and saves visualizations of model performance if desired.
#'
#' @param object The input object, either of class 'Model_data' or a list containing 'train' and 'test' elements (representing training and testing datasets).
#' @param methods A character vector specifying the classification methods to use. Default includes models such as "gbm", "rf", "svmLinear", "svmRadial", and "glmnet".
#' @param control A list of control parameters for training, such as cross-validation settings. Default is a 10-fold repeated cross-validation (5 repeats).
#' @param tune_grids A list of tuning grids, specifying the hyperparameter values for each model to be trained.
#' @param classProbs A logical value indicating whether to compute class probabilities. Default is TRUE.
#' @param verboseIter A logical value indicating whether to show progress during model training. Default is FALSE.
#' @param allowParallel A logical value indicating whether to allow parallel processing during model training. Default is FALSE.
#' @param group_col The name of the column in the dataset representing the group labels (e.g., class labels for classification). Default is "group".
#' @param palette_name The name of the color palette to use for plotting. Default is "AsteroidCity1".
#' @param base_size The base font size for plots. Default is 14.
#' @param save_plots A logical value indicating whether to save the plots generated during the analysis. Default is TRUE.
#' @param save_dir The directory where plots will be saved. Default is "ModelData/best_model_result".
#' @param plot_width The width of the plots saved. Default is 5.
#' @param plot_height The height of the plots saved. Default is 5.
#' @param seed A seed value for random number generation, ensuring reproducibility of results. Default is 123.
#' @param alpha Transparency level for the plot lines. Default is 0.8.
#'
#' @returns A 'Model_data' object or a list containing:
#'   - 'train_performance': A data frame containing performance metrics for the trained models on the training dataset.
#'   - 'roc_list': A list of ROC curve objects for each model, generated on the test dataset.
#' If the input object is of class 'Model_data', the function updates this object with the results and returns it. Otherwise, a list is returned.
#' @export
#'
#' @examples
#' # Train and evaluate models using a 'Model_data' object
#' result <- ModelTrainAnalysis(object = model_data_object)
#'
#' # Train and evaluate models using a list of train and test datasets
#' result <- ModelTrainAnalysis(object = list(train = train_data, test = test_data))
#'
#' @details
#' This function is useful for automating the process of training, tuning, and evaluating multiple machine learning models for classification tasks. It provides ROC curves for model evaluation and ranks models based on accuracy. The function can handle parallel processing (if specified) and can save visualizations (e.g., ROC curves) in a specified directory.
ModelTrainAnalysis <- function(object,
                               methods = c("gbm", "rf", "svmLinear", "svmRadial", "glmnet"),
                               control = list(method = "repeatedcv", number = 10,
                                              repeats = 5),
                               tune_grids = list(
                                 gbm = expand.grid(
                                   n.trees = c(50, 100),
                                   interaction.depth = c(2, 3),
                                   shrinkage = c(0.001, 0.01),
                                   n.minobsinnode = c(10, 20)
                                 ),
                                 rf = expand.grid(
                                   mtry = c(2, 3, 4, 5)
                                 ),
                                 svmLinear = expand.grid(
                                   C = c(0.01, 0.1, 1)
                                 ),
                                 svmRadial = expand.grid(
                                   sigma = c(0.01, 0.05, 0.1),
                                   C = c(0.1, 1)
                                 ),
                                 glmnet = expand.grid(
                                   alpha = c(0.1, 0.5, 0.9),
                                   lambda = 10^seq(-4, -1, 1)
                                 )
                               ),
                               classProbs = TRUE, verboseIter = FALSE,
                               allowParallel = F, group_col = "group",
                               palette_name = "AsteroidCity1",
                               base_size = 14,
                               save_plots = TRUE,
                               save_dir = here("ModelData", "best_model_result"),
                               plot_width = 5,
                               plot_height = 5,
                               seed=123,
                               alpha = 0.8) {

  set.seed(seed)

  if (inherits(object, "Model_data")) {
    cat("Input is of class 'Model_data'. Extracting datasets...\n")
    data_sets <- Extract_filtered.set(object)
    train_data <- data_sets$training
    test_data <- data_sets$testing
    group_col <- object@group_col
  } else if (is.list(object) && all(c("train", "test") %in% names(object))) {
    cat("Input is a list with 'train' and 'test' elements.\n")
    train_data <- object$train
    test_data <- object$test
  } else {
    stop("Input must be an object of class 'Model_data' or a list with 'train' and 'test' elements")
  }

  cat("Data extracted. Checking factor levels in the training data...\n")
  check_factor_levels(data=train_data, group_col=group_col)

  cat("Training and evaluating models...\n")
  model_list <- train_and_evaluate_models(data=train_data,
                                          methods=methods,
                                          control=control,
                                          tune_grids=tune_grids,
                                          classProbs=classProbs,
                                          verboseIter=verboseIter,
                                          allowParallel=allowParallel,
                                          group_col=group_col)

  cat("Evaluating models on the test dataset...\n")
  train_performance <- evaluate_model_performance(data=train_data,
                                                  model_result=model_list,
                                                  group_col =group_col)

  cat("Sorting results by accuracy score...\n")
  train_performance <- train_performance[order(train_performance$accuracy_score, decreasing = TRUE), ]

  cat("Generating ROC curves on test data...\n")
  roc_list <- plot_roc_curve(model_list=model_list,
                             validation_data=train_data,
                             group_col=group_col,
                             palette_name =palette_name,
                             base_size=base_size,
                             save_plots=save_plots,
                             save_dir=save_dir,
                             plot_width=plot_width,
                             plot_height=plot_height,
                             alpha = alpha)

  if (inherits(object, "Model_data")) {
    object@all.results <- train_performance
    object@train.models <- model_list
    cat("Updating 'Model_data' object...\n")
    cat("The 'Model_data' object has been updated with the following slots:\n")
    cat("- 'all.results' slot updated.\n")
    cat("- 'train.models' slot updated.\n")

    return(object)
  } else {
    cat("Returning results as a list...\n")
    return(list(train_performance = train_performance, roc_list = roc_list))
  }

}



#' Plot ROC Curves for the Best Model and Compare AUCs of Training and Testing Sets
#'
#' This function generates ROC curves for the best model using training and testing data,
#' and compares the AUC values for both datasets. The ROC curves are visualized and optionally saved as a PDF.
#'
#' @import stats
#' @import ggplot2
#' @import wesanderson
#' @import here 
#' @param best_model The best trained model, typically a classification model.
#' @param training_data Training dataset containing features and labels.
#' @param testing_data Testing dataset containing features and labels.
#' @param group_col The name of the label column, default is "group".
#' @param palette_name The name of the color palette, default is "AsteroidCity1".
#' @param base_size The base font size for the plot, default is 14.
#' @param save_plots Logical value indicating whether to save the plot. Default is TRUE.
#' @param save_dir The directory where the plot will be saved, default is "ModelData/best_model_result".
#' @param plot_width The width of the plot when saved, default is 5.
#' @param plot_height The height of the plot when saved, default is 5.
#' @param alpha The transparency of the plot elements, default is 1 (completely opaque).
#' @param subtitle The subtitle of the plot, default is "Training and Testing Datasets".
#'
#' @returns A list containing the ROC plot data for the training and testing datasets.
#' @export
#'
#' @examples
#' plot_best_model_roc(best_model = trained_model, training_data = train_data, testing_data = test_data)
plot_best_model_roc <- function(best_model,
                                training_data = NULL,
                                testing_data = NULL,
                                group_col = "group",
                                palette_name = "AsteroidCity1",
                                base_size = 14,
                                save_plots = TRUE,
                                save_dir = here("ModelData", "best_model_result"),
                                plot_width = 5,
                                plot_height = 5,
                                alpha = 1,
                                subtitle = "Training and Testing Datasets") {

  plot_data_list <- list()

  if (!is.null(training_data)) {
    training_predictions <- predict(best_model, newdata = training_data, type = "prob")[, 2]
    roc_training <- roc(training_data[[group_col]], training_predictions, levels = c("0", "1"), direction = "<")

    auc_training <- auc(roc_training)
    auc_ci_training <- ci.auc(roc_training)
    training_plot_data <- data.frame(
      Specificity = 1 - roc_training$specificities,
      Sensitivity = roc_training$sensitivities,
      Dataset = paste0("Training Set (AUC = ", sprintf("%.3f", auc_training),
                       " ± ", sprintf("%.3f", (auc_ci_training[3]-auc_ci_training[1])/2), ")")
    )

    plot_data_list$training <- training_plot_data
  }

  if (!is.null(testing_data)) {
    testing_predictions <- predict(best_model, newdata = testing_data, type = "prob")[, 2]
    roc_testing <- roc(testing_data[[group_col]], testing_predictions, levels = c("0", "1"), direction = "<")

    auc_testing <- auc(roc_testing)
    auc_ci_testing <- ci.auc(roc_testing)
    testing_plot_data <- data.frame(
      Specificity = 1 - roc_testing$specificities,
      Sensitivity = roc_testing$sensitivities,
      Dataset = paste0("Testing Set (AUC = ", sprintf("%.3f", auc_testing),
                       " ± ", sprintf("%.3f", (auc_ci_testing[3]-auc_ci_testing[1])/2), ")")
    )

    plot_data_list$testing <- testing_plot_data
  }

  combined_plot_data <- do.call(rbind, plot_data_list)
  combined_plot_data$Dataset <- factor(combined_plot_data$Dataset,
                                       levels = c(unique(plot_data_list$training$Dataset),
                                                  unique(plot_data_list$testing$Dataset)))
  p <- ggplot(combined_plot_data, aes(x = Specificity, y = Sensitivity, color = Dataset)) +
    geom_line(size = 1.25, alpha = alpha) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
    scale_color_manual(values = wes_palette(palette_name)) +
    labs(
      title = "ROC Curves Validation Comparison",
      x = "1 - Specificity",
      y = "Sensitivity",
      color = "Validation Cohort"
    ) +
    scale_x_continuous(
      breaks = seq(0, 1, 0.2),
      limits = c(0, 1),
      expand = expansion(mult = 0.01)
    ) +
    scale_y_continuous(
      breaks = seq(0, 1, 0.2),
      limits = c(0, 1),
      expand = expansion(mult = 0.01)
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      legend.position = c(0.95, 0.05),
      legend.justification = c(1, 0),
      legend.background = element_rect(fill = alpha("white", 0.8)),
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 8),
      panel.grid.major = element_line(color = "grey90"),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )

  print(p)

  if (save_plots) {
    if (!dir.exists(save_dir)) {
      dir.create(save_dir, recursive = TRUE)
    }
    ggsave(filename = file.path(save_dir, "best_model_roc_plot.pdf"),
           plot = p,
           width = plot_width,
           height = plot_height,
           device = "pdf")
    cat("Plot saved to:", file.path(save_dir, "best_model_roc_plot.pdf"), "\n")
  }

  return(plot_data_list)
}

#' Model Evaluation and ROC Curve Plotting for Best Model
#'
#' This function identifies the best model based on a specified performance metric from an object of class 'Model_data',
#' evaluates its performance on the test dataset, and plots the ROC curve for both training and testing datasets.
#' Optionally, the plot can be saved to a specified directory.
#' @import methods
#' @import stats
#' @import here 
#' @param object An object of class 'Model_data' containing training models and evaluation results.
#' @param group_col The name of the label column in the dataset, default is "group".
#' @param palette_name The name of the color palette to use in the ROC plot, default is "AsteroidCity1".
#' @param base_size The base font size for the plot, default is 14.
#' @param save_plots Logical value indicating whether to save the plot. Default is TRUE.
#' @param save_dir The directory where the plot will be saved, default is "ModelData/best_model_result".
#' @param plot_width The width of the saved plot, default is 5.
#' @param plot_height The height of the saved plot, default is 5.
#' @param alpha The transparency of plot elements, default is 1 (completely opaque).
#' @param metric The metric used to select the best model (e.g., "auc", "accuracy"). Default is "auc".
#' @param metric_function The function used to determine the best model based on the metric (e.g., `max` for AUC, `min` for error). Default is `max`.
#'
#' @returns The updated 'Model_data' object, with the 'best.model.result' slot updated with the best model and its test result.
#' @export
#'
#' @examples
#' updated_object <- ModelBestRoc(object = model_data_object,
#'                                group_col = "group",
#'                                metric = "auc",
#'                                metric_function = max)
ModelBestRoc <- function(object,
                         group_col = "group",
                         palette_name = "AsteroidCity1",
                         base_size = 14,
                         save_plots = TRUE,
                         save_dir = here("ModelData", "best_model_result"),
                         plot_width = 5,
                         plot_height = 5,
                         alpha = 1,
                         metric = "auc",
                         metric_function = max) {

  if (inherits(object, "Model_data")) {
    cat("Input is of class 'Model_data'. Extracting datasets...\n")

    all.results <- slot(object, "all.results")
    model_list <- slot(object, "train.models")
    group_col  <-  object@group_col
    if (!metric %in% colnames(all.results)) {
      stop("The specified metric is not available in the results.")
    }

    best.model.result <- all.results[which(all.results[[metric]] == metric_function(all.results[[metric]])), ]

    best.model.result <- best.model.result[1, ]

    best_model_type <- best.model.result$Model

    best_model <- model_list[[best_model_type]]

    data_sets <- Extract_filtered.set(object)
    train_data <- data_sets$training
    test_data <- data_sets$testing

    cat("Evaluating the best model on the test dataset...\n")
    test_result <- evaluate_model_performance(data=test_data,
                                              model_result=best_model,
                                              group_col =group_col)

    cat("Plotting ROC curve for the best model...\n")
    roc_results <- plot_best_model_roc(
      best_model = best_model,
      training_data = train_data,
      testing_data = test_data,
      group_col = group_col,
      palette_name = palette_name,
      base_size = base_size,
      save_plots = save_plots,
      save_dir = save_dir,
      plot_width = plot_width,
      plot_height = plot_height,
      alpha = alpha
    )

    object@best.model.result <- list(model = best_model,
                                     test_result = test_result)
    cat("Updating 'Model_data' object...\n")
    cat("The 'Model_data' object has been updated with the following slots:\n")
    cat("- 'best.model.result' slot updated.\n")

    return(object)
  } else {
    stop("Input must be an object of class 'Model_data'")
  }
}
