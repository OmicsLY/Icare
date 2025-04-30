.onAttach <- function(libname, pkgname) {
  needed_packages <-c("dplyr", "tidyr", "reshape2", "mice", "recipes", "autoReg", "survival", "glmnet",
                      "plsRcox", "CoxBoost", "superpc", "randomForestSRC", "MASS", "car", "nortest",
                      "outliertree", "boot", "timeROC", "maxstat", "rmda", "Hmisc", "infotheo",
                      "minerva", "Information", "pROC", "survcomp", "ggplot2", "ggridges", "ggprism",
                      "gridExtra", "RColorBrewer", "wesanderson", "paletteer", "ggsignif", "pheatmap",
                      "corrplot", "factoextra", "survminer", "plotly", "patchwork", "ggtext",
                      "flextable", "officer", "grid", "caret", "doParallel",  "lightgbm",
                      "kernelshap", "shapviz", "Rtsne", "umap", "NMF", "here", "webshot", "progress",
                      "stringr", "snowfall", "rms", "ROSE", "scales")

  missing <- needed_packages[!(needed_packages %in% installed.packages()[,"Package"])]
  if (length(missing)) {
    packageStartupMessage(paste("Missing packages detected:", paste(missing, collapse = ", ")))
    packageStartupMessage("Please install them manually using install.packages().")
  }
}
.onLoad <- function(libname, pkgname) {
  # 使用非标准评估防止覆盖
  assign(":::", `:::`)
  assign("%:::%", function(pkg, name) get(name, envir = asNamespace(pkg)))

  # 定义绝对安全的filter
  assign("filter",
         function(.data, ...) {
           `%:::%`("dplyr", "filter")(.data, ...)
         },
         envir = parent.env(environment()))

  # 锁定所有关键绑定
  lockBinding("filter", parent.env(environment()))
  lockBinding(":::", environment())
  lockBinding("%:::%", environment())
}

.pkg_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # 强制覆盖全局filter绑定
  if (exists("filter", envir = parent.env(environment()))) {
    rm("filter", envir = parent.env(environment()))
  }

  # 定义包专属filter实现
  assign("filter",
         function(.data, ...) {
           if (!requireNamespace("dplyr", quietly = TRUE))
             stop("dplyr package required")
           getExportedValue("dplyr", "filter")(.data, ...)
         },
         envir = parent.env(environment()))

  # 锁定绑定防止被覆盖
  lockBinding("filter", parent.env(environment()))

  # 延迟加载stats函数
  assign("stats_filter",
         function(x, filter, method = "convolution") {
           if (!requireNamespace("stats", quietly = TRUE))
             stop("stats package required")
           getExportedValue("stats", "filter")(x, filter, method = method)
         },
         envir = .pkg_env)

  # 确保环境隔离
  lockEnvironment(.pkg_env)
}
