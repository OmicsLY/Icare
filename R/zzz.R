.onAttach <- function(libname, pkgname) {
  # 检查并提示安装建议包
  if (!requireNamespace("conflicted", quietly = TRUE)) {
    packageStartupMessage(
      "For better namespace management, consider installing:\n",
      "  install.packages(\"conflicted\")"
    )
  }
}

.onLoad <- function(libname, pkgname) {
  # 检查Bioconductor依赖但不自动安装
  required_bioc_pkgs <- c("CoxBoost", "superpc", "BiocGenerics",
                          "BiocParallel", "mixOmics", "survcomp")
  missing_pkgs <- required_bioc_pkgs[
    !sapply(required_bioc_pkgs, requireNamespace, quietly = TRUE)
  ]

  if (length(missing_pkgs) > 0) {
    packageStartupMessage(
      "This package requires Bioconductor packages:\n",
      paste(missing_pkgs, collapse = ", "), "\n",
      "Install with: BiocManager::install(c(\"",
      paste(missing_pkgs, collapse = "\", \""), "\"))"
    )
  }

  # 更安全的filter冲突解决方案
  if (requireNamespace("dplyr", quietly = TRUE)) {
    # 使用dplyr的filter
    utils::globalVariables("filter")
    filter <- dplyr::filter
  } else {
    # 回退到基本R的filter
    filter <- stats::filter
    packageStartupMessage(
      "dplyr not available, using stats::filter instead.\n",
      "For dplyr functionality, install with: install.packages(\"dplyr\")"
    )
  }

  # 显式导出函数
  if (is.null(getOption("Icare.export_filter"))) {
    options(Icare.export_filter = TRUE)
    assign("filter", filter, envir = parent.env(environment()))
  }

  # 推荐使用conflicted处理命名空间冲突
  if (requireNamespace("conflicted", quietly = TRUE)) {
    conflicted::conflict_prefer("filter", "dplyr", quiet = TRUE)
  }
}
