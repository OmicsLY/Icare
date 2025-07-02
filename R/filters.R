#' @title 安全过滤函数
#' @description 避免与 stats::filter 冲突的过滤实现
#' @export
filter <- function(.data, ...) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("请先安装 dplyr 包: install.packages('dplyr')")
  }
  dplyr::filter(.data, ...)
}

#' @title 时间序列过滤
#' @description stats::filter 的安全替代
#' @export
ts_filter <- function(x, filter, method = "convolution") {
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("stats 包未加载")
  }
  stats::filter(x, filter, method = method)
}
