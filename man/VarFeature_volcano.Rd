% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-4.R
\name{VarFeature_volcano}
\alias{VarFeature_volcano}
\title{Generate Volcano Plot for Variable Features}
\usage{
VarFeature_volcano(
  object,
  logFC_col = "logFC",
  p_adjust_col = "p.adjust",
  title = "Volcano Plot",
  palette_name = "Zissou1",
  plot_width = 5,
  plot_height = 5,
  save_dir = here("StatObject", "deg_info"),
  base_size = 14
)
}
\arguments{
\item{object}{An object of class 'Stat' or a data frame containing the results of differential expression analysis.
It should include columns for log-fold change and adjusted p-values.}

\item{logFC_col}{The name of the column representing log-fold change values (default: "logFC").}

\item{p_adjust_col}{The name of the column representing adjusted p-values (default: "p.adjust").}

\item{title}{The title of the volcano plot (default: "Volcano Plot").}

\item{palette_name}{A string indicating the color palette to use for the plot.}

\item{plot_width}{The width of the plot in inches (default: 5).}

\item{plot_height}{The height of the plot in inches (default: 5).}

\item{save_dir}{The directory to save the volcano plot image (default: "here('StatObject', 'deg_info')").}

\item{base_size}{The base font size for the plot (default: 14).}
}
\value{
A \code{ggplot} object representing the volcano plot. If the input is a 'Stat' object, it also updates the
'Stat' object with the volcano plot.
}
\description{
This function generates a volcano plot based on the log-fold change and adjusted p-values of variable features from
a 'Stat' object or a data frame. It then saves the volcano plot as an image in the specified directory.
}
\examples{
# Example usage:
VarFeature_volcano(object = stat_object, logFC_col = "logFC", p_adjust_col = "p.adjust")

VarFeature_volcano(object = stat_object, title = "Custom Volcano Plot", palette_name = "Zissou1")
}
