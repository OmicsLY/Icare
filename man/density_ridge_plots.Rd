% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-2.R
\name{density_ridge_plots}
\alias{density_ridge_plots}
\title{Density Ridge Plots for Numeric Variables}
\usage{
density_ridge_plots(
  data,
  vars_per_plot = 1,
  save_dir = here("StatObject"),
  palette_name = "Zissou1",
  group_col = "group",
  max_unique_values = 5,
  sub_var = NULL,
  save_plots = TRUE,
  plot_width = 5,
  plot_height = 5,
  base_size = 14
)
}
\arguments{
\item{data}{A data frame containing the data to plot.}

\item{vars_per_plot}{Integer. The number of variables (columns) to include
in each individual plot. Default is 5.}

\item{save_dir}{String. The directory to save the plots in. Default is
the directory \code{"StatObject"}.}

\item{palette_name}{String. The name of the color palette to use. The
default is \code{"Royal1"}, which uses a predefined color scheme.}

\item{group_col}{String. The name of the column in the data used to group
the samples. Default is \code{"group"}.}

\item{max_unique_values}{Integer. The maximum number of unique values
allowed for categorical variables. Defaults to 5. Used for filtering
variables when diagnosing their types.}

\item{sub_var}{Character vector. A subset of variable names (columns)
to include in the plot. If \code{NULL}, all numeric variables will be included.}

\item{save_plots}{Logical. If \code{TRUE}, the plots will be saved as PDF
files in the specified \code{save_dir}. Default is \code{TRUE}.}

\item{plot_width}{Numeric. The width of each saved plot (in inches).
Default is 5.}

\item{plot_height}{Numeric. The height of each saved plot (in inches).
Default is 5.}

\item{base_size}{Numeric. The base font size for the plot. Default is 14.}
}
\value{
A list of ggplot objects containing the generated density ridge plots.
If \code{save_plots} is \code{TRUE}, the plots are also saved as PDF files in
the specified directory.
}
\description{
This function generates density ridge plots for numeric variables in the dataset,
with optional grouping by a specified column. It handles large numbers of variables
by splitting them into multiple plots, each containing a set of variables (defined
by the \code{vars_per_plot} argument). The function also allows for customization of plot
appearance and save locations.
}
\examples{
density_ridge_plots(data = my_data,
                    vars_per_plot = 4,
                    save_dir = here("StatObject"),
                    palette_name = "Zissou1",
                    group_col = "group",
                    save_plots = TRUE)

}
