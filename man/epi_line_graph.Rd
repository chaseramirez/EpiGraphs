% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epi_line_graph.R
\name{epi_line_graph}
\alias{epi_line_graph}
\title{Line Graph for Epidemiological Data}
\usage{
epi_line_graph(
  data,
  x,
  y,
  group = NULL,
  period_var = NULL,
  start_period = NULL,
  end_period = NULL,
  filter_loc = NULL,
  loc_column = NULL,
  disease_name = "Disease"
)
}
\arguments{
\item{data}{The dataset containing the variables.}

\item{x}{The time variable for the x-axis (e.g., year).}

\item{y}{The numeric variable for the y-axis (e.g., number of cases).}

\item{group}{The variable for grouping lines (optional; defaults to filtering for a single location).}

\item{period_var}{The column name for the period variable (e.g., "Year").}

\item{start_period}{The start period for filtering data (optional).}

\item{end_period}{The end period for filtering data (optional).}

\item{filter_loc}{The specific location to filter for.}

\item{loc_column}{The column name where countries are listed.}

\item{disease_name}{The name of the disease to include in the title (e.g., "Congenital Syphilis").}
}
\value{
A ggplot object representing the line graph.
}
\description{
Creates a line graph for epidemiological data, with filtering for a specific location and period range.
}
