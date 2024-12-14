#' Line Graph for Epidemiological Data
#'
#' Creates a line graph for epidemiological data, with filtering for a specific location and period range.
#'
#' @param data The dataset containing the variables.
#' @param x The time variable for the x-axis (e.g., year).
#' @param y The numeric variable for the y-axis (e.g., number of cases).
#' @param group The variable for grouping lines (optional; defaults to filtering for a single location).
#' @param period_var The column name for the period variable (e.g., "Year").
#' @param start_period The start period for filtering data (optional).
#' @param end_period The end period for filtering data (optional).
#' @param filter_loc The specific location to filter for.
#' @param loc_column The column name where countries are listed.
#' @param disease_name The name of the disease to include in the title (e.g., "Congenital Syphilis").
#' @return A ggplot object representing the line graph.
#' @export
epi_line_graph <- function(data, x, y, group = NULL, period_var = NULL, start_period = NULL, end_period = NULL, filter_loc = NULL, loc_column = NULL, disease_name = "Disease") {
  # Filter for the specific location if specified
  if (!is.null(filter_loc) && !is.null(loc_column)) {
    data <- data %>% dplyr::filter(!!rlang::sym(loc_column) == filter_loc)
  }

  # Filter by period range if specified
  if (!is.null(period_var) && (!is.null(start_period) || !is.null(end_period))) {
    if (!is.null(start_period)) {
      data <- data %>% dplyr::filter(!!rlang::sym(period_var) >= start_period)
    }
    if (!is.null(end_period)) {
      data <- data %>% dplyr::filter(!!rlang::sym(period_var) <= end_period)
    }
  }

  # Check if filtered data is empty
  if (nrow(data) == 0) {
    stop("No data available for the specified period or location.")
  }

  # Generate dynamic title
  title <- paste(
    disease_name, "- Trends in",
    if (!is.null(filter_loc)) filter_loc else "Unknown",
    if (!is.null(start_period) && !is.null(end_period)) paste0("(", start_period, " - ", end_period, ")") else ""
  )

  # Base plot
  p <- ggplot(data, aes(x = !!rlang::sym(x), y = !!rlang::sym(y)))

  # Add group aesthetic if provided
  if (!is.null(group)) {
    p <- p + aes(color = !!rlang::sym(group), group = !!rlang::sym(group))
  }

  # Add line chart and enhanced styling
  p <- p +
    geom_line(linewidth = 1.5) +  # Make the line thicker
    scale_x_continuous(
      breaks = seq(start_period, end_period, by = 1),  # Ensure filtered range on x-axis
      limits = c(start_period, end_period)  # Restrict x-axis range
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "grey90"), # Light grey border
      panel.grid.major = element_line(color = "grey80"), # Grey major gridlines
      panel.grid.minor = element_line(color = "grey90"), # Light grey minor gridlines
      axis.line = element_line(color = "black", linewidth = 0.8), # Black axis lines
      axis.text = element_text(size = 10, color = "black"), # Smaller axis text
      axis.title = element_text(size = 12, face = "bold"), # Smaller bold axis titles
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "darkblue"), # Centered dark blue title
      legend.title = element_text(size = 10, face = "bold"), # Smaller legend title
      legend.text = element_text(size = 10) # Smaller legend text
    ) +
    labs(
      x = x,
      y = "Number of Cases",
      color = group,
      title = title
    )

  return(p)
}
