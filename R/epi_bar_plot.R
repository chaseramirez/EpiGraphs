#' Bar Plot for Epidemiological Data
#'
#' Creates a custom bar plot for epidemiological data with filtering by period and top values.
#'
#' @param data The dataset containing the variables.
#' @param x The categorical variable for the x-axis (e.g., country).
#' @param y The numeric variable for the bar heights (e.g., number of cases).
#' @param period_var The column name for the period variable (e.g., "Year").
#' @param period The specific period to filter the data (e.g., 2007).
#' @param top_n The number of top values to display (optional; defaults to all data).
#' @param disease_name The name of the disease to include in the title (e.g., "Congenital Syphilis").
#' @param y_desc A description of the y-axis variable (e.g., "cases", "deaths").
#' @return A ggplot object representing the bar plot.
#' @export
epi_bar_plot <- function(data, x, y, period_var = NULL, period = NULL, top_n = NULL, disease_name = "Disease", y_desc = "values") {
  # Filter by period if specified
  if (!is.null(period_var) && !is.null(period)) {
    data <- data %>% dplyr::filter(!!rlang::sym(period_var) == period)
  }

  # Filter the top_n values if specified
  if (!is.null(top_n)) {
    data <- data %>%
      dplyr::arrange(dplyr::desc(!!rlang::sym(y))) %>%
      dplyr::slice_head(n = top_n)
  }

  # Generate dynamic title
  title <- paste(
    disease_name, "- Top",
    ifelse(is.null(top_n), "All", top_n),
    y_desc, "by", x, "in", period
  )

  # Base plot
  p <- ggplot(data, aes(x = reorder(!!rlang::sym(x), -!!rlang::sym(y)), y = !!rlang::sym(y))) +
    geom_bar(stat = "identity", width = 0.8) + # Bar chart without fill aesthetic
    scale_y_continuous(labels = scales::label_comma()) +  # Format axis labels
    theme_minimal(base_size = 12) +  # Reduced base font size
    theme(
      panel.background = element_rect(fill = "white", color = "grey90"), # Light grey border
      panel.grid.major = element_line(color = "grey80"), # Grey major gridlines
      panel.grid.minor = element_line(color = "grey90"), # Light grey minor gridlines
      axis.line = element_line(color = "black", linewidth = 0.8), # Black axis lines
      axis.text = element_text(size = 10, color = "black"), # Smaller axis text
      axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis text
      axis.title = element_text(size = 12, face = "bold"), # Smaller bold axis titles
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "darkblue") # Centered dark blue title
    ) +
    labs(
      x = paste0(x, "s"),
      y = paste("Number of", y_desc), # Dynamic y-axis description
      title = title
    )

  return(p)
}
