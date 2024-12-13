#' Custom Scatter Plot
#'
#' This function creates a scatter plot with custom color and title.
#'
#' @param data The data frame containing the data
#' @param x The variable for the x-axis
#' @param y The variable for the y-axis
#' @return A ggplot scatter plot
#' @export
custom_scatter_plot <- function(data, x, y) {
  ggplot(data, aes_string(x = x, y = y)) +
    geom_point(color = "darkorange") +
    theme_minimal() +
    labs(title = paste("Scatter Plot of", y, "vs", x), x = x, y = y)
}
