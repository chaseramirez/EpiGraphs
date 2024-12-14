server <- function(input, output, session) {
  dataset <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })

  # Generate input controls based on plot type
  output$variable_inputs <- renderUI({
    req(dataset())
    vars <- names(dataset())
    if (input$plot_type == "Bar Plot") {
      tagList(
        selectInput("bar_x", "X-axis Variable", choices = vars),
        selectInput("bar_y", "Y-axis Variable", choices = vars),
        numericInput("top_n", "Top N Values", value = 10, min = 1)
      )
    } else if (input$plot_type == "Line Graph") {
      tagList(
        selectInput("line_x", "X-axis Variable (Time)", choices = vars),
        selectInput("line_y", "Y-axis Variable", choices = vars),
        textInput("line_title", "Title", value = "Interactive Line Graph")
      )
    }
  })

  # Generate the interactive plot
  output$interactive_plot <- renderPlot({
    req(dataset())
    data <- dataset()

    if (input$plot_type == "Bar Plot") {
      # Filter for the top N values
      data <- data %>%
        arrange(desc(!!sym(input$bar_y))) %>%
        slice_head(n = input$top_n)

      ggplot(data, aes_string(x = input$bar_x, y = input$bar_y)) +
        geom_bar(stat = "identity", width = 0.8) +
        ggtitle(paste("Top", input$top_n, "by", input$bar_x)) +
        theme_minimal()
    } else if (input$plot_type == "Line Graph") {
      ggplot(data, aes_string(x = input$line_x, y = input$line_y)) +
        geom_line(size = 1) +
        ggtitle(input$line_title) +
        theme_minimal()
    }
  })
}
