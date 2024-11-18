#' Shiny App for Viewing CDR Job Estimation Results
#'
#' Launches a Shiny app to interactively view CDR job estimation results.
#'
#' @param results A list of datasets to visualize.
#' @examples
#' run_shiny_app(results)
#' @export
run_shiny_app <- function(output_path) {
  library(shiny)
  library(ggplot2)
  library(dplyr)

  # Load the provided CSV files from the user-specified output path
  # Use the output_path provided by the user to dynamically load CSV files
  job_cum_total_data <- read.csv(file.path(output_path, "Job_cum_total.csv"))
  job_total_year_data <- read.csv(file.path(output_path, "Job_total_year.csv"))

  # Create a list of datasets
  results <- list(
    Job_cum_total = job_cum_total_data,
    Job_total_year = job_total_year_data
  )

  ui <- fluidPage(
    titlePanel("CDR Job Estimation Results Viewer"),
    sidebarLayout(
      sidebarPanel(
        selectInput("dataset", "Select Dataset:", choices = if (length(results) > 0) names(results) else "No data available"),
        selectInput("type", "Select Visualization Type:", choices = c("total_year", "cum_tech")),
        selectInput("job_metric", "Select Job Metric:", choices = c("mean_Jobs", "min_Jobs", "max_Jobs")),
        uiOutput("scenario_ui"),
        uiOutput("region_ui"),
        numericInput("ncol", "Number of Columns for Facets:", value = 2, min = 1),
        numericInput("nrow", "Number of Rows for Facets:", value = 2, min = 1)
      ),
      mainPanel(
        plotOutput("jobPlot")
      )
    )
  )

  server <- function(input, output, session) {
    observe({
      if (input$dataset == "No data available") {
        output$jobPlot <- renderPlot({
          plot.new()
          text(0.5, 0.5, "No data available", cex = 1.5)
        })
        return()
      }
    })

    selected_data <- reactive({
      req(input$dataset)
      data <- results[[input$dataset]]
      if (!is.data.frame(data)) {
        data <- as.data.frame(data)
      }
      data
    })

    output$scenario_ui <- renderUI({
      req(selected_data())
      selectInput("selected_scenarios", "Select Scenarios:",
                  choices = unique(selected_data()$scenario),
                  selected = unique(selected_data()$scenario),
                  multiple = TRUE)
    })

    output$region_ui <- renderUI({
      req(selected_data())
      selectInput("selected_regions", "Select Regions:",
                  choices = unique(selected_data()$region),
                  selected = unique(selected_data()$region),
                  multiple = TRUE)
    })

    output$jobPlot <- renderPlot({
      req(selected_data())
      filtered_data <- selected_data()
      if (!is.null(input$selected_scenarios)) {
        filtered_data <- filtered_data[filtered_data$scenario %in% input$selected_scenarios, ]
      }
      if (!is.null(input$selected_regions)) {
        filtered_data <- filtered_data[filtered_data$region %in% input$selected_regions, ]
      }

      visualize_results(
        data = filtered_data,
        type = input$type,
        job_metric = input$job_metric,
        selected_scenarios = input$selected_scenarios,
        selected_regions = input$selected_regions,
        ncol = input$ncol,
        nrow = input$nrow,
        output_path = tempdir()
      )
    })
  }

  shinyApp(ui = ui, server = server)
}
