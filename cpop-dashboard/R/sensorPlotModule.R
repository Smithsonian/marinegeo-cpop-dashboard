# Modules for sensor plotting

# Each module will contain a plotly object,
# and inputs for site and attribute

sensorUI <- function(id) {
  tagList(
    selectInput(NS(id, "attribute"), "Variable",
                choices = setNames(pan_bdt_names$sensor_names, pan_bdt_names$display_names),
                selected = "WaterTemp"),
    plotlyOutput(NS(id, "sensor_plot"))
  )
}

sensorServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$sensor_plot <- renderPlotly({
      plot_ly(pan_bdt_df, x = ~timestamp, y = ~get(input$attribute), 
              type = "scatter") %>%
      toWebGL()  # Conversion from SVG drastically improves performance
    })
  })
}

