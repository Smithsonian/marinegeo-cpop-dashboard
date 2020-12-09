# Modules for sensor plotting

# Each module will contain a plotly object,
# and inputs for site and attribute

sensorUI <- function(id, site, site_attribute_names) {
  tagList(
    fluidRow(id = "plot_row",
      column(width = 3,
             tags$br(), tags$h4(site), tags$br(), 
             selectInput(NS(id, "attribute"), "Variable",
                         choices = setNames(site_attribute_names$sensor_names, site_attribute_names$display_names),
                         selected = "WaterTemp")),
      column(width = 9, plotlyOutput(NS(id, "sensor_plot"))
      )
    ),
    tags$style(HTML("#plot_row {border-bottom: 2px solid black; padding-bottom: 20px}"))
  )
}

sensorServer <- function(id, site_df) {
  moduleServer(id, function(input, output, session) {
    output$sensor_plot <- renderPlotly({
      plot_ly(site_df, x = ~timestamp, y = ~get(input$attribute), 
              type = "scatter") %>%
        layout(yaxis = list(title = input$attribute),
               xaxis = list(title = "", 
                            range = c(max(site_df$timestamp) - weeks(2), max(site_df$timestamp))
                            # Function does not work well with datetime marker plots
                            # See: https://github.com/plotly/plotly.js/issues/2209
                            # rangeselector = list(
                            #   buttons = list(
                            #     list(count = 24, label = "1 day", step = "hour", stepmode = "backward"),
                            #     list(count = 7, label = "1 wk", step = "day", stepmode = "backward"),
                            #     list(count = 14, label = "2 wk", step = "day", stepmode = "backward"),
                            #     list(count = 1, label = "1 mo", step = "month", stepmode = "backward"),
                            #     list(step = "all")
                            #   ))
                            )) %>%
      toWebGL()  # Conversion from SVG drastically improves performance
    })
  })
}

