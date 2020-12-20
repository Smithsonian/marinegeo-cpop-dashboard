# Modules for sensor plotting

# Each module will contain a plotly object,
# and inputs for site and attribute

sensorUI <- function(id, site, site_attribute_names, site_df) {
  tagList(
    fluidRow(id = "plot_row",
      column(width = 3,
             tags$br(), tags$h4(site), tags$br(), 
             selectInput(NS(id, "attribute"), "Variable",
                         choices = setNames(site_attribute_names$sensor_names, site_attribute_names$display_names),
                         selected = "WaterTemp"),
             
             dateInput(NS(id, "start_date"), label = "Update start date", 
                       value = min(site_df$timestamp), min = min(site_df$timestamp), max = max(site_df$timestamp)),
             
             #dateInput("start_date", label = "Update start date"),
             selectInput(NS(id, "date_interval"), label = "Select a date interval", 
                         choices = c("All data", "1 day", "1 week", "1 month"))
      ),
      
      column(width = 9, plotlyOutput(NS(id, "sensor_plot"))
      )
    ),
    tags$style(HTML("#plot_row {border-bottom: 2px solid black; padding-bottom: 20px}"))
  )
}

sensorServer <- function(id, site_df) {
  moduleServer(id, function(input, output, session) {
    
    date_range_max <- reactive({
      
      if(input$date_interval == "All data"){
        return(max(site_df$timestamp))
      } else if(input$date_interval == "1 day"){
        return(input$start_date + hours(24))
      } else if(input$date_interval == "1 week"){
        return(input$start_date + weeks(1))
      } else if(input$date_interval == "1 month"){
        return(input$start_date + months(1))
      }
    })
    
    output$sensor_plot <- renderPlotly({
      plot_ly(site_df, x = ~timestamp, y = ~get(input$attribute), 
              type = "scatter") %>%
        layout(yaxis = list(title = input$attribute),
               xaxis = list(title = "", 
                            #range = c(max(site_df$timestamp) - weeks(2), max(site_df$timestamp))
                            range = c(input$start_date, as.Date(date_range_max()))
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

