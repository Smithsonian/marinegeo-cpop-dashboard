# Modules for sensor plotting

# Each module will contain a plotly object,
# and inputs for site and attribute

sensorUI <- function(id, initial_selection) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(id = "plot_row",
      column(width = 3,
             
             tags$br(),

             selectInput(ns("site"), "Site",
                         choices = c("PAN-BDT", "USA-MDA"),
                         selected = initial_selection),
             
             uiOutput(ns("attribute")),
             uiOutput(ns("start_date")),
             
             selectInput(ns("date_interval"), label = "Select a date interval", 
                         choices = c("Previous 7 days", "Previous month", "Previous 24 hours", "All data"))
      ),
      
      column(width = 9, plotlyOutput(NS(id, "sensor_plot"))
      )
    ),
    tags$style(HTML("#plot_row {border-bottom: 2px solid black; padding-bottom: 20px}"))
  )
}

sensorServer <- function(id, initial_selection) {
  moduleServer(id, function(input, output, session) {
    
    current_site_data <- reactiveValues(df = data.frame(),
                                        names = setNames(rosetta$sensor_names, rosetta$display_names))
    
    observeEvent(input$site, {
      if(input$site == "PAN-BDT"){
        current_site_data$df <- pan_bdt_df
        current_site_data$names <- pan_bdt_names
        
      } else if(input$site == "USA-MDA"){
        current_site_data$df <- usa_mda_df
        current_site_data$names <- usa_mda_names
        
      }
    })
    
    output$attribute <- renderUI({
      selectInput(session$ns("attribute"), "Variable",
                  choices = current_site_data$names,
                  selected = initial_selection)
    })
    
    output$start_date <- renderUI({
      dateInput(session$ns("start_date"), label = "Update start date", 
                value = min(current_site_data$df$timestamp), 
                min = min(current_site_data$df$timestamp), max = max(current_site_data$df$timestamp))
    })
    
    date_range <- reactive({
      
      if(input$date_interval == "Previous 7 days"){
        return(c(as.Date(max(current_site_data$df$timestamp)) - weeks(1), 
                 as.Date(max(current_site_data$df$timestamp))))
      } else if(input$date_interval == "All data"){
        return(c(as.Date(min(current_site_data$df$timestamp)),
                 as.Date(max(current_site_data$df$timestamp))))
      } else if(input$date_interval == "Previous 24 hours"){
        return(c(as.Date(max(current_site_data$df$timestamp)) - hours(24), 
                 as.Date(max(current_site_data$df$timestamp))))
      } else if(input$date_interval == "Previous month"){
        return(c(as.Date(max(current_site_data$df$timestamp)) - months(1), 
                 as.Date(max(current_site_data$df$timestamp))))
      }
    })
    
    output$sensor_plot <- renderPlotly({
      
      req(input$attribute)
      
      plot_ly(current_site_data$df, x = ~timestamp, y = ~get(input$attribute), 
              type = "scatter") %>%
        layout(yaxis = list(title = input$attribute),
               xaxis = list(title = "", 
                            range = date_range()
               )) %>%
        toWebGL()  # Conversion from SVG drastically improves performance
    })
  })
}

