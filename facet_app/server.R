# An R-Shiny app to visualize and download MarineGEO CPOP data. 
# Currently the application displays the most recent water quality data from MarineGEO's LoggerNet server. 
# Ultimately, this application will allow users to display, query, and download chemical and physical sensor data including water quality and meterological data across a range of quality control levels, from raw and unprocessed to highly curated.  
# Server script 

function(input, output) {

  current_data <- reactiveValues(df = pan_bdt_df)
  
  # Update data type categories available to input whenever site selection input is updated
  updateDataTypeAvailability <- reactive({
    index %>%
      filter(site_code %in% input$site_selection) %>%
      select(data_type) %>%
      distinct() %>%
      pull(data_type)
  })
  
  output$data_type <- renderUI({
    checkboxGroupInput("data_type", "Select data types",
                       choices = updateDataTypeAvailability(), selected = first(updateDataTypeAvailability()))
  })
  
  # Update variables available to input whenever data types or sites are updated
  updateVariableAvailability <- reactive({
    plotting_variables %>%
      filter(site_code %in% input$site_selection) %>%
      pull(mgeo_cpop_variable_R, name = display_name)
  })
  
  output$var_selection <- renderUI({
    selectInput("var_selection", "Select variables to plot",
                choices = updateVariableAvailability(), 
                selected = first(updateVariableAvailability()), multiple = TRUE)
  })
  
  date_range <- reactive({
    
    if(input$date_interval == "Previous 7 days"){
      return(c(as.Date(max(current_data$df$timestamp)) - weeks(1), 
               as.Date(max(current_data$df$timestamp))))
    } else if(input$date_interval == "All data"){
      return(c(as.Date(min(current_data$df$timestamp)),
               as.Date(max(current_data$df$timestamp))))
    } else if(input$date_interval == "Previous 24 hours"){
      return(c(as.Date(max(current_data$df$timestamp)) - hours(24), 
               as.Date(max(current_data$df$timestamp))))
    } else if(input$date_interval == "Previous month"){
      return(c(as.Date(max(current_data$df$timestamp)) - months(1), 
               as.Date(max(current_data$df$timestamp))))
    }
  })
  
  output$plot_object <- renderPlotly({
    
    req(input$var_selection)
    
    plot_list <- list()
    
    for(site in input$site_selection){
      for(variable in input$var_selection){
        df <- water_quality_df %>%
          filter(site_code == site) 
        
        plot_list[[paste0(site, variable)]] <- plot_ly(
          df, x = ~timestamp, y = ~get(variable), type = "scatter"
        ) %>%
          layout(yaxis = list(title = variable),
                 xaxis = list(title = "", range = date_range())) %>%
          toWebGL()
      }  
    }
    
    # plot_list <- list(
    #   plot_ly(pan_bdt_df, x = ~timestamp, y = ~get(input$var_selection), 
    #           type = "scatter") %>%
    #     layout(yaxis = list(title = input$var_selection),
    #            xaxis = list(title = "", range = date_range())) %>%
    #     toWebGL(),  
    #   
    #   plot_ly(usa_mda_df, x = ~timestamp, y = ~get(input$var_selection), 
    #           type = "scatter") %>%
    #     layout(yaxis = list(title = input$var_selection),
    #            xaxis = list(title = "", range = date_range())) %>%
    #     toWebGL()
    # )
    
    subplot(plot_list, nrows = 2, titleY = TRUE)
  })
}
