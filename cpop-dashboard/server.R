# An R-Shiny app to visualize and download MarineGEO CPOP data. 
# Currently the application displays the most recent water quality data from MarineGEO's LoggerNet server. 
# Ultimately, this application will allow users to display, query, and download chemical and physical sensor data including water quality and meterological data across a range of quality control levels, from raw and unprocessed to highly curated.  
# Server script 

function(input, output, session) {

  #source("./R/load_data_temporary.R")
  
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
  
  getUpdatedSelections <- reactive({
    plotting_variables %>%
      filter(data_type %in% input$data_type,
             site_code %in% input$site_selection) %>%
      select(mgeo_cpop_variable_R, display_name) %>%
      distinct() %>%
      pull(mgeo_cpop_variable_R, name = display_name)
  })
  
  observeEvent(input$site_selection, {
    updateSelectInput(session, "var_selection", choices = getUpdatedSelections(), selected = input$var_selection)

  }, ignoreInit = TRUE)

  observeEvent(input$data_type, {
    updateSelectInput(session, "var_selection", choices = getUpdatedSelections(), selected = input$var_selection)

  }, ignoreInit = TRUE)
  
  date_filtered_df <- reactive({
    
    if(input$date_interval == "Previous 7 days"){
      joined_df %>%
        filter(timestamp >= max(timestamp) - weeks(1))
      
    } else if(input$date_interval == "All data"){
      joined_df
      
    } else if(input$date_interval == "Previous 24 hours"){
      joined_df %>%
        filter(timestamp >= max(timestamp) - hours(24))
      
    } else if(input$date_interval == "Previous month"){
      joined_df %>%
        filter(timestamp >= max(timestamp) - months(1))
    }
  })
  
  output$plot_object <- renderPlot({

    tryCatch({
      input$update_plot

      isolate({

        plot <- date_filtered_df() %>%
          filter(site_code %in% input$site_selection) %>%
          mutate(site_code = as.factor(site_code)) %>%
          select(site_code, timestamp, input$var_selection) %>%
          pivot_longer(cols = any_of(input$var_selection),
                       names_to = "variable", values_to = "value", values_drop_na = TRUE) %>%
          ggplot(aes(timestamp, value, color = site_code)) +
          geom_point() +
          theme_minimal() + labs(x = "", y = "", color = "Site Code") +
          theme(legend.position = "top",
                legend.title = element_text(size = 15),
                legend.text = element_text(size = 15),
                strip.text = element_text(size = 15),
                axis.text = element_text(size = 12),
                panel.spacing.y = unit(5, "lines")) +
          guides(colour = guide_legend(override.aes = list(size=5)))

        if(length(input$site_selection) == 1){
          plot + facet_wrap(~variable, scales = "free", ncol = 1, 
                            labeller = labeller(variable = formatted_plot_variables))

        } else if(length(input$var_selection) == 1){
          plot + facet_wrap(~site_code, scales = "free", ncol = 1,
                            labeller = labeller(site_code = setNames(
                              rep(unname(formatted_plot_variables[names(formatted_plot_variables) == input$var_selection]), 
                                  length(input$site_selection)),input$site_selection)))

        } else{
          plot + facet_grid(variable ~ site_code, scales = "free")
        }

      })

    }, error = function(e) e)
  }, height = 600)
}
