# An R-Shiny app to visualize and download MarineGEO CPOP data. 
# Currently the application displays the most recent water quality data from MarineGEO's LoggerNet server. 
# Ultimately, this application will allow users to display, query, and download chemical and physical sensor data including water quality and meterological data across a range of quality control levels, from raw and unprocessed to highly curated.  
# Server script 

function(input, output, session) {
  
  # Store all inputs in reactive values object 
  # RV gets passed to each module
  selected_parameters <- reactiveValues(sites = NA, data_type = NA, vars = NA, date_interval = initial_date_range_value)

  observe({
    selected_parameters$sites <- input$site_selection
    selected_parameters$data_type <- input$data_type
    selected_parameters$vars <- input$var_selection
    
    if(!is.null(input$date_interval)){
      selected_parameters$date_interval <- input$date_interval
    }
    
  })
  
  ## Reactives for updating inputs ####
  # Update data type categories available to input whenever site selection input is updated
  updateDataTypeAvailability <- reactive({
    index %>%
      filter(site_code %in% input$site_selection) %>%
      select(data_type) %>%
      distinct() %>%
      pull(data_type)
  })
  
  getUpdatedSelections <- reactive({
    
    var_list_df <- plotting_variables %>%
      filter(data_type %in% input$data_type,
             site_code %in% input$site_selection) %>%
      group_by(mgeo_cpop_variable_R, display_name) %>%
      summarize(n_sites = n(), sites = paste(site_code, collapse = ", ")) %>%
      mutate(column_header = case_when(
        n_sites == num_sites ~ "All sites",
        T ~ sites
      )) %>%
      arrange(column_header)
    
    var_list <- list()
    for(i in unique(var_list_df$column_header)){
      var_list[[i]] <- var_list_df %>%
        filter(column_header == i) %>%
        pull(mgeo_cpop_variable_R, name = display_name)
    }
    
    return(var_list)
  })
  
  observeEvent(input$site_selection, {
    checkboxGroupInput("data_type", "Data types",
                       choices = updateDataTypeAvailability(), selected = input$data_type)
    
    updateSelectInput(session, "var_selection", choices = getUpdatedSelections(), selected = input$var_selection)

  }, ignoreInit = TRUE)

  observeEvent(input$data_type, {
    updateSelectInput(session, "var_selection", choices = getUpdatedSelections(), selected = input$var_selection)

  }, ignoreInit = TRUE)
  
  
  # Render UI for date selection modes
  output$date_selection <- renderUI({
    
    if(!input$toggle_date_mode){
      selectInput("date_interval", label = "Date interval", 
                  choices = c("Previous 7 days", "Previous month", "Previous 24 hours", "All data"))
    } else {
      dateRangeInput("date_interval", label = "Date range")
    }
    
  })
  
  ## Data table modules and plot ####
  water_quality_module <- table_control_server("wq", water_quality_df, selected_parameters)
  met_module <- table_control_server("met", met_df, selected_parameters)
  water_level_module <- table_control_server("wl", water_level_df, selected_parameters)
  
  table_unification <- reactive({
    # Bound selected data tables together and remove NULL list items
    dat_list <- compact(list(
      "Water Quality" = water_quality_module(),
      "Meteorological" = met_module(),
      "Water Level" = water_level_module()
    ))
    
    # Combine (row-wise) all dataframes included in list and return to plot
    bind_rows(dat_list)
  })
  
  # Single zoomable plot (on left)
  # Disabled y range, all facets will only be zoomed by x axis
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot_dblclick, {
    
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(as.POSIXct(brush$xmin, origin = "1970-01-01"), as.POSIXct(brush$xmax, origin = "1970-01-01"))
    } else {
      ranges$x <- NULL
    }
  })
  
  output$plot_object <- renderPlot({

    #tryCatch({
      input$update_plot
      input$plot_dblclick
      
      isolate({

        plot <- table_unification() %>%
          ggplot(aes(timestamp, value, color = site_code)) +
          geom_point() +
          theme_minimal() + labs(x = "", y = "", color = "Site Code") +
          coord_cartesian(xlim = ranges$x) +
          theme(legend.position = "top",
                legend.title = element_text(size = 15),
                legend.text = element_text(size = 15),
                strip.text = element_text(size = 15),
                axis.text = element_text(size = 12),
                panel.spacing.y = unit(5, "lines")) +
          guides(colour = guide_legend(override.aes = list(size=5)))

        if(length(input$site_selection) == 1){
          plot + facet_wrap(~variable, scales = "free_y", ncol = 1,
                            labeller = labeller(variable = formatted_plot_variables))

        } else if(length(input$var_selection) == 1){
          plot + facet_wrap(~site_code, scales = "free_y", ncol = 1,
                            labeller = labeller(site_code = setNames(
                              rep(unname(formatted_plot_variables[names(formatted_plot_variables) == input$var_selection]),
                                  length(input$site_selection)),input$site_selection)))

        } else{
          # Not sure this makes much sense.. might as well just combine all sites into a single plot
          plot + facet_grid(variable ~ site_code, scales = "free_y", 
                            labeller = labeller(variable = formatted_plot_variables[names(formatted_plot_variables) %in% input$var_selection]))
        }

      })

  #  }, error = function(e) e)
  }, height = 600)

  ## About this data link ####
  observeEvent(input$about, {
    showModal(modalDialog(
      title = "About",
      div(
        "All data available is raw, unprocessed data (Level 0). ",  
        "Data processed according to quality control protocols will be available in this application soon. "
      ),
      easyClose = TRUE
    ))
    
  })

  
  ## Download ####
  # Module server for packaging download and serving as a zip folder
  download_server("download", df_list, selected_parameters, data_dictionary)
  
}
