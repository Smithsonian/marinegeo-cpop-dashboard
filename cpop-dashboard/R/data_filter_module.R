# Module filters a table based on user input
# There will be a module for each type of data loaded into the app
# For now: MET, Water Quality, Water Level

table_control_server <- function(id, df, selected_parameters){
  
  moduleServer(id, function(input, output, session) {
    
    # Filter data frame based on selected time interval
    date_filtered_df <- reactive({
      
      if(selected_parameters$date_interval == "Previous 7 days"){
        df %>%
          filter(timestamp >= max(timestamp) - weeks(1))
        
      } else if(selected_parameters$date_interval == "All data"){
        df
        
      } else if(selected_parameters$date_interval == "Previous 24 hours"){
        df %>%
          filter(timestamp >= max(timestamp) - hours(24))
        
      } else if(selected_parameters$date_interval == "Previous month"){
        df %>%
          filter(timestamp >= max(timestamp) - months(1))
      }
      
    })
    
    reactive({
      
      if(any(selected_parameters$vars %in% colnames(df))){
        date_filtered_df() %>%
          filter(site_code %in% selected_parameters$sites) %>%
          mutate(site_code = as.factor(site_code)) %>%
          select(site_code, timestamp, any_of(selected_parameters$vars)) %>%
          pivot_longer(cols = any_of(selected_parameters$vars),
                       names_to = "variable", values_to = "value", values_drop_na = TRUE)
      } else {
        NULL
      }

    })
  })
  
}