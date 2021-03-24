download_UI <- function(id){
  ns <- NS(id)
  
  div(id = id,
      downloadButton(ns("download"), "Download selected data")
  )
}


download_server <- function(id, df_list, selected_parameters, data_dictionary){
  
  moduleServer(id, function(input, output, session) {
    
    filter_date <- function(df){
      
      if(is.Date(selected_parameters$date_interval[1])){
        df %>%
          filter(timestamp >= selected_parameters$date_interval[1],
                 timestamp <= selected_parameters$date_interval[2])
        
      } else if(selected_parameters$date_interval == "Previous 7 days"){
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
    }
    
    output$download <- downloadHandler(
      filename = function() {
        "marinegeo_sensor_data_download.zip"
      },
      
      content = function(fname) {
        
        # showModal(modalDialog(
        #   title = "Your download is being prepared",
        #   div(
        #     "It may take a few moments depending on the size of the dataset you have selected."
        #   ),
        #   easyClose = TRUE
        # ))
        
        # Set a temporary directory
        setwd(tempdir())
        
        # # Write readme
        fs <- c()
        
        # Vector of columns included in the download
        included_cols <- c()
        
        if("Water Quality" %in% selected_parameters$data_type){
          df <- filter_date(df_list[["Water Quality"]]) %>%
            dplyr::filter(site_code %in% selected_parameters$sites) %>%
            dplyr::select_if(all_na_test) %>%
            dplyr::select(site_code, timestamp, everything()) %>%
            dplyr::select(-c(Date_MM_DD_YYYY, Time_HH_mm_ss, sn, snn))
          
          path <- "marinegeo_water_quality_data.csv"
          fs <- c(fs, path)
          write_csv(df, path)
          
          included_cols <- c(included_cols, colnames(df))
        }
        
        if("Water Level" %in% selected_parameters$data_type){
          df <- filter_date(df_list[["Water Level"]]) %>%
            dplyr::filter(site_code %in% selected_parameters$sites) %>%
            dplyr::select_if(all_na_test) %>%
            dplyr::select(site_code, timestamp, everything())
          
          path <- "marinegeo_water_level_data.csv"
          fs <- c(fs, path)
          write_csv(df, path)
          
          included_cols <- c(included_cols, colnames(df))
          
        }
        
        if("Meteorological" %in% selected_parameters$data_type){
          df <- filter_date(df_list[["Meteorological"]]) %>%
            dplyr::filter(site_code %in% selected_parameters$sites) %>%
            dplyr::select_if(all_na_test) %>%
            dplyr::select(site_code, timestamp, everything())
          
          path <- "marinegeo_meteorological_data.csv"
          fs <- c(fs, path)
          write_csv(df, path)
          
          included_cols <- c(included_cols, colnames(df))
        }
        
        custom_dictionary <- data_dictionary %>%
          dplyr::filter(parameter_name %in% included_cols)
        
        path <- "data_dictionary.csv"
        fs <- c(fs, path)
        write_csv(custom_dictionary, path)
        
        # Zip the files together
        zip(zipfile=fname, files=fs)
        
      },
      contentType = "application/zip"
    )
    
    
  })
}