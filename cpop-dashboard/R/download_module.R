download_UI <- function(id){
  ns <- NS(id)
  
  div(id = id,
      downloadButton(ns("download"), "Download selected data")
  )
}


download_server <- function(id, df_list, selected_parameters){
  
  moduleServer(id, function(input, output, session) {
    
    filter_date <- function(df){
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
        # path <- "_README.txt"
        # file.copy(paste0(original_wd, "/data/", "download_readme.txt"), path)
        fs <- c()
        
        if("Water Quality" %in% selected_parameters$data_type){
          df <- filter_date(df_list[["Water Quality"]]) %>%
            filter(site_code %in% selected_parameters$sites) 
          
          path <- "marinegeo_water_quality_data.csv"
          fs <- c(fs, path)
          write_csv(df, path)
        }
        
        if("Water Level" %in% selected_parameters$data_type){
          df <- filter_date(df_list[["Water Level"]]) %>%
            filter(site_code %in% selected_parameters$sites) 
          
          path <- "marinegeo_water_level_data.csv"
          fs <- c(fs, path)
          write_csv(df, path)
          
        }
        
        if("Meteorological" %in% selected_parameters$data_type){
          df <- filter_date(df_list[["Meteorological"]]) %>%
            filter(site_code %in% selected_parameters$sites) 
          
          path <- "marinegeo_meteorological_data.csv"
          fs <- c(fs, path)
          write_csv(df, path)
          
        }
        
        # citation_download <- study_citations %>%
        #   dplyr::filter(study_id %in% subset_cores()$study_id) %>%
        #   dplyr::arrange(study_id) %>%
        #   dplyr::select_if(all_na_test)
        # 
        # core_download <- cores %>%
        #   dplyr::filter(core_id %in% subset_cores()$core_id) %>%
        #   dplyr::arrange(study_id) %>%
        #   dplyr::select_if(all_na_test)
        # 
        # path <- paste0("CCRCN_cores.csv")
        # fs <- c(fs, path)
        # write_csv(core_download, path)
        # 
        # site_download <- sites %>%
        #   dplyr::filter(site_id %in% subset_cores()$site_id) %>%
        #   dplyr::arrange(study_id) %>%
        #   dplyr::select_if(all_na_test)
        # 
        # path <- paste0("CCRCN_sites.csv")
        # fs <- c(fs, path)
        # write_csv(site_download, path)
        # 
        # depthseries_download <- depthseries %>%
        #   dplyr::filter(core_id %in% subset_cores()$core_id) %>%
        #   dplyr::select_if(all_na_test) %>%
        #   dplyr::arrange(study_id, site_id, core_id, depth_min, depth_max)
        # 
        # path <- paste0("CCRCN_depthseries.csv")
        # fs <- c(fs, path)
        # write_csv(depthseries_download, path)
        # 
        # impact_download <- impacts %>%
        #   dplyr::filter(core_id %in% subset_cores()$core_id) %>%
        #   dplyr::arrange(study_id)
        # 
        # path <- paste0("CCRCN_impacts.csv")
        # fs <- c(fs, path)
        # write_csv(impact_download, path)
        # 
        # methods_download <- methods %>%
        #   dplyr::filter(study_id %in% subset_cores()$study_id) %>%
        #   dplyr::arrange(study_id) %>%
        #   dplyr::select_if(all_na_test)
        # 
        # path <- paste0("CCRCN_methods.csv")
        # fs <- c(fs, path)
        # write_csv(methods_download, path)
        # 
        # species_download <- species %>%
        #   dplyr::filter(study_id %in% subset_cores()$study_id) %>%
        #   dplyr::arrange(study_id)
        # 
        # path <- paste0("CCRCN_species.csv")
        # fs <- c(fs, path)
        # write_csv(species_download, path)
        
        # # Download standardized depthseries if a custom range has been applied
        # if(nrow(target_horizons$df) > 0){
        #   path <- paste0("CCRCN_standardized_depthseries.csv")
        #   fs <- c(fs, path)
        #   write_csv(customIntervals(), path)
        # }
        # 
        # bib_file <- citation_download %>%
        #   dplyr::select(-study_id, -bibliography_id, -publication_type) %>%
        #   dplyr::mutate_if(is.factor, as.character) %>%
        #   dplyr::distinct()
        # 
        # keys <- bib_file$key
        # bib_file <- as.data.frame(bib_file)
        # row.names(bib_file) <- keys
        # bib_file <- select(bib_file, -key)
        # 
        # path <- paste0("CCRCN_bibliography.bib")
        # 
        # # Save bibliography as bibentry file and write .bib file
        # bibliography <- as.BibEntry(bib_file)
        # 
        # WriteBib(as.BibEntry(bib_file), path)
        # fs <- c(fs, path)
        # 
        # # Removing CSV format and created text file of formatted citations
        # path <- paste0("./CCRCN_bibliography.txt")
        # # Cite all citations in bib file to prepare formatted bibliography
        # NoCite(bibliography)
        # 
        # # Write bibliography to text file
        # sink("./CCRCN_bibliography.txt")
        # PrintBibliography(bibliography, .opts=list(bib.style="authoryear"))
        # sink()
        # 
        # fs <- c(fs, path)
        # 
        # path <- paste0("CCRCN_bibliography.csv")
        # fs <- c(fs, path)
        # write_csv(citation_download, path)
        # 
        # column_names <- unique(c(colnames(methods_download), colnames(site_download),
        #                          colnames(core_download), colnames(depthseries_download),
        #                          colnames(species_download), colnames(impact_download)))
        # 
        # download_database_structure <- database_structure %>%
        #   dplyr::filter(attribute_name %in% column_names)
        # 
        # path <- paste0("CCRCN_data_dictionary.csv")
        # fs <- c(fs, path)
        # write_csv(download_database_structure, path)
        
        # Zip the files together
        zip(zipfile=fname, files=fs)
        
      },
      contentType = "application/zip"
    )
    
    
  })
}