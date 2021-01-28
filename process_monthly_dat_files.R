source_dir <- "./input_data/"
output_dir <- "./cpop-dashboard/data/"

processDAT <- function(source_dir, output_dir){
  library(readr)
  library(dplyr)
  
  sites <- c("bocas_exosonde", "MGEO_SERC_ExoTable", "MGEO_SMS_ExoTable")
  #sites <- "MGEO_SMS_ExoTable"
  
  for(site in sites){
    ## PAN-BDT Data ####
    data_files <- list.files(source_dir)[grepl(site, list.files(source_dir))]
    
    df <- data.frame()
    
    for(file in data_files){

      filepath <- paste0(source_dir, file)
      print(filepath)
      column_headers <- read.table(filepath, nrows = 1, skip = 1, sep=",",
                                   colClasses = "character")

      child_df <- read.table(filepath, skip = 4, sep=",", na.strings = c("NA", "NAN"))

      colnames(child_df) <- as.character(column_headers[1,])

      df <- child_df %>%
        bind_rows(df)
    } 
    
    write_csv(df, paste0(output_dir, site, "_bundle.csv"))
    }
  
}

processDAT(source_dir, output_dir)
