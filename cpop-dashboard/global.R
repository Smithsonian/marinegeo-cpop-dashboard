# An R-Shiny app to visualize and download MarineGEO CPOP data. 
# Currently the application displays the most recent water quality data from MarineGEO's LoggerNet server. 
# Ultimately, this application will allow users to display, query, and download chemical and physical sensor data including water quality and meterological data across a range of quality control levels, from raw and unprocessed to highly curated.  
# Global script runs first when initializing application

library(shiny)
library(plyr)
library(lubridate)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

# Lookup table for sensor attributes and properly formatted names for shiny app
rosetta <- read_csv("./data/cpop_schema.csv") %>%
  mutate(data_type = "Water Quality")

rosetta_met <- read_csv("./data/met_rosetta.csv") %>%
  mutate(data_type = "Meteorological")

rosetta_wl <- read_csv("./data/wl_rosetta.csv", locale = locale(encoding = "Windows-1252")) %>%
  mutate(data_type = "Water Level") 

index <- read_csv("./data/cpop_index.csv")

## PAN-BDT Data ####
# Filter rosetta table to correct variables
pan_bdt_match <- rosetta %>%
  filter(site_code == "PAN-BDT",
         file_source == "loggernet",
         stop_date == "Present")

pan_bdt_match_met <- rosetta_met %>%
  filter(site_code == "PAN-BDT",
         file_source == "DropBox/MarineGEO Water Monitoring Panama Bocas/STRI_TMON_Rawdata_Loggernet")

# Read in past data
pan_bdt_df <- read_csv("./data/bocas_exosonde_bundle.csv")
pan_bdt_df_met <- read_csv("./data/MET_STRI_Table1_bundle.csv")

## USA-MDA Data ####
usa_mda_match <- rosetta %>%
  filter(site_code == "USA-MDA",
         file_source == "loggernet",
         stop_date == "Present")

# Read in past data
usa_mda_df <- read_csv("./data/MGEO_SERC_ExoTable_bundle.csv")
usa_mda_df_met <- read_csv("./data/MGEO_SERC_MetTable_bundle.csv")
usa_mda_df_wl <- read_csv("./data/MGEO_SERC_LevelTable_bundle.csv")

rosetta_wl_usa_mda <- rosetta_wl %>%
  filter(file_source == "SERC Dock/SERC_DOCK Rawdata_Loggernet",
         !is.na(original_file_variable))

usa_mda_irl_match_met <- rosetta_met %>%
  filter(original_file_variable %in% colnames(usa_mda_df_met),
         stop_date == "present",
         site_code == "USA-MDA")

## USA-SMS Data ####
usa_irl_match <- rosetta %>%
  filter(site_code == "USA-IRL",
         file_source == "loggernet",
         stop_date == "Present")

# Read in past data
usa_irl_df <- read_csv("./data/MGEO_SMS_ExoTable_bundle.csv")
usa_irl_df_met <- read_csv("./data/MGEO_SMS_MetTable_bundle.csv")

data_type_list <- unique(index$data_type)
initial_selected_data_type <- "Water Quality"
num_sites <- length(unique(index$site_code))

## Select Input variable names
# Drop: BGA_PE_microg_L, Chlorophyll_microg_L, fDOM_QSU, ODO_sat, pH_mV, nLF_Cond_microS_cm(2), ODO_local
# Many of these have another variable that is the same, but in a different unit. 
# Removing cleans up the list of available variables
plotting_variables <- bind_rows(pan_bdt_match, usa_mda_match, usa_irl_match, 
                                pan_bdt_match_met, usa_mda_irl_match_met,
                                rosetta_wl_usa_mda) %>%
  filter(!(display_name %in% c("Not published", "Date", "Time", "Record", "Timestamp")),
         !(mgeo_cpop_variable_R %in% c("BGA_PE_microg_L", "Chlorophyll_microg_L", "fDOM_QSU", 
                                       "ODO_sat", "pH_mV", "nLF_Cond_microS_cm", "ODO_local",
                                       "Wiper_Position_volt", "Cable_Pwr_V", "Battery_V"))) %>%
  arrange(display_name)

var_list_df <- plotting_variables %>%
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

formatted_plot_variables <- plotting_variables %>%
  filter(site_code %in% index$site_code) %>%
  select(mgeo_cpop_variable_R, display_name) %>%
  distinct() %>%
  pull(display_name, name = mgeo_cpop_variable_R)

initial_selected_variable <- "Temp_C"