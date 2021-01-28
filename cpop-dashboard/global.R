# An R-Shiny app to visualize and download MarineGEO CPOP data. 
# Currently the application displays the most recent water quality data from MarineGEO's LoggerNet server. 
# Ultimately, this application will allow users to display, query, and download chemical and physical sensor data including water quality and meterological data across a range of quality control levels, from raw and unprocessed to highly curated.  
# Global script runs first when initializing application

library(shiny)
library(lubridate)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

# Lookup table for sensor attributes and properly formatted names for shiny app
rosetta <- read_csv("./data/cpop_schema.csv") 
index <- read_csv("./data/cpop_index.csv")

## PAN-BDT Data ####
# Filter rosetta table to correct variables
pan_bdt_match <- rosetta %>%
  filter(site_code == "PAN-BDT",
         file_source == "loggernet",
         stop_date == "Present")

# Read in past data
pan_bdt_df <- read_csv("./data/bocas_exosonde_bundle.csv")


## USA-MDA Data ####
usa_mda_match <- rosetta %>%
  filter(site_code == "USA-MDA",
         file_source == "loggernet",
         stop_date == "Present")

# Read in past data
usa_mda_df <- read_csv("./data/MGEO_SERC_ExoTable_bundle.csv")


## USA-SMS Data ####
usa_irl_match <- rosetta %>%
  filter(site_code == "USA-IRL",
         file_source == "loggernet",
         stop_date == "Present")

# Read in past data
usa_irl_df <- read_csv("./data/MGEO_SMS_ExoTable_bundle.csv")

## Select Input variable names
plotting_variables <- bind_rows(pan_bdt_match, usa_mda_match, usa_irl_match) %>%
  filter(!(display_name %in% c("Not published", "Date", "Time", "Record", "Timestamp"))) 

formatted_plot_variables <- plotting_variables %>%
  filter(site_code %in% index$site_code) %>%
  select(mgeo_cpop_variable_R, display_name) %>%
  distinct() %>%
  pull(display_name, name = mgeo_cpop_variable_R)
