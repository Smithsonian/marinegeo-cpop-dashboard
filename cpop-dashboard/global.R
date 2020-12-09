# An R-Shiny app to visualize and download MarineGEO CPOP data. 
# Currently the application displays the most recent water quality data from MarineGEO's LoggerNet server. 
# Ultimately, this application will allow users to display, query, and download chemical and physical sensor data including water quality and meterological data across a range of quality control levels, from raw and unprocessed to highly curated.  
# Global script runs first when initializing application

library(shiny)
library(lubridate)
library(readr)
library(dplyr)
library(plotly)

# Lookup table for sensor attributes and properly formatted names for shiny app
rosetta <- read_csv("./data/MarineGEO_rosetta.csv") %>%
  rename(site_code = Site,
         table_source = `File source`,
         sensor_names = `Kor EXO/Loggernet Variable`,
         display_names = `Published variable (for Shiny)`)


## PAN-BDT Data ####
# Read in past data
pan_bdt_df <- read_csv("./data/bocas_exosonde_bundle.csv")

# Read in near-real time .Dat table
column_headers <- read.table("./data/bocas_exosonde.dat", nrows = 1, skip = 1, sep=",",
                             colClasses = "character")

df <- read.table("./data/bocas_exosonde.dat", skip = 4, sep=",", na.strings = c("NA", "NAN"))

colnames(df) <- as.character(column_headers[1,])

pan_bdt_df <- df %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP)) %>%
  bind_rows(pan_bdt_df) %>%
  rename(timestamp = TIMESTAMP) 

# Create a lookup table that will be used to translate between loggernet attribute names and "pretty" forms for plotting
# Results will be provided as a named list to select input modules
pan_bdt_names <- rosetta %>%
  filter(site_code == "PAN-BDT",
         table_source == "Loggernet",
         sensor_names %in% colnames(pan_bdt_df)) %>%
  select(sensor_names, display_names)

## USA-MDA Data ####
# Read in past data
usa_mda_df <- read_csv("./data/MGEO_SERC_ExoTable_bundle.csv")

# Read in near-real time .Dat table
column_headers <- read.table("./data/MGEO_SERC_ExoTable.dat", nrows = 1, skip = 1, sep=",",
                             colClasses = "character")

df <- read.table("./data/MGEO_SERC_ExoTable.dat", skip = 4, sep=",", na.strings = c("NA", "NAN"))

colnames(df) <- as.character(column_headers[1,])

usa_mda_df <- df %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP)) %>%
  bind_rows(usa_mda_df) %>%
  rename(timestamp = TIMESTAMP,
         WaterTemp = Temp_C) 

# Create a lookup table that will be used to translate between loggernet attribute names and "pretty" forms for plotting
# Results will be provided as a named list to select input modules
# SERC attributes haven't been added to rosetta yet
# usa_mda_names <- rosetta %>%
#   filter(#site_code == "USA-MDA",
#          table_source == "Loggernet",
#          sensor_names %in% colnames(usa_mda_df)) %>%
#   select(sensor_names, display_names)
usa_mda_names <- tibble(sensor_names = colnames(usa_mda_df),
                        display_names = colnames(usa_mda_df)) %>%
  mutate(display_names = ifelse(sensor_names == "WaterTemp", "Temperature (℃)", display_names))
