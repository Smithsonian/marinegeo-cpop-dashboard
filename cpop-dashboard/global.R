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
# Read in line with column headers
pan_bdt_column_headers <- read.table("./data/bocas_exosonde.dat", nrows = 1, skip = 1, sep=",",
                             colClasses = "character")

# Read in data, assign column names, and conduct initial date formatting
pan_bdt_df <- read.table("./data/bocas_exosonde.dat", skip = 4, sep=",")
colnames(pan_bdt_df) <- as.character(pan_bdt_column_headers[1,])
pan_bdt_df <- pan_bdt_df %>%
  rename(timestamp = TIMESTAMP) %>%
  mutate(timestamp = ymd_hms(timestamp))

# Create a lookup table that will be used to translate between loggernet attribute names and "pretty" forms for plotting
# Results will be provided as a named list to select input modules
pan_bdt_names <- rosetta %>%
  filter(site_code == "PAN-BDT",
         table_source == "Loggernet",
         sensor_names %in% as.character(pan_bdt_column_headers[1,])) %>%
  select(sensor_names, display_names)

