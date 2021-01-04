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

# Read in near-real time .Dat table
column_headers <- read.table("./data/bocas_exosonde.dat", nrows = 1, skip = 1, sep=",",
                             colClasses = "character")

df <- read.table("./data/bocas_exosonde.dat", skip = 4, sep=",", na.strings = c("NA", "NAN"))

colnames(df) <- as.character(column_headers[1,])

pan_bdt_df <- df %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP)) %>%
  bind_rows(pan_bdt_df) %>%
  mutate(site_code = "PAN-BDT")

# Reassign column names
name_match = match(names(pan_bdt_df), pan_bdt_match$original_file_variable)

names(pan_bdt_df)[na.omit(name_match)] = pan_bdt_match$mgeo_cpop_variable_R[!is.na(name_match)]

## USA-MDA Data ####
usa_mda_match <- rosetta %>%
  filter(site_code == "USA-MDA",
         file_source == "loggernet",
         stop_date == "present")

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
  mutate(site_code = "USA-MDA")

name_match = match(names(usa_mda_df), usa_mda_match$original_file_variable)

names(usa_mda_df)[na.omit(name_match)] = usa_mda_match$mgeo_cpop_variable_R[!is.na(name_match)]

## Bind data ####
water_quality_df <- bind_rows(usa_mda_df, pan_bdt_df)

## Select Input variable names
plotting_variables <- bind_rows(pan_bdt_match, usa_mda_match) %>%
  filter(!(display_name %in% c("Not published", "Date", "Time", "Record", "Timestamp")))

