# Read in data from loggernet
# Temporary method until hive tables are created

## Water Quality ####
## ... PAN-BDT Data ####
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

## ... USA-MDA Data ####
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

## ... USA-IRL Data ####
# Read in near-real time .Dat table
column_headers <- read.table("./data/MGEO_SMS_ExoTable.dat", nrows = 1, skip = 1, sep=",",
                             colClasses = "character")

df <- read.table("./data/MGEO_SMS_ExoTable.dat", skip = 4, sep=",", na.strings = c("NA", "NAN"))

colnames(df) <- as.character(column_headers[1,])

usa_irl_df <- df %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP)) %>%
  bind_rows(usa_irl_df) %>%
  mutate(site_code = "USA-IRL")

name_match = match(names(usa_irl_df), usa_irl_match$original_file_variable)

names(usa_irl_df)[na.omit(name_match)] = usa_irl_match$mgeo_cpop_variable_R[!is.na(name_match)]

## MET data ####

## ... PAN-BDT ####
column_headers <- read.table("./data/MET_STRI_Table1.dat", nrows = 1, skip = 1, sep=",",
                             colClasses = "character")

df <- read.table("./data/MET_STRI_Table1.dat", skip = 4, sep=",", na.strings = c("NA", "NAN"))

colnames(df) <- as.character(column_headers[1,])

pan_bdt_df_met <- df %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP)) %>%
  bind_rows(pan_bdt_df_met) %>%
  mutate(site_code = "PAN-BDT")

# Only works as long as site code is last column
name_match = match(names(pan_bdt_df_met), pan_bdt_match_met$original_file_variable)
names(pan_bdt_df_met) = pan_bdt_match_met$mgeo_cpop_variable_R[name_match]
names(pan_bdt_df_met) = c(names(pan_bdt_df_met)[!is.na(names(pan_bdt_df_met))], "site_code")


## ... USA-MDA ####
column_headers <- read.table("./data/MGEO_SERC_MetTable.dat", nrows = 1, skip = 1, sep=",",
                             colClasses = "character")

df <- read.table("./data/MGEO_SERC_MetTable.dat", skip = 4, sep=",", na.strings = c("NA", "NAN"))

colnames(df) <- as.character(column_headers[1,])

usa_mda_df_met <- df %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP)) %>%
  bind_rows(usa_mda_df_met) %>%
  mutate(site_code = "USA-MDA")

test <- usa_mda_df_met

name_match = match(names(usa_mda_df_met), usa_mda_irl_match_met$original_file_variable)
names(usa_mda_df_met) = usa_mda_irl_match_met$mgeo_cpop_variable_R[name_match]
names(usa_mda_df_met) = c(names(usa_mda_df_met)[!is.na(names(usa_mda_df_met))], "site_code")

## WL data ####

## ... USA-MDA ####
column_headers <- read.table("./data/MGEO_SERC_LevelTable.dat", nrows = 1, skip = 1, sep=",",
                             colClasses = "character")

df <- read.table("./data/MGEO_SERC_LevelTable.dat", skip = 4, sep=",", na.strings = c("NA", "NAN"))

colnames(df) <- as.character(column_headers[1,])

usa_mda_df_wl <- df %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP)) %>%
  bind_rows(usa_mda_df_wl) %>%
  mutate(site_code = "USA-MDA")

name_match = match(names(usa_mda_df_wl), rosetta_wl_usa_mda$original_file_variable)
names(usa_mda_df_wl) = rosetta_wl_usa_mda$mgeo_cpop_variable_R[name_match]
names(usa_mda_df_wl) = c(names(usa_mda_df_wl)[!is.na(names(usa_mda_df_wl))], "site_code")

## ... PAN-BDT ####
column_headers <- read.table("./data/Bocas_MGeo_Tide.dat", nrows = 1, skip = 1, sep=",",
                             colClasses = "character")

df <- read.table("./data/Bocas_MGeo_Tide.dat", skip = 4, sep=",", na.strings = c("NA", "NAN"))

colnames(df) <- as.character(column_headers[1,])

pan_bdt_df_wl <- df %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP)) %>%
  bind_rows(pan_bdt_df_wl) %>%
  mutate(site_code = "PAN-BDT") %>%
  distinct()

name_match = match(names(pan_bdt_df_wl), rosetta_wl_panbdt$original_file_variable)
names(pan_bdt_df_wl) = rosetta_wl_panbdt$mgeo_cpop_variable_R[name_match]
names(pan_bdt_df_wl) = c(names(pan_bdt_df_wl)[!is.na(names(pan_bdt_df_wl))], "site_code")

## Bind data ####
water_quality_df <- bind_rows(usa_mda_df, pan_bdt_df, usa_irl_df) %>%
  select(-Record)

met_df <- bind_rows(pan_bdt_df_met, usa_mda_df_met) %>%
  select(-record)

water_level_df <- bind_rows(usa_mda_df_wl, pan_bdt_df_wl) %>%
  select(-record)

df_list <- list(
  "Water Quality" = water_quality_df,
  "Meteorological" = met_df,
  "Water Level" = water_level_df
)