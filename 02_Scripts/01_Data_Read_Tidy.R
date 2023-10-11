# ------------------ Read in and Tidy Data ------------------  #

# Load required packages

library(tidyverse)
library(readxl)
library(plyr)
library(lubridate)


# Read in data ------------------------------------------------------------

# Load in the data

file_names <- list.files("01_data/percentage_covers", full.names = TRUE) #create a list of all the file names

data_list <- list() # create a blank list

for (file_name in file_names) { # for each file name in the list
  data <- read.csv(file_name) # read is a csv with that file name
  data_list[[file_name]] <- data # add it to the list
}

# Add a column to each data frame with the survey year

data_list[[1]]$year <- 2007
data_list[[2]]$year <- 2011
data_list[[3]]$year <- 2012
data_list[[4]]$year <- 2013
data_list[[5]]$year <- 2014
data_list[[6]]$year <- 2015
data_list[[7]]$year <- "2021/22/23"


# Tidy the CPCE Data ---------------------------------------------------------------

# The data that was classified in CPCE and the ReefCloud data are in different formats
# CPCE data was from 2007 - 2015, everythig else was classified on reef cloud. 
# First, deal with the CPCE data, then the ReefCloud data. 

# bind the CPCe data frames (2007-2015) together

cpce_data <- do.call(rbind, data_list[1:6]) # bind data from 2007-2015 together
cpce_data <- data.frame(cpce_data, row.names = NULL) # make it a data frame

# Divide the Name column into date, site and image number

cpce_data$Name <- sub("Ebanks_", "", cpce_data$Name) # the years 2013 and 2014 have "Ebanks_" in the name, so this removes that so we can then separate out

cpce_data <- cpce_data %>%
  tidyr:: separate(Name, into = c("date", "transect", "image_number"), sep = "_", remove = FALSE) # separate the photo name into separate columsn with the date, transect number and image number

cpce_data <- cpce_data %>%
  dplyr:: mutate(date = lubridate:: ymd(date)) #convert the date column into a date

cpce_data$year <- year(cpce_data$date)

# tidy the site names by removing any leading "EB" or "MB"

cpce_data$transect <- gsub("EB", "", cpce_data$transect) # removes the leading EB on some of the site names
cpce_data$transect <- gsub("MB", "", cpce_data$transect) # Removes the leading MB on some of the site names


# tidy the image numbers by removing the .jpg

cpce_data$image_number <- gsub(".JPG", "", cpce_data$image_number) # removes the .JPG from the end of the image number
cpce_data$image_number <- as.numeric(cpce_data$image_number) # make the image numbers numeric


# create a new column that is the 'site/zone' and the transect number

cpce_data$zone <- sub("[0-9]+", "", cpce_data$transect) # create a new column called zone by copying the transect, but removing the numbers
cpce_data$transect_number <- as.numeric(sub("[A-Z]+", "", cpce_data$transect)) # Create a new column called transect number by copying the transect column but removing the letters
cpce_data$zone <- sub("T", "", cpce_data$zone) # remove the T from the zone name
cpce_data$zone <- sub("MOA", "MO", cpce_data$zone) # replased MOA with MO

# make the data long form

#cpce_data_long <- cpce_data %>%
 # tidyr:: pivot_longer(cols = 7:63, names_to = "category") %>%
  #dplyr:: select(-c(Annotation.status, Annotation.area)) 


# Reefcloud data ----------------------------------------------------------

reefcloud <- data_list[[7]] # create a data frame that is just the reef cloud data

# tidy the 2023 dataset

reefcloud <- reefcloud %>%
  tidyr:: separate(image_name, into = c("date", "transect", "image_number"), sep = "_", remove = FALSE) # create new columns with the date, transect and image number from the name column

reefcloud <- reefcloud %>%
  dplyr:: mutate(date = lubridate:: ymd(date)) # makes the dates dates

reefcloud$year <- year(reefcloud$date)

# Create zone names

reefcloud$zone <- reefcloud$transect # create a new column called zone by copying the transect column
reefcloud$zone <- gsub("EB", "", reefcloud$zone) # remove the EB from some of the zone names
reefcloud$zone <- sub("[0-9]+", "", reefcloud$zone) #remove the numbers
reefcloud$zone <- sub("T", "", reefcloud$zone) # remove the T
reefcloud$zone <- sub(".JPG", "", reefcloud$zone) # remove any .JPG
reefcloud$zone <- sub("\\(.*?\\)", "", reefcloud$zone) # remove anything enclosed in parenthesis ()
reefcloud$zone <- sub("MOA", "MO", reefcloud$zone) # Change MOA to MO
reefcloud$zone <- sub("CHE", "CH", reefcloud$zone) # Change CHE to CH
reefcloud$zone <- sub("CHO", "CH", reefcloud$zone) # change CHO to CH
reefcloud$zone <- sub("AM ", "AM", reefcloud$zone) # remove the double space at the end of some of the AM zone names

# tidy the transect names

reefcloud$transect <- sub(".JPG", "", reefcloud$transect) # removes any .JPG
reefcloud$transect <- sub("\\(.*?\\)", "", reefcloud$transect) # remove anything enclosed in parenthesis ()
reefcloud$transect <- sub(" ", "", reefcloud$transect) # Removes any excess space
reefcloud$transect <- sub("EB", "", reefcloud$transect) # Removes any EB in the transect name

reefcloud$transect_number <- as.numeric(sub("[A-Z]+", "", reefcloud$transect)) # makes the transect numbers numeric and removes and letters

reefcloud$image_number <- sub(".JPG", "", reefcloud$image_number) # removes .JPG from the image number
reefcloud$image_number <- sub(".jpg", "", reefcloud$image_number) # removes . jpg from the image number
reefcloud$image_number <- as.numeric(reefcloud$image_number) #makes image numbers numeric


# Join data - wide form ---------------------------------------------------------------

dictionary <- read.csv("01_Data/Dictionary.csv") # read in the dictionary

for (col_name in colnames(reefcloud)) {
  if (col_name %in% dictionary$original) {
    new_col_name <- dictionary$category_name[dictionary$original == col_name]
    colnames(reefcloud)[colnames(reefcloud) == col_name] <- new_col_name
  }
}


for (col_name in colnames(cpce_data)) {
  if (col_name %in% dictionary$original) {
    new_col_name <- dictionary$category_name[dictionary$original == col_name]
    colnames(cpce_data)[colnames(cpce_data) == col_name] <- new_col_name
  }
}

colnames(reefcloud) <- make.unique(colnames(reefcloud))
colnames(cpce_data) <- make.unique(colnames(cpce_data))

cpce_data$image_name <- cpce_data$Name

wide_data <- full_join(reefcloud, cpce_data)

wide_data <- wide_data %>%
  mutate(not_seagrass = rowSums(select(., starts_with("Other")), na.rm = TRUE)) %>%
  select(-starts_with("other")) %>%
  mutate(sg_cymodocea_sp = rowSums(select(., starts_with("Cymodocea")), na.rm = TRUE)) %>%
  select(-starts_with("Cymodocea")) %>%
  mutate(sg_zmhu = rowSums(select(., starts_with("Zostera muelleri")), na.rm = TRUE)) %>%
  select(-starts_with("Zostera")) %>%
  mutate(sg_halo_oval = rowSums(select(., starts_with("Halophila ovalis")), na.rm = TRUE)) %>%
  select(-starts_with("Halophila ovalis")) %>%
  mutate(sg_syris = rowSums(select(., starts_with("Syringodium isoetifolium")), na.rm = TRUE)) %>%
  select(-starts_with("Syringodium isoetifolium")) %>%
  mutate(sg_hal_spin = rowSums(select(., starts_with("Halophila spinulosa")), na.rm = TRUE)) %>%
  select(-starts_with("Halophila spinulosa")) %>%
  mutate(sg_unknown = rowSums(select(., starts_with("Unknown")), na.rm = TRUE)) %>%
  select(-starts_with("Unknown")) %>%
  mutate(col_remove = rowSums(select(., starts_with("remove")), na.rm = TRUE)) %>%
  select(-starts_with("remove"))

wide_data <- wide_data %>%
  select(-c(unique_id, survey_title, project, site, depth_m, total, org_name, Name,Annotation.status, Annotation.area, col_remove))

str(wide_data)

# remove any where the sum of the whole row is zero

wide_data <- wide_data[rowSums(wide_data[,8:14])>0,]


# Fix transect names

wide_data$transect <- gsub("0(?=\\d)", "", wide_data$transect, perl = TRUE) # remove any leading 0s
wide_data$transect <- gsub("T", "", wide_data$transect, perl = TRUE) # remove the T
wide_data$transect <- gsub("CH01", "CH1", wide_data$transect, perl = TRUE) # Fix CH1 transect name
wide_data$transect <- gsub("CHO1", "CH1", wide_data$transect, perl = TRUE) # Fix CH1 transect name
wide_data$transect <- gsub("CH1E", "CH1", wide_data$transect, perl = TRUE) # Fix CH1 transect name

wide_data <- wide_data %>%
  filter(transect %in% c("MA3", "AM4", "AM1", "AM6",
                         "WA3", "MO2", "MO3", "AM7", "AM8", 
                         "AM10", "AM5", "MA1", "MA2", "MA4", 
                         "MA5", "WA1", "WA4", "WA5", "WA6", 
                         "MO14", "MO33", "MO5", "MO4", "MO6",
                         "MO9", "MA20", "AM24", "AM3", "WA7", 
                         "MO21", "MO8", "MO7", "AM23"))

# fix the sampling events

wide_data <- wide_data %>%
  dplyr:: mutate(date = lubridate:: ymd(date)) 

wide_data$year_month <- format(as.Date(wide_data$date, "%Y-%m-%d"), "%Y-%m")

wide_data$year_month <- as.factor(wide_data$year_month)

wide_data$year_month <- recode_factor(wide_data$year_month, "2022-05" = "2022-06", "2012-06" = "2012-07", 
                                      "2013-02" = "2013-01")

wide_data <- wide_data %>%
  filter(year_month != "2013-05")

wide_data <- wide_data %>%
  dplyr:: mutate(total_sg = rowSums(across(c(sg_cymodocea_sp, sg_zmhu, sg_halo_oval, sg_syris, sg_hal_spin, sg_unknown))))

# write to a csv

write_csv(wide_data, "01_Data/compiled_data_wideform.csv")

# make the data long form

long_data <- wide_data %>%
  pivot_longer(cols = c("not_seagrass", "sg_cymodocea_sp", "sg_zmhu", "sg_halo_oval", "sg_syris", "sg_unknown", "sg_hal_spin", "total_sg"), 
               names_to = "category_name", 
               values_to = "value")


write_csv(long_data, "01_Data/compiled_data_longform.csv")










