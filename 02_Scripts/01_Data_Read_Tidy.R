# ------------------ Read in and Tidy Data ------------------  #

# Load required packages

library(tidyverse)
library(readxl)
library(plyr)
library(lubridate)


# Read in data ------------------------------------------------------------

# Load in the data

file_names <- list.files("01_Data/02_percentage_covers", full.names = TRUE) #create a list of all the file names

data_list <- list() # create a blank list

for (file_name in file_names) { # for each file name in the list
  data <- read.csv(file_name) # read is a csv with that file name
  data_list[[file_name]] <- data # add it to the list
}


# Tidy the CPCE Data ---------------------------------------------------------------

# The data that was classified in CPCE and the ReefCloud data are in different formats
# CPCE data was from 2007 - 2015, everything else was classified on reefcloud. 
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

cpce_data$year <- lubridate:: year(cpce_data$date) # create a column with the year

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


# Reefcloud data ----------------------------------------------------------

reefcloud <- data_list[[7]] # create a data frame that is just the reef cloud data

# tidy the 2023 dataset

reefcloud <- reefcloud %>%
  tidyr:: separate(image_name, into = c("date", "transect", "image_number"), sep = "_", remove = FALSE) # create new columns with the date, transect and image number from the name column

reefcloud <- reefcloud %>%
  dplyr:: mutate(date = lubridate:: ymd(date)) # makes the dates dates

reefcloud$year <- lubridate:: year(reefcloud$date) # create a new column with the year of survey

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


# Update names of categories ---------------------------------------------------------------

# the CPCE and reefcloud data use different naming comventions for the categories. 
# This section of code updates those names so they are consistent across the dataset
# it does this using a created excel file called 'dictionary' which has a column with the original name, and a column with the updated name

dictionary <- read.csv("01_Data/Dictionary.csv") # read in the dictionary, which lists the CPCE/reefcloud categories and updated names

for (col_name in colnames(reefcloud)) { #for each column in the reefcloud data
  if (col_name %in% dictionary$original) { # if the column name is in the 'original' column of the dictionary
    new_col_name <- dictionary$category_name[dictionary$original == col_name] #find the new name
    colnames(reefcloud)[colnames(reefcloud) == col_name] <- new_col_name # update the column name
  }
}


for (col_name in colnames(cpce_data)) { #for each column in the CPCE data
  if (col_name %in% dictionary$original) { # if the column name is in the 'original' column of the dictionary
    new_col_name <- dictionary$category_name[dictionary$original == col_name] #find the new name
    colnames(cpce_data)[colnames(cpce_data) == col_name] <- new_col_name # update the column name
  }
}

colnames(reefcloud) <- make.unique(colnames(reefcloud)) #ensures there are no duplicates
colnames(cpce_data) <- make.unique(colnames(cpce_data)) #ensures there are no duplicates

cpce_data$image_name <- cpce_data$Name # create a new column in the CPCE data called image_name to match the reefcloud dataset

wide_data <- full_join(reefcloud, cpce_data) # join the reefcloude and CPCE data together. 

# This next section of code aggregates all the different categories into overall seagrass and non-seagrass categories. 

wide_data <- wide_data %>%
  mutate(not_seagrass = rowSums(select(., starts_with("Other")), na.rm = TRUE)) %>% # Calculate the sum of columns whose names start with "Other" and store it in a new column 'not_seagrass'
  select(-starts_with("other")) %>% # Remove the columns that start with "Other"
  mutate(sg_cymodocea_sp = rowSums(select(., starts_with("Cymodocea")), na.rm = TRUE)) %>% # Calculate the sum of columns whose names start with "Cymodocea" and store it in a new column 'sg_cymodocea_sp'
  select(-starts_with("Cymodocea")) %>% # Remove the columns that start with "Cymodocea"
  mutate(sg_zmhu = rowSums(select(., starts_with("Zostera muelleri")), na.rm = TRUE)) %>% # Calculate the sum of columns whose names start with "Zostera muelleri" and store it in a new column 'sg_zmhu'
  select(-starts_with("Zostera")) %>% # Remove the columns that start with "Zostera muelleri"
  mutate(sg_halo_oval = rowSums(select(., starts_with("Halophila ovalis")), na.rm = TRUE)) %>% # Calculate the sum of columns whose names start with "Halophila ovalis" and store it in a new column 'sg_halo_oval'
  select(-starts_with("Halophila ovalis")) %>% # Remove the columns that start with "Halophila ovalis"
  mutate(sg_syris = rowSums(select(., starts_with("Syringodium isoetifolium")), na.rm = TRUE)) %>% # Calculate the sum of columns whose names start with "Syringodium isoetifolium" and store it in a new column 'sg_syris'
  select(-starts_with("Syringodium isoetifolium")) %>% # Remove the columns that start with "Syringodium isoetifolium"
  mutate(sg_hal_spin = rowSums(select(., starts_with("Halophila spinulosa")), na.rm = TRUE)) %>% # Calculate the sum of columns whose names start with "Halophila spinulosa" and store it in a new column 'sg_hal_spin'
  select(-starts_with("Halophila spinulosa")) %>% # Remove the columns that start with "Halophila spinulosa"
  mutate(sg_unknown = rowSums(select(., starts_with("Unknown")), na.rm = TRUE)) %>% # Calculate the sum of columns whose names start with "Unknown" and store it in a new column 'sg_unknown'
  select(-starts_with("Unknown")) %>% # Remove the columns that start with "Unknown"
  mutate(col_remove = rowSums(select(., starts_with("remove")), na.rm = TRUE)) %>% # Calculate the sum of columns whose names start with "remove" and store it in a new column 'col_remove'
  select(-starts_with("remove")) # Remove the columns that start with "remove"


# remove any extra, unnecessary columns

wide_data <- wide_data %>%
  select(-c(unique_id, survey_title, project, site, depth_m, total, org_name, Name,Annotation.status, Annotation.area, col_remove))

# remove any where the sum of the whole row is zero

wide_data <- wide_data[rowSums(wide_data[,8:14])>0,]

# Fix transect names

wide_data$transect <- gsub("0(?=\\d)", "", wide_data$transect, perl = TRUE) # remove any leading 0s
wide_data$transect <- gsub("T", "", wide_data$transect, perl = TRUE) # remove the T
wide_data$transect <- gsub("CH01", "CH1", wide_data$transect, perl = TRUE) # Fix CH1 transect name
wide_data$transect <- gsub("CHO1", "CH1", wide_data$transect, perl = TRUE) # Fix CH1 transect name
wide_data$transect <- gsub("CH1E", "CH1", wide_data$transect, perl = TRUE) # Fix CH1 transect name

# Remove any transects with less than four monitoirng events

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
  dplyr:: mutate(date = lubridate:: ymd(date)) #make sure the date is in the proper format

wide_data$year_month <- format(as.Date(wide_data$date, "%Y-%m-%d"), "%Y-%m") # create a new year-month column
wide_data$year_month <- as.factor(wide_data$year_month) 
wide_data$year_month <- recode_factor(wide_data$year_month, "2022-05" = "2022-06", "2012-06" = "2012-07", 
                                      "2013-02" = "2013-01") #update some of the year months where the first/last sampling day was on the first/last day of a month (e.g. sampling event was 31/5, 1/6, 2/6, 3/6) 

wide_data <- wide_data %>%
  filter(year_month != "2013-05") # remove the 2013 sampling event, as only one transect was surveyed. 

# calculate the total seagrass coverage inclusing of all speies. 

wide_data <- wide_data %>%
  dplyr:: mutate(total_sg = rowSums(across(c(sg_cymodocea_sp, sg_zmhu, sg_halo_oval, sg_syris, sg_hal_spin, sg_unknown))))

# write to a csv and save

write_csv(wide_data, "01_Data/compiled_data_wideform.csv")

# make the data long form and save. 

long_data <- wide_data %>%
  pivot_longer(cols = c("not_seagrass", "sg_cymodocea_sp", "sg_zmhu", "sg_halo_oval", "sg_syris", "sg_unknown", "sg_hal_spin", "total_sg"), 
               names_to = "category_name", 
               values_to = "value")


write_csv(long_data, "01_Data/compiled_data_longform.csv")









