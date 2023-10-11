# -------- Explore the dataset -------- #

library(tidyverse)
library(corrplot)
library(broom)

# read in the data --------------------------------------------------------

all_data <- read.csv("01_Data/compiled_data_2007_2023_all_categories.csv")
seagrass_data <- read.csv("01_Data/compiled_data_2007_2023_seagrass_only.csv")
wide_data <- read.csv("01_Data/compiled_data_wideform.csv")


# create histograms of the data 

str(seagrass_data)
hist(seagrass_data$value)
hist(all_data$value)

zmhu <- seagrass_data %>% 
  filter(updated_name == "Zostera muelleri/Halodule uninervis")

hist(zmhu$value)

cym <- seagrass_data %>% 
  filter(updated_name == "Cymodocea sp")

hist(cym$value)

halo <- seagrass_data %>% 
  filter(updated_name == "Halophila ovalis")

hist(halo$value)

halsp <- seagrass_data %>% 
  filter(updated_name == "Halophila spinulosa")

hist(halsp$value)

syris <- seagrass_data %>% 
  filter(updated_name == "Syringodium isoetifolium")

hist(syris$value)

