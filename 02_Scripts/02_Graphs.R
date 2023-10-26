# -------- Graphs -------- #

# load required packages

library(tidyverse)
library(RColorBrewer)

# read in the data and set up graph labels and colours --------------------------------------------------------

long_data <- read.csv("01_Data/compiled_data_longform.csv")

long_data <- long_data %>%
  dplyr:: mutate(date = lubridate:: ymd(date)) # make sure the dates are dates

# create labels for facet titles in graphs

new_labels <- c("Not seagrass", "Cymodocea sp.", "Halophila ovalis", 
                "Syringodium isoetifolium", "Unidentified seagrass", 
                "Halophila spinulosa", "Zostera muelleri/Halodule uninervis", "Total Seagrass")

names(new_labels) <- c("not_seagrass", "sg_cymodocea_sp", 
                       "sg_halo_oval", "sg_syris", "sg_unknown",
                       "sg_hal_spin", "sg_zmhu", "total_sg")

zone.labs <- c("Amity", "Chain", "Maroom", "Moreton", "Wanga-Wallen")
names(zone.labs) <- c("AM", "CH", "MA", "MO", "WA")

# designate plot colours

unique(long_data$category_name)

speciescols <- c("not_seagrass" = "#E59B36", 
                "sg_cymodocea_sp" = "#578a53", 
                "sg_zmhu" = "#E6C532", 
                "sg_halo_oval" = "#29581e", 
                "sg_syris" = "#49ABE6", 
                "sg_unknown" = "#A0E548", 
                "sg_hal_spin" = "#665858")


# Whole Eastern Banks -----------------------------------------------------

# create summary data for the whole bank scale. 

all_data_whole_bank_summary <- long_data %>%
  dplyr:: group_by(year_month, category_name) %>%
  dplyr:: summarise(Mean_Cover = mean(value),
                   SE_Cover = sd(value) / sqrt(n())) %>%
  dplyr:: mutate(Lower_CI = Mean_Cover - qnorm(0.975) * SE_Cover,
         Upper_CI = Mean_Cover + qnorm(0.975) * SE_Cover)

all_data_whole_bank_summary <- all_data_whole_bank_summary %>%
 dplyr:: mutate(year_month = lubridate:: ym(year_month)) # make sure dates are dates


write_csv(all_data_whole_bank_summary, "04_Supplementary_information/supplementary_table_1_whole_bank_summary.csv")

# plot the whole-bank scale with each category as an individual plot and save plot

plot_whole_bank <- ggplot(all_data_whole_bank_summary %>% filter(category_name != "total_sg"), 
                          aes(x = year_month, y = Mean_Cover, group = category_name)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  facet_wrap(~ category_name, labeller = labeller(category_name = new_labels), ncol = 3) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Bank",
       fill = "Bank") +
  scale_x_date(date_labels = "%Y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none") 

plot_whole_bank

ggsave(plot_whole_bank, 
      file = "03_Figures/final_figures/whole_bank_scale.tiff", 
     dpi = 300, 
    width = 14, 
   height = 10)


# plot at the whole bank scale with everything on the one plot. 

plot_whole_bank_2 <- ggplot(all_data_whole_bank_summary %>% filter(category_name != "total_sg"), 
                             aes(x = year_month, y = Mean_Cover, group = category_name, fill = category_name)) +
  geom_point(aes(colour = category_name), size = 1) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI, colour = category_name)) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Category",
       fill = "Category") +
  scale_color_manual(values = speciescols, labels = new_labels) +
  scale_fill_manual(values = speciescols, labels = new_labels) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%Y") 

plot_whole_bank_2

ggsave(plot_whole_bank_2, 
       file = "03_Figures/final_figures/whole_bank_scale_2.tiff", 
       dpi = 300, 
       width = 10, 
       height = 6)


# Graphs by Bank ----------------------------------------------------------

# create summary data for each zone and save

zone_summary <- long_data %>%
  dplyr:: group_by(zone, year_month, category_name) %>%
  dplyr::summarise(Mean_Cover = mean(value),
                   SE_Cover = sd(value) / sqrt(n())) %>%
  dplyr:: mutate(Lower_CI = Mean_Cover - qnorm(0.975) * SE_Cover,
         Upper_CI = Mean_Cover + qnorm(0.975) * SE_Cover)

zone_summary <- zone_summary %>%
  dplyr:: mutate(year_month = lubridate:: ym(year_month))


write_csv(zone_summary, "04_Supplementary_information/supplementary_table_2_individual_bank_summary.csv")

# create a plot of each individual bank

seagrass_zone_plot <- ggplot(zone_summary %>% 
                               filter(zone != "EP") %>% # exclude "EP" bank
                               filter(zone != "CH") %>% # Exlude chain bank, not enough sampling events
                               filter(category_name != "not_seagrass") %>% # don't plot the not-seagrass category
                               filter(category_name != "total_sg"), 
                             aes(x = year_month, y = Mean_Cover, group = category_name, color = category_name)) +
  geom_point(aes(colour = category_name), size = 2) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  facet_wrap(~ zone, labeller = labeller(zone = zone.labs)) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Species",
       fill = "Species") +
  scale_color_manual(values = speciescols, labels = new_labels) +
  scale_fill_manual(values = speciescols, labels = new_labels) +
  scale_x_date(date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_bw()

seagrass_zone_plot

ggsave(seagrass_zone_plot, 
       file = "03_Figures/final_figures/zone_scale_means.tiff", 
       dpi = 300, 
       width = 14, 
       height = 10)


# Create a plot for the total seagrass coverage by bank and save

 seagrass_zone_plot_2 <- ggplot(zone_summary %>% 
                               filter(zone != "EP") %>% filter(zone != "CH") %>% filter(category_name == "total_sg"), 
                             aes(x = year_month, y = Mean_Cover)) +
  geom_point(aes(), size = 1) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  facet_wrap(~ zone, labeller = labeller(zone = zone.labs)) +
  labs(x = "Year",
       y = "Mean Coverage (%)") +
   scale_color_manual(values = speciescols, labels = new_labels) +
   scale_fill_manual(values = speciescols, labels = new_labels) +
  scale_x_date(date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

seagrass_zone_plot_2

ggsave(seagrass_zone_plot_2, 
       file = "03_Figures/final_figures/zone_scale_means_total_seagrass.tiff", 
       dpi = 150, 
       width = 14, 
       height = 8)


# Graphs by individual transect ------------------------------------------------------

# create a summary for each transect

data_by_site <- long_data %>%
  dplyr:: group_by(transect, year_month, category_name) %>%
  dplyr:: summarise(Mean_Cover = mean(value),
                   SE_Cover = sd(value) / sqrt(n())) %>%
  dplyr:: mutate(Lower_CI = Mean_Cover - qnorm(0.975) * SE_Cover,
         Upper_CI = Mean_Cover + qnorm(0.975) * SE_Cover)

data_by_site <- data_by_site %>%
  dplyr:: mutate(year_month = lubridate:: ym(year_month))


write_csv(data_by_site, "04_Supplementary_information/supplementary_table_3_transect_scale_summary.csv")

# create a plot of coverage for each individual transect and save

transect_plots <- ggplot(data_by_site %>% filter(category_name != "total_sg") %>% filter(category_name != "not_seagrass"), 
       aes(x = year_month, y = Mean_Cover, group = category_name, color = category_name)) +
  geom_point(aes(colour = category_name), size = 1) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  facet_wrap(~ transect) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Species",
       fill = "Species") +
  scale_color_manual(values = speciescols, labels = new_labels) +
  scale_fill_manual(values = speciescols, labels = new_labels) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_bw()

transect_plots

ggsave(transect_plots, 
       file = "03_Figures/final_figures/transect_plots.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)
 




