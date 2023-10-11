# -------- Graphs -------- #

# load required packages

library(tidyverse)
library(corrplot)
library(broom)
library(RColorBrewer)

# read in the data --------------------------------------------------------

long_data <- read.csv("01_Data/compiled_data_longform.csv")

long_data <- long_data %>%
  dplyr:: mutate(date = lubridate:: ymd(date))


# Whole Eastern Banks -----------------------------------------------------

all_data_whole_bank_summary <- long_data %>%
  dplyr:: group_by(year_month, category_name) %>%
  dplyr:: summarise(Mean_Cover = mean(value),
                   SE_Cover = sd(value) / sqrt(n())) %>%
  dplyr:: mutate(Lower_CI = Mean_Cover - qnorm(0.975) * SE_Cover,
         Upper_CI = Mean_Cover + qnorm(0.975) * SE_Cover)

all_data_whole_bank_summary <- all_data_whole_bank_summary %>%
 dplyr:: mutate(year_month = lubridate:: ym(year_month))

new_labels <- c("Not seagrass", "Cymodocea sp.", "Halophila ovalis", 
                "Syringodium isoetifolium", "Unidentified seagrass", 
                "Halophila spinulosa", "Zostera muelleri/Halodule uninervis", "Total Seagrass")

names(new_labels) <- c("not_seagrass", "sg_cymodocea_sp", 
                       "sg_halo_oval", "sg_syris", "sg_unknown",
                       "sg_hal_spin", "sg_zmhu", "total_sg")


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

plot_whole_bank_2 <- ggplot(all_data_whole_bank_summary %>% filter(category_name != "total_sg"), 
                             aes(x = year_month, y = Mean_Cover, group = category_name, fill = category_name)) +
  geom_point(aes(colour = category_name), size = 1) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI, colour = category_name)) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Category",
       fill = "Category") +
  scale_color_brewer(palette = "Dark2", labels = new_labels) +
  scale_fill_brewer(palette = "Dark2", labels = new_labels) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%Y") 

plot_whole_bank_2

ggsave(plot_whole_bank_2, 
       file = "03_Figures/final_figures/whole_bank_scale_2.tiff", 
       dpi = 300, 
       width = 10, 
       height = 6)


all_data_whole_bank_summary %>% filter(category_name == "sg_zmhu")
all_data_whole_bank_summary %>% filter(category_name == "sg_cymodocea_sp")
all_data_whole_bank_summary %>% filter(category_name == "sg_halo_oval")
all_data_whole_bank_summary %>% filter(category_name == "sg_syris")
all_data_whole_bank_summary %>% filter(category_name == "sg_hal_spin")
all_data_whole_bank_summary %>% filter(category_name == "sg_unknown")
all_data_whole_bank_summary %>% filter(category_name == "not_seagrass")
all_data_whole_bank_summary %>% filter(category_name == "total_sg")

# Graphs by Bank ----------------------------------------------------------

zone_summary <- long_data %>%
  group_by(zone, year_month, category_name) %>%
  dplyr::summarise(Mean_Cover = mean(value),
                   SE_Cover = sd(value) / sqrt(n())) %>%
  mutate(Lower_CI = Mean_Cover - qnorm(0.975) * SE_Cover,
         Upper_CI = Mean_Cover + qnorm(0.975) * SE_Cover)

zone_summary <- zone_summary %>%
  dplyr:: mutate(year_month = lubridate:: ym(year_month))

zone.labs <- c("Amity", "Chain", "Maroom", "Moreton", "Wanga-Wallen")
names(zone.labs) <- c("AM", "CH", "MA", "MO", "WA")


seagrass_zone_plot <- ggplot(zone_summary %>% 
                               filter(zone != "EP") %>% 
                               filter(zone != "CH") %>% 
                               filter(category_name != "not_seagrass") %>%
                               filter(category_name != "total_sg"), 
                             aes(x = year_month, y = Mean_Cover, group = category_name, color = category_name)) +
  geom_point(aes(colour = category_name), size = 2) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  facet_wrap(~zone, labeller = labeller(zone = zone.labs)) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Species",
       fill = "Species") +
  scale_color_brewer(palette = "Dark2", labels = new_labels) +
  scale_fill_brewer(palette = "Dark2", labels = new_labels) +
  scale_x_date(date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_bw()

seagrass_zone_plot

ggsave(seagrass_zone_plot, 
       file = "03_Figures/final_figures/zone_scale_means.tiff", 
       dpi = 300, 
       width = 14, 
       height = 10)

 seagrass_zone_plot_2 <- ggplot(zone_summary %>% 
                               filter(zone != "EP") %>% filter(zone != "CH") %>% filter(category_name == "total_sg"), 
                             aes(x = year_month, y = Mean_Cover)) +
  geom_point(aes(), size = 1) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  facet_wrap(~zone, labeller = labeller(zone = zone.labs)) +
  labs(x = "Year",
       y = "Mean Coverage (%)") +
  scale_color_brewer(palette = "Dark2", labels = new_labels) +
  scale_fill_brewer(palette = "Dark2", labels = new_labels) +
  scale_x_date(date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

seagrass_zone_plot_2

ggsave(seagrass_zone_plot_2, 
       file = "03_Figures/final_figures/zone_scale_means_total_seagrass.tiff", 
       dpi = 150, 
       width = 14, 
       height = 8)



zone_summary %>% filter(zone == "MA") %>% filter(category_name == "sg_hal_spin")




# Graphs by transect ------------------------------------------------------


# Graphs by transect

# filter for transects with more than 4 monitoring events
transect_data <- long_data %>%
  filter(transect %in% c("MA3", "AM4", "AM1", "AM6",
                         "WA3", "MO2", "MO3", "AM7", "AM8", 
                         "AM10", "AM5", "MA1", "MA2", "MA4", 
                         "MA5", "WA1", "WA4", "WA5", "WA6", 
                         "MO14", "MO33", "MO5", "MO4", "MO6",
                         "MO9", "MA20", "AM24", "AM3", "WA7", 
                         "MO21", "MO8", "MO7", "AM23"))


data_by_site <- transect_data %>%
  group_by(transect, year_month, category_name) %>%
  dplyr::summarise(Mean_Cover = mean(value),
                   SE_Cover = sd(value) / sqrt(n())) %>%
  mutate(Lower_CI = Mean_Cover - qnorm(0.975) * SE_Cover,
         Upper_CI = Mean_Cover + qnorm(0.975) * SE_Cover)

data_by_site <- data_by_site %>%
  dplyr:: mutate(year_month = lubridate:: ym(year_month))


transect_plots <- ggplot(data_by_site %>% filter(category_name != "total_sg") %>% filter(category_name != "not_seagrass"), 
       aes(x = year_month, y = Mean_Cover, group = category_name, color = category_name)) +
  #geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, fill = category_name), alpha = 0.2, colour = NA) +
  #geom_line(aes(colour = category_name)) +
  geom_point(aes(colour = category_name), size = 1) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  facet_wrap(~ transect) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Species",
       fill = "Species") +
  scale_color_brewer(palette = "Dark2", labels = new_labels) +
  scale_fill_brewer(palette = "Dark2", labels = new_labels) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_bw()

transect_plots

ggsave(transect_plots, 
       file = "03_Figures/final_figures/transect_plots.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)
 




