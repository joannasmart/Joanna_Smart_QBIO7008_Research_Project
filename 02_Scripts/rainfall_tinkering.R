library(tidyverse)
library(lubridate)

monthly_rainfall <- read.csv("01_Data/weather_data/Brisbane_monthly_rainfall.csv")


monthly_rainfall <- monthly_rainfall %>% 
  filter(Year %in% c("2007", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", 
                     "2021", "2022", "2023"))

monthly_rainfall$Year <- as.character(monthly_rainfall$Year)
monthly_rainfall$Month <- as.character(monthly_rainfall$Month)

monthly_rainfall <- monthly_rainfall %>%
  dplyr::mutate(year_month = paste(Year, Month, sep = "-"))

monthly_rainfall$year_month <- ym(monthly_rainfall$year_month)
monthly_rainfall$year_month <- format(as.Date(monthly_rainfall$year_month, "%Y-%m-%d"), "%Y-%m") # change the dates to remove the day so they can be grouped by month

annual_rainfall <- monthly_rainfall %>%
  group_by(Year) %>%
  dplyr:: summarise(annual_rainfall = sum(Monthly.Precipitation.Total..millimetres.))

annual_rainfall$year <- as.numeric(annual_rainfall$Year)

seagrass_data <- left_join(seagrass_data, annual_rainfall, by = "year")



rainfall_summary <- seagrass_data %>%
  group_by(year, updated_name, annual_rainfall) %>%
  dplyr::summarise(Mean_Cover = mean(value),
                   SE_Cover = sd(value) / sqrt(n())) %>%
  mutate(Lower_CI = Mean_Cover - qnorm(0.975) * SE_Cover,
         Upper_CI = Mean_Cover + qnorm(0.975) * SE_Cover)

rainfall_summary_plot <- ggplot(rainfall_summary %>% filter(updated_name != "Total seagrass"), 
                              aes(x = annual_rainfall, y = Mean_Cover, group = updated_name, color = updated_name)) +
  geom_line() +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, fill = updated_name), alpha = 0.2) +
  geom_point() +
  facet_wrap(~ updated_name) +
  labs(x = "Annual Rainfall (mm)",
       y = "Mean Coverage (%)",
       color = "Species",
       fill = "Species") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  #theme_dark() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none")

rainfall_summary_plot

ggsave(rainfall_summary_plot, 
       file = "03_Figures/Preliminary_figures/rainfall_experiment.tiff", 
       dpi = 150, 
       width = 14, 
       height = 8)


seagrass_data$value_converted[seagrass_data$value_converted == 1] <- 0.9999
seagrass_data$value_converted[seagrass_data$value_converted == 0] <- 0.0001

rainfall_glmm <- ggplot(seagrass_data %>% filter(updated_name != "Total seagrass"), 
       aes(x = annual_rainfall, y = value/100)) +
         geom_point(aes(colour = updated_name)) +
         geom_smooth(method='glm', 
                     method.args=list(family="beta_family"),
                     formula = y ~ x) +
  labs(y = "Proportion of coverage", 
       x = "Annual Rainfall (mm)", 
       colour = "Species")

rainfall_glmm

ggsave(rainfall_glmm, 
       file = "03_Figures/Preliminary_figures/rainfall_experiment_glmm.tiff", 
       dpi = 150, 
       width = 14, 
       height = 8)

