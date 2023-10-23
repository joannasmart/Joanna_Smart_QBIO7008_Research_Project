# ------------------------ Model Fitting ---------------------- #

library(tidyverse)
library(mgcv)
library(lubridate)
library(DHARMa)
library(mgcViz)
library(plotrix)

# read in the data an tidy --------------------------------------------------------

data_wide <- read.csv("01_Data/compiled_data_wideform.csv")
data_long <- read.csv("01_Data/compiled_data_longform.csv")

# create the function for the transformation (so data is [0,1])

transform_fn <- function(p) (p * (length(p) - 1) + 0.5) / length(p) 

# create a dummy numeric variable column 

data_wide <- data_wide %>%
  dplyr:: mutate(dummy = 1)

# make the results (percentages) into proportions 

data_wide[, 8:14] <- data_wide[, 8:14]/100
data_wide$total_sg <- data_wide$total_sg/100

# transform the proportion data

data_wide[, 8:14] <- apply(data_wide[, 8:14], 2, transform_fn)
data_wide$total_sg <- transform_fn(data_wide$total_sg)

# make sure the dates are dates
data_wide <- data_wide %>%
  dplyr:: mutate(year_month = lubridate:: ym(year_month))

# make a numeric date column

data_wide$num_date <- as.numeric(data_wide$year_month)

# select only the desired banks

data_wide <- data_wide %>%
  filter(zone %in% c("AM", "MO", "MA", "WA"))

data_long <- data_long %>%
  filter(zone %in% c("AM", "MO", "MA", "WA"))


# Summary data ------------------------------------------------------------

# create summary data

data_long$category_name <- as.factor(data_long$category_name)

levels(data_long$category_name) <- c("Other", "Cymodocea sp", "Halophila spinulosa", "Halophila ovalis", 
                                     "Syringodium isoetifolium", "Unknown Seagrass", "Zostera muelleri/Halodule uninervis", 
                                     "Total Seagrass")


data_long$value <- data_long$value/100 # make the results (percentages) into proportions 

data_long <- data_long %>%
  dplyr:: mutate(year_month = lubridate:: ym(year_month)) # make sure dates are dates

data_long$num_date <- as.numeric(data_long$year_month) # create a numeric date column

data_summary <- data_long %>%
  group_by(num_date, category_name, zone) %>%
  dplyr:: summarise(mean.cover = mean(value), se.cover = plotrix::std.error(value)) # calculate the means and SE



# Total seagrass ----------------------------------------------------------


# --------- Total Seagrass Model ---------- 

# make the zone and transect a factor

data_wide$zone <- as.factor(data_wide$zone)
data_wide$transect <- as.factor(data_wide$transect)

# Create different GAM models for comparison with different explanatory variable combinations

total_gam <- gam(total_sg ~ s(num_date, bs = "cr"),
                 family = betar,
                 data = data_wide)

total_gam_zone1 <- gam(total_sg ~ s(num_date, bs = "cr", by = zone),
                       family = betar,
                       data = data_wide)

total_gam_zone2 <- gam(total_sg ~ s(num_date, bs = "cr") + zone,
                       family = betar,
                       data = data_wide)

total_gam_zone_trans1 <- gam(total_sg ~ s(num_date, bs = "cr") + zone + transect,
                             family = betar,
                             data = data_wide)

total_gam_zone_trans2 <- gam(total_sg ~ s(num_date, bs = "cr", by = zone) + transect,
                             family = betar,
                             data = data_wide)

total_gam_zone_trans3 <- gam(total_sg ~ s(num_date, bs = "cr", by = zone, k = 10) + s(transect, bs = "re", by = dummy),
                             family = betar,
                             data = data_wide)

# compare the AICs for each model

AIC(total_gam, total_gam_zone1, total_gam_zone2, total_gam_zone_trans1, total_gam_zone_trans2, total_gam_zone_trans3)


# Create diagnostic plots for the 'best' model from the AIC

par(mfrow = c(3, 2))
plot(total_gam_zone_trans3)
par(mfrow = c(1, 1))
gam.check(total_gam_zone_trans3)

model_summary <- summary(total_gam_zone_trans3)

model_summary$p.table
model_summary$s.table

hist(residuals(total_gam_zone_trans3))


# Visualise with mgcVis

b <- getViz(total_gam_zone_trans3)
print(plot(b, allTerms = T), pages = 1)


# Make predicted data

pred_total_gam <- tidyr::expand(data_wide, 
                                nesting(zone, transect),
                                num_date = seq(13726, 19539, by = 20), 
                                dummy = 0)

preds_total <- predict(total_gam_zone_trans3, newdata = pred_total_gam, se.fit = T)

ilink <-  family(total_gam_zone_trans3)$linkinv

preds_total <- transform(preds_total,
                         fit = ilink(fit)*100,
                         upper = ilink(fit + (2 * se.fit))*100,
                         lower = ilink(fit - (2 * se.fit))*100,
                         newLabel = "Total_sg",
                         pred_total_gam)


# Change numeric dates to ymd

preds_total$date <- lubridate:: as_date(preds_total$num_date)

preds_total <- preds_total %>%
  dplyr:: mutate(date = lubridate:: ymd(date))

data_summary$date <- lubridate::as_date(data_summary$num_date)

data_summary <- data_summary %>%
  dplyr:: mutate(date = lubridate:: ymd(date))

# plot the data

zone.labs <- c("Amity", "Maroom", "Moreton", "Wanga-Wallen")
names(zone.labs) <- c("AM", "MA", "MO", "WA")

# Plot the GAM model predicted data with SE and save

model_plot_total <- ggplot() +
  geom_point(data = data_summary %>% filter(category_name == "Total Seagrass"), aes(x = date, y = 100*mean.cover, group = zone)) +
  geom_jitter(data = data_wide, aes(x = year_month, y = total_sg*100), size = 0.5, alpha = 0.02, width = 100) +
  geom_errorbar(data = data_summary %>% filter(category_name == "Total Seagrass"), aes(x = date, y = 100*mean.cover, 
                                                                                       ymax = 100*(mean.cover + se.cover), ymin = 100*(mean.cover - se.cover))) +
  geom_line(data = preds_total, aes(x = date, y = fit, group = newLabel)) +
  geom_ribbon(data = preds_total, aes(x = date, y = fit, group = newLabel, ymax = upper, ymin = lower), col = NA, alpha = 0.3) +
  facet_wrap(~ zone, scales = "free_y", labeller = labeller(zone = zone.labs)) +
  labs(x = "Year", y = "% Cover") +
  theme_bw() +
  theme(legend.position = "none") 

model_plot_total

ggsave(model_plot_total, 
       file = "03_Figures/final_figures/total_seagrass_model.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)
