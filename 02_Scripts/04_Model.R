# ------------------------ Model Fitting ---------------------- #

library(tidyverse)
library(glmmTMB)
library(mgcv)
library(lme4)
library(lubridate)
library(DHARMa)
library(mgcViz)
library(vegan)
library(plotrix)

# read in the data --------------------------------------------------------

data_wide <- read.csv("01_Data/compiled_data_wideform.csv")
data_long <- read.csv("01_Data/compiled_data_longform.csv")

vars <- c("not_seagrass", "sg_cymodocea_sp", "sg_zmhu", "sg_halo_oval", "sg_syris", "sg_hal_spin", "sg_unknown", "total_sg")

transform_fn <- function(p) (p * (length(p) - 1) + 0.5) / length(p) 

data_wide <- data_wide %>%
  mutate(dummy = 1)

data_wide[, 8:14] <- data_wide[, 8:14]/100
data_wide$total_sg <- data_wide$total_sg/100

data_wide[, 8:14] <- apply(data_wide[, 8:14], 2, transform_fn)
data_wide$total_sg <- transform_fn(data_wide$total_sg)

data_wide <- data_wide %>%
  dplyr:: mutate(year_month = lubridate:: ym(year_month))

data_wide$num_date <- as.numeric(data_wide$year_month)

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


data_long$value <- data_long$value/100

data_long <- data_long %>%
  dplyr:: mutate(year_month = lubridate:: ym(year_month))

data_long$num_date <- as.numeric(data_long$year_month)

data_summary <- data_long %>%
  group_by(num_date, category_name, zone) %>%
  dplyr:: summarise(mean.cover = mean(value), se.cover = plotrix::std.error(value))





# Total seagrass ----------------------------------------------------------


# --------- Total Seagrass Model ---------- 

# make the zone and transect a factor

data_wide$zone <- as.factor(data_wide$zone)
data_wide$transect <- as.factor(data_wide$transect)

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


AIC(total_gam, total_gam_zone1, total_gam_zone2, total_gam_zone_trans1, total_gam_zone_trans2, total_gam_zone_trans3)


par(mfrow = c(2, 2))
plot(total_gam_zone_trans3)
par(mfrow = c(1, 1))
gam.check(total_gam_zone_trans3)

k.check(total_gam_zone)

model_summary <- summary(total_gam_zone_trans3)

model_summary$p.table
model_summary$s.table

vis.gam(total_gam_zone_trans3, theta = 120, n.grid = 50, lwd = 0.4)

summary(total_gam_zone_trans2)
plot(total_gam_zone_trans3)
hist(residuals(total_gam_zone_trans3))
AIC(total_gam_zone_trans3)

par(mfrow = c(1, 1))
plot(total_gam_zone_trans3, all.terms = TRUE)




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





















# Species level GAM -------------------------------------------------------


# Fit a zostera model

zmhu_gam <- gam(sg_zmhu ~ s(num_date),
                        family = betar,
                        data = data_wide)
summary(zmhu_gam)

hist(residuals(zmhu_gam))

plot(sm(getViz(zmhu_gam), 1))



# Now use tidyr::expand
pred <- tidyr::expand(data_wide, 
                      nesting(zone),
                      num_date = seq(13726, 19539, by = 5), 
                      dummy = 0)

preds <- predict(zmhu_gam, newdata = pred, se.fit = T)

ilink <-  family(zmhu_gam)$linkinv

preds <- transform(preds,
                   fit = ilink(fit)*100,
                   upper = ilink(fit + (2 * se.fit))*100,
                   lower = ilink(fit - (2 * se.fit))*100,
                   newLabel = "Zostera muelleri/Halodule uninervis",
                   pred)


# Fit a Cymodocea model

cym_gam <- gam(sg_cymodocea_sp ~ s(num_date, k = 5),
                family = betar,
                data = data_wide)

summary(cym_gam)

hist(residuals(cym_gam))

plot(sm(getViz(cym_gam), 1))

# Now use tidyr::expand

preds1 <- predict(cym_gam, newdata = pred, se.fit = T)

ilink <-  family(cym_gam)$linkinv

preds1 <- transform(preds1,
                   fit = ilink(fit)*100,
                   upper = ilink(fit + (2 * se.fit))*100,
                   lower = ilink(fit - (2 * se.fit))*100,
                   newLabel = "Cymodocea sp",
                   pred)

preds <- rbind(preds, preds1)


# Fit a Halo_sp model

halo_sp_gam <- gam(sg_hal_spin ~ s(num_date, k = 5),
               family = betar,
               data = data_wide)

summary(halo_sp_gam)

hist(residuals(halo_sp_gam))

plot(sm(getViz(halo_sp_gam), 1))

# Now use tidyr::expand

preds2 <- predict(halo_sp_gam, newdata = pred, se.fit = T)

ilink <-  family(halo_sp_gam)$linkinv

preds2 <- transform(preds2,
                    fit = ilink(fit)*100,
                    upper = ilink(fit + (2 * se.fit))*100,
                    lower = ilink(fit - (2 * se.fit))*100,
                    newLabel = "Halophila spinulosa",
                    pred)

preds <- rbind(preds, preds2)

# Fit a syris model

syris_gam <- gam(sg_syris ~ s(num_date, k = 5),
                   family = betar,
                   data = data_wide)

summary(syris_gam)

hist(residuals(syris_gam))

plot(sm(getViz(syris_gam), 1))

# Now use tidyr::expand

preds3 <- predict(syris_gam, newdata = pred, se.fit = T)

ilink <-  family(syris_gam)$linkinv

preds3 <- transform(preds3,
                    fit = ilink(fit)*100,
                    upper = ilink(fit + (2 * se.fit))*100,
                    lower = ilink(fit - (2 * se.fit))*100,
                    newLabel = "Syringodium isoetifolium",
                    pred)

preds <- rbind(preds, preds3)


# Fit a halo ov model

halo_ov_gam <- gam(sg_halo_oval ~ s(num_date, k = 5),
                 family = betar,
                 data = data_wide)

summary(halo_ov_gam)

hist(residuals(halo_ov_gam))

plot(sm(getViz(halo_ov_gam), 1))

# Now use tidyr::expand

preds4 <- predict(halo_ov_gam, newdata = pred, se.fit = T)

ilink <-  family(halo_ov_gam)$linkinv

preds4 <- transform(preds4,
                    fit = ilink(fit)*100,
                    upper = ilink(fit + (2 * se.fit))*100,
                    lower = ilink(fit - (2 * se.fit))*100,
                    newLabel = "Halophila ovalis",
                    pred)

preds <- rbind(preds, preds4)


# Fit unknown seagrass model

sgunknown_gam <- gam(sg_unknown ~ s(num_date, k = 5),
                   family = betar,
                   data = data_wide)

summary(sgunknown_gam)

hist(residuals(sgunknown_gam))

plot(sm(getViz(sgunknown_gam), 1))

# Now use tidyr::expand

preds5 <- predict(sgunknown_gam, newdata = pred, se.fit = T)

ilink <-  family(sgunknown_gam)$linkinv

preds5 <- transform(preds5,
                    fit = ilink(fit)*100,
                    upper = ilink(fit + (2 * se.fit))*100,
                    lower = ilink(fit - (2 * se.fit))*100,
                    newLabel = "Unknown Seagrass",
                    pred)

preds <- rbind(preds, preds5)


# Fit other seagrass model

other_gam <- gam(not_seagrass ~ s(num_date, k = 5),
                     family = betar,
                     data = data_wide)

summary(other_gam)

hist(residuals(other_gam))

plot(sm(getViz(other_gam), 1))

# Now use tidyr::expand

preds6 <- predict(other_gam, newdata = pred, se.fit = T)

ilink <-  family(other_gam)$linkinv

preds6 <- transform(preds6,
                    fit = ilink(fit)*100,
                    upper = ilink(fit + (2 * se.fit))*100,
                    lower = ilink(fit - (2 * se.fit))*100,
                    newLabel = "Other",
                    pred)

preds <- rbind(preds, preds6)



model_plot <- ggplot() +
  geom_point(data = data_summary, aes(x = num_date, y = 100*mean.cover, group = category_name, col = category_name)) +
  geom_errorbar(data = data_summary, aes(x = num_date, y = 100*mean.cover, group = category_name, colour = category_name, 
                                             ymax = 100*(mean.cover + se.cover), ymin = 100*(mean.cover - se.cover))) +
    geom_line(data = preds, aes(x = num_date, y = fit, group = newLabel), lwd = 1) +
    geom_ribbon(data = preds, aes(x = num_date, y = fit, group = newLabel, ymax = upper, ymin = lower), col = NA, alpha = 0.3) +
    facet_wrap(~ category_name, scales = "free_y") +
    labs(x = "", y = "% Cover") +
    theme_bw() +
    theme(legend.position = "none")

model_plot

ggsave(model_plot, 
       file = "03_Figures/final_figures/transect_plots.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)



