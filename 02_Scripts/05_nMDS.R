# -------------------------------- nMDS Plots ------------------------------------ #

# need to load the datasets from 01

library(tidyverse)
library(readxl)
library(plyr)
library(vegan)
library(lattice)
library(permute)


data_wide <- read.csv("01_Data/compiled_data_wideform.csv")

data_nmds <- data_wide %>%
  dplyr:: group_by(year, zone) %>%
  dplyr:: select(8:14) %>%
  dplyr:: summarise_if(is.numeric, mean, na.rm = TRUE)

bray_dist <- vegan:: vegdist(as.matrix(sqrt(data_nmds[,3:9])), method = "bray")
nMDS_data <- vegan:: metaMDS(bray_dist, k = 3)

species <- data_nmds[,3:9]

nmds <- cbind.data.frame(data_nmds[, 1:2], nMDS_data$points)

nmds_centroids <- nmds %>% 
  dplyr:: group_by(year) %>% 
  dplyr:: summarize(MDS1 = mean(MDS1), MDS2 = mean(MDS2))

stress <- nMDS_data$stress

nmds_hulls <- nmds %>%
  dplyr:: group_by(year) %>%
  dplyr:: slice(grDevices:: chull(MDS1, MDS2))

nmds_hulls_zone <- nmds %>%
  dplyr:: group_by(zone) %>%
  dplyr:: slice(grDevices:: chull(MDS1, MDS2))

zone.labs <- c("Amity", "Maroom", "Moreton", "Wanga-Wallen")
names(zone.labs) <- c("AM", "MA", "MO", "WA")


nmds_plot <- ggplot() +
  geom_point(data = nmds, aes(x = MDS1, y = MDS2, col = as.factor(year)), size = 4) +
  geom_polygon(data = nmds_hulls, aes(x = MDS1, y = MDS2, group = as.factor(year), fill = as.factor(year)), alpha = 0.1) +
  annotate(geom = "text", label = paste("Stress =", round(stress, 2)), x = Inf, y = -Inf, hjust = 1.2, vjust = -1) +
  theme_bw(base_size = 12) +
  stat_ellipse() +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired") +
  scale_shape(labels = zone.labs) +
  labs(colour = "Year", fill = "Year")


nmds_plot

ggsave(nmds_plot, 
       file = "03_Figures/final_figures/nmds.tiff", 
       dpi = 300, 
       width = 10, 
       height = 10)

nmds_plot_zone <- ggplot() +
  geom_point(data = nmds, aes(x = MDS1, y = MDS2, col = zone), size = 4) +
  geom_polygon(data = nmds_hulls_zone, aes(x = MDS1, y = MDS2, group = as.factor(zone), fill = as.factor(zone)), alpha = 0.1) +
  annotate(geom = "text", label = paste("Stress =", round(stress, 2)), x = Inf, y = -Inf, hjust = 1.2, vjust = -1) +
  theme_bw(base_size = 12) +
  stat_ellipse() +
  scale_fill_brewer(palette = "Dark2", labels = zone.labs) +
  scale_color_brewer(palette = "Dark2", labels = zone.labs) +
  scale_shape(labels = zone.labs) +
  labs( colour = "Bank", fill = "Bank")

nmds_plot_zone

ggsave(nmds_plot_zone, 
       file = "03_Figures/final_figures/nmds_zone.tiff", 
       dpi = 300, 
       width = 10, 
       height = 10)


