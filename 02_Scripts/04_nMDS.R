# -------------------------------- nMDS Plots ------------------------------------ #

# load required packages

library(tidyverse)
library(vegan)
library(lattice)
library(permute)

# read in the data

data_wide <- read.csv("01_Data/compiled_data_wideform.csv")

# select the data for the NMDS plot (only percentage columns)

data_nmds <- data_wide %>%
  dplyr:: group_by(year, zone) %>%
  dplyr:: select(8:14) %>%
  dplyr:: summarise_if(is.numeric, mean, na.rm = TRUE)

# calculate a bray-curtis dissimilarity matrix

bray_dist <- vegan:: vegdist(as.matrix(sqrt(data_nmds[,3:9])), method = "bray") 

# run the NMDS

nMDS_data <- vegan:: metaMDS(bray_dist, k = 3)

# Extract the species data from the 'data_nmds' dataframe

species <- data_nmds[,3:9]

# Combine the data with the NMDS results (MDS1 and MDS2)

nmds <- cbind.data.frame(data_nmds[, 1:2], nMDS_data$points)

# Calculate the stress value from the NMDS analysis

stress <- nMDS_data$stress

# Calculate convex hulls for the NMDS data points, grouped by 'year'

nmds_hulls <- nmds %>%
  dplyr:: group_by(year) %>%
  dplyr:: slice(grDevices:: chull(MDS1, MDS2))

# Calculate convex hulls for the NMDS data points, grouped by 'bank'

nmds_hulls_zone <- nmds %>%
  dplyr:: group_by(zone) %>%
  dplyr:: slice(grDevices:: chull(MDS1, MDS2))

# Create a mapping of bank labels

zone.labs <- c("Amity", "Maroom", "Moreton", "Wanga-Wallen")
names(zone.labs) <- c("AM", "MA", "MO", "WA")

# Create the NMDS plot with year groupings and save 
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

# Create a NMDS plot with bank grouping and save

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


