###
# Project: Wide-field view stereo-video drop-camera
# Data:    BOSS Habitat data
# Task:    Visualise habitat data
# Author:  Claude Spencer
# Date:    August 2023
##


# Clear the environment ----
rm(list = ls())

# Load libraries ----
library(tidyverse)
library(GlobalArchive)
library(ggplot2)
library(scatterpie)
library(sf)
library(ggnewscale)

# Load tidy data
tidy.habitat <- read.csv("data/tidy/2021-2022_SwC_BOSS_Habitat.csv") %>%
  dplyr::filter(!level_2 %in% "Unscorable") %>%
  # Make broad habitat levels for plotting
  dplyr::mutate(habitat = case_when(level_2 %in% c("Sponges", "Cnidaria",
                                                   "Bryozoa", "Sessile invertebrates",
                                                   "Ascidians", "Echinoderms") ~ "sessile.invertebrates",
                                    level_2 %in% "Macroalgae" ~ "macroalgae",
                                    level_2 %in% "Seagrasses" ~ "seagrasses",
                                    level_2 %in% "Substrate" & level_3 %in% "Unconsolidated (soft)"~ "sand",
                                    level_2 %in% "Substrate" & level_3 %in% "Consolidated (hard)"~ "rock")) %>%
  pivot_wider(names_from = habitat, values_from = count, values_fill = 0) %>%
  glimpse()

# Load shapefiles for plotting ----
sf_use_s2(F)                                                                    # Needed so st_crop doesn't error when cropping with lat longs                                                                   # Otherwise st_crop errors - need to find a better way
# Marine park data from CAPAD 2022
marine.parks <- st_read("data/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
  st_crop(xmin = min(tidy.habitat$longitude),
          xmax = max(tidy.habitat$longitude),
          ymin = min(tidy.habitat$latitude),
          ymax = max(tidy.habitat$latitude)) %>%
  dplyr::mutate(ZONE_TYPE = str_replace_all(ZONE_TYPE, 
                                            "\\s*\\([^\\)]+\\)", "")) %>%
  dplyr::filter(ZONE_TYPE %in% c("National Park Zone", "Sanctuary Zone"))

# High res outline for Australia
aus <- st_read("data/spatial/shapefiles/cstauscd_r.mif", crs = 4283) %>%
  dplyr::filter(!FEAT_CODE %in% "sea") %>%
  st_crop(xmin = min(tidy.habitat$longitude),
          xmax = max(tidy.habitat$longitude),
          ymin = min(tidy.habitat$latitude),
          ymax = max(tidy.habitat$latitude))

# Coastal waters limit
cwatr <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_crop(xmin = min(tidy.habitat$longitude),
          xmax = max(tidy.habitat$longitude),
          ymin = min(tidy.habitat$latitude),
          ymax = max(tidy.habitat$latitude))

# Visualise data as a scatterpie ----
ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.2) +
  geom_sf(data = marine.parks, aes(fill = ZONE_TYPE), alpha = 2/5, colour = NA) +
  scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                               "Sanctuary Zone" = "#bfd054"),
                    name = "Marine Parks") +
  new_scale_fill() +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.3) +
  geom_scatterpie(data = tidy.habitat, aes(x = longitude, y = latitude),
                  cols = c("sessile.invertebrates", "macroalgae", "seagrasses", "rock", "sand"),
                  pie_scale = 0.25, colour = NA) +
  scale_fill_manual(values = c("sessile.invertebrates" = "plum",
                               "macroalgae" = "darkgoldenrod4",
                               "seagrasses" = "forestgreen",
                               "rock" = "grey40",
                               "sand" = "wheat"), 
                    name = "Habitat") +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(114.75, 114.95), ylim = c(-34.01, -34.14)) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#b9d1d6"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

