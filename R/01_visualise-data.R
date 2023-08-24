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
tidy.habitat <- read.csv("data/tidy/2021-2022_SwC_BRUV-BOSS_Habitat.csv") %>%   # Need to transform into correct columns for plotting
  glimpse()

# Visualise data as a scatterpie ----
sf_use_s2(F)                                                                    # Otherwise st_crop errors - need to find a better way
marine.parks <- st_read("data/spatial/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
  st_crop(xmin = min(tidy.habitat$longitude),
          xmax = max(tidy.habitat$longitude),
          ymin = min(tidy.habitat$latitude),
          ymax = max(tidy.habitat$latitude)) %>%
  dplyr::mutate(ZONE_TYPE = str_replace_all(ZONE_TYPE, "\\s*\\([^\\)]+\\)", "")) %>%
  dplyr::filter(ZONE_TYPE %in% c("National Park Zone", "Sanctuary Zone"))
plot(marine.parks[1])

aus <- st_read("data/spatial/cstauscd_r.mif", crs = 4283) %>%
  dplyr::filter(!FEAT_CODE %in% "sea") %>%
  st_crop(xmin = min(tidy.habitat$longitude),
          xmax = max(tidy.habitat$longitude),
          ymin = min(tidy.habitat$latitude),
          ymax = max(tidy.habitat$latitude))

ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.2) +
  geom_sf(data = marine.parks, aes(fill = ZONE_TYPE), alpha = 2/5, colour = NA) +
  scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                               "Sanctuary Zone" = "#bfd054"),
                    name = "Marine Parks") +
  new_scale_fill() +
  geom_scatterpie(data = tidy.habitat, aes(x = longitude, y = latitude),
                  cols = c("sessile.invertebrates", "macroalgae", "seagrasses", "rock", "sand"),
                  pie_scale = 0.65, colour = NA) +
  scale_fill_manual(values = c("sessile.invertebrates" = "plum",
                               "macroalgae" = "darkgoldenrod4",
                               "seagrasses" = "forestgreen",
                               "rock" = "grey40",
                               "sand" = "wheat"), 
                    name = "Habitat") +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf() +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#b9d1d6"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

