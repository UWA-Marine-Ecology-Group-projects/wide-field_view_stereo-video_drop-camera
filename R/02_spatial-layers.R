rm(list = ls())

# Load libraries ----
library(sf)
library(terra)
library(stars)
library(starsExtra)

# Load bathymetry data ----
bathy <- rast("data/spatial/rasters/swc_multibeam_UTM50.tif")                   # UTM zone 50

# Create bathymetry derivatives
preds <- terrain(bathy, neighbors = 8,
                 v = c("slope", "aspect", "TPI", "TRI", "roughness"),           # Remove here as necessary
                 unit = "degrees")

# Calculate detrended bathymetry
zstar <- st_as_stars(bathy)                                                     # Convert to a stars object
detre <- detrend(zstar, parallel = 8)                                           # Detrend bathymetry - This usually runs quite slow!
detre <- as(object = detre, Class = "SpatRaster")                               # Convert it to a terra raster
names(detre) <- c("detrended", "lineartrend")

preds <- rast(list(bathy, preds, detre[[1]]))                                   # Stack the derivatives with the bathymetry
names(preds)[1] <- "depth"

saveRDS(preds, file = "data/spatial/rasters/2021-2022_SwC_bathymetry-derivatives.rds") # File is too large so is ignored

# Load in the habitat data and extract derivatives ----
tidy.habitat <- read.csv("data/tidy/2021-2022_SwC_BOSS_Habitat.csv") %>%
  dplyr::filter(!level_2 %in% "Unscorable") %>%
  # Make broad habitat levels for modelling
  dplyr::mutate(habitat = case_when(level_2 %in% c("Sponges", "Cnidaria",
                                                   "Bryozoa", "Sessile invertebrates",
                                                   "Ascidians", "Echinoderms") ~ "sessile.invertebrates",
                                    level_2 %in% "Macroalgae" ~ "macroalgae",
                                    level_2 %in% "Seagrasses" ~ "seagrasses",
                                    level_2 %in% "Substrate" & level_3 %in% "Unconsolidated (soft)"~ "sand",
                                    level_2 %in% "Substrate" & level_3 %in% "Consolidated (hard)"~ "rock")) %>%
  group_by(campaignid, sample) %>%
  dplyr::mutate(total.points.annotated = sum(points)) %>%
  ungroup() %>%
  glimpse()

habitat.vect <- vect(tidy.habitat, geom = c("longitude", "latitude"), crs = "epsg:4326") %>%
  project(bathy)                                                                # Project points to match crs of multibeam
plot(bathy)
plot(habitat.vect, add = T)
