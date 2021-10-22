
# 30DayMapChallenge Day 21: Elevation - Elevation of Australia
# Shandiya Balasubramaniam
# 21 November 2021

library(here)
library(elevatr)
library(rnaturalearth)
library(sf)
library(raster)
library(ggplot2)
library(scico)


# get all the data----------

# country and elevation
aus <- ne_states(country = "Australia", returnclass = "sf")
#elev <- get_elev_raster(locations = aus, z = 7, clip = "locations", serial = TRUE)

# save elevation to file to avoid having to keep downloading
# writeRaster(elev, filename = here("data", "elev.tif"), options = c("COMPRESS = DEFLATE", "PREDICTOR = 2", "ZLEVEL = 7"))
# to read in again 
elev <- raster(here("data", "elev.tif"))

# wrangle just the bits of the raster I want
cropped_elev <- crop(elev, aus)
elev_df <- as.data.frame(cropped_elev, xy = TRUE)
colnames(elev_df)[3] <- "elevation"
elev_complete <- elev_df[complete.cases(elev_df),]

# clear some space!
rm(list = c("elev", "elev_df", "cropped_elev"))

# plot -----------

ggplot() +                         
  geom_sf(data = aus, fill = NA, colour = NA) +
  geom_tile(data = elev_complete, aes(x = x, y = y, fill = elevation)) +
  scale_fill_scico(palette = "grayC", direction = -1) +
  coord_sf() +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#f0f0f0", colour = NA))
ggsave(here("submissions", "10_raster", "10_raster.png"), width = 3, height = 3, units = "in")

