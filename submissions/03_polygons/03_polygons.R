
# 30DayMapChallenge Day 3: Polygons - Building Footprint in Melbourne
# Shandiya Balasubramaniam
# 3 November 2021

library(here)
library(tidyverse)
library(sf)
library(RSocrata)
library(showtext)

font_add(family = "arbonnie", regular = "ARBONNIE.ttf") 
showtext_auto()


# building footprint data from CoM----------
# https://data.melbourne.vic.gov.au/Property/2020-Building-Footprints/th7x-7gv7
raw_foots <- read.socrata("https://data.melbourne.vic.gov.au/resource/th7x-7gv7.csv")
foots_sf <- st_as_sf(raw_foots, wkt = "the_geom", crs = 4326)

# plot-----------
title = " Buildings in Melbourne"
caption = "Shandiya Balasubramaniam | Data: City of Melbourne    "
my_col = c("#FFBE00", "#FA0001", "#006697", "#000000", "#FFF2D5")

p <- ggplot(data = foots_sf) +
  geom_sf(aes(fill = roof_type), colour = NA) +
  scale_fill_manual(values = my_col) +
  coord_sf(xlim = c(144.94, 144.97), ylim = c(-37.82, -37.80)) +
  labs(title = title, caption = caption) +  
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(
          family = "arbonnie", 
          colour = "#006697", 
          size = 30),
        plot.caption = element_text(
          family = "arbonnie", 
          colour = "#006697", 
          size = 20),
        plot.background = element_rect(fill = '#F7F1D8', colour = NA),
        panel.background = element_rect(fill = '#F7F1D8', colour = NA)) 

ggsave(here("submissions", "03_polygons", "03_polygons.png"), 
       p, dpi = 300, type = "cairo", width = 4, height = 4, units = "in")

  


