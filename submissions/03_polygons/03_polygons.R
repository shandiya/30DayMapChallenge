
# 30DayMapChallenge Day 3: Polygons - Building Footprint in Melbourne
# Shandiya Balasubramaniam
# 3 November 2021

library(here)
library(tidyverse)
library(sf)
library(RSocrata)
library(showtext)

font_add(family = "CenturyGothic", regular = "GOTHIC.ttf")
showtext_auto()

# building footprint data from CoM
# https://data.melbourne.vic.gov.au/Property/2020-Building-Footprints/th7x-7gv7
raw_foots <- read.socrata("https://data.melbourne.vic.gov.au/resource/th7x-7gv7.csv")
foots_sf <- st_as_sf(raw_foots, wkt = "the_geom", crs = 4326)

# plot
caption = "Shandiya Balasubramaniam | Buildings in Melbourne | Data: City of Melbourne"
my_col = c("#FFBE00", "#FA0001", "#006697", "#000000", "#FFF2D5")
ggplot(data = foots_sf) +
  geom_sf(aes(fill = roof_type), colour = NA) +
  scale_fill_manual(values = my_col) +
  coord_sf(xlim = c(144.94, 144.97), ylim = c(-37.82, -37.80)) +
  labs(caption = caption) +  
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family = "CenturyGothic"))

ggsave(here("submissions", "03_polygons", "03_polygons.png"), width = 3, height = 3, units = "in")

  


