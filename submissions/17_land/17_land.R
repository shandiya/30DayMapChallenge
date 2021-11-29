
# 30DayMapChallenge Day 17: Land - Residential dwelling types in Melbourne
# Shandiya Balasubramaniam
# 17 November 2021

library(RSocrata)
library(ggplot2)
library(dplyr)
library(sf)
library(showtext)
library(here)
library(osmdata)
library(osmextract)
library(ggtext)

# building data

res <- read.socrata("https://data.melbourne.vic.gov.au/resource/rm92-h5tq.csv")  

# basemaps

water <- opq(bbox = 'Melbourne') %>%
  add_osm_feature(key = 'natural', value = 'water') %>% 
  osmdata_sf()

melb <- oe_get("Melbourne", query = "SELECT highway, geometry FROM 'lines'")
road_types <- c("primary", "secondary", "tertiary", "unclassified")
melb_roads <- melb %>% 
  filter(highway %in% road_types) 

# plot 

font_add_google("Montserrat Alternates", "malt")
showtext_auto()

subtitle <- "\n<span style = 'color:deepskyblue4;'>Houses/Townhouses</span>, 
<span style = 'color:darkolivegreen;'>Residential Apartments</span>, and 
<span style = 'color:firebrick;'>Student Apartments</span> in Melbourne"

caption <- "Shandiya Balasubramaniam • Data: City of Melbourne • Map: OpenStreetMap"

p <- ggplot() +
  geom_sf(data = water$osm_polygons, 
          colour = NA, 
          fill = "#6F9EAF") +
  geom_sf(data = melb, 
          colour = "#888888",
          size = 0.1) +
  geom_point(data = res,
             aes(x = x_coordinate, 
                 y = y_coordinate, 
                 colour = dwelling_type),
             size = 0.3, 
             alpha = 0.5) +
  scale_colour_manual(values = c("deepskyblue4", "darkolivegreen", "firebrick")) +
  coord_sf(xlim = c(144.9, 145),
           ylim = c(-37.85, -37.775)) +
  labs(subtitle = subtitle, caption = caption) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(colour = "#F6D887", fill = "#F6D887"),
    panel.background = element_rect(colour = "#F6D887", fill = "#F6D887"),
    plot.subtitle = element_markdown(
      family = "malt",
      colour = "black",
      size = 19, 
      hjust = 0.02, 
      padding = unit(c(2, 2, 2, 2), "pt")),
    plot.caption = element_text(
      family = "malt",
      colour = "black",
      size = 10,
      hjust = 0.98)
  )

ggsave(here("submissions", "17_land", "17_land.png"), 
       p, dpi = 300, type = "cairo", width = 5, height = 5, units = "in")

