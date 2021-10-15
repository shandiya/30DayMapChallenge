
# 30DayMapChallenge Day 1: Points - London Plane Trees in the City of Melbourne
# Shandiya Balasubramaniam
# 1 November 2021

library(dplyr)
library(readr)
library(ggplot2)
library(ggmap)
library(janitor)
library(here)
library(showtext)

font_add(family = "futura", regular = "futura light bt_1.ttf") 
showtext_auto()

# data from City of Melbourne
# https://data.melbourne.vic.gov.au/Environment/Trees-with-species-and-dimensions-Urban-Forest-/fp38-wiyy
trees <- read_csv(here("data", "Trees__with_species_and_dimensions__Urban_Forest_.csv"))

lp <- trees %>% 
  clean_names() %>% 
  filter(common_name == "London Plane") %>% 
  select(latitude, longitude, year_planted)

# bounding box
left <- min(lp$longitude)
bottom <- min(lp$latitude)
right <- max(lp$longitude)
top <- max(lp$latitude)
bbox <- c(left, bottom, right, top)

# labels
title = "London Plane Trees in the City of Melbourne"
caption = "Shandiya Balasubramaniam • Data: City of Melbourne • Map: OpenStreetMap, Stamen Design"

# map!
ggmap(get_stamenmap(bbox, maptype = "terrain-lines", zoom = 15)) +
  geom_point(data = lp, 
             aes(x = longitude, y = latitude), 
             size = 0.1, 
             colour = "olivedrab") +
  coord_equal() +
  labs(title = title, caption = caption) +
  theme_void() +
  theme(text = element_text(family = "futura"),
        plot.title = element_text(hjust = 1, size = 20), 
        plot.caption = element_text(hjust = 1, size = 12)) 

ggsave(here("submissions", "01_points", "01_points.png"), width = 3, height = 3, units = "in")


  


