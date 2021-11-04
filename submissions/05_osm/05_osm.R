
library(here)
library(osmdata)
library(tidyverse)
library(ggtext)
library(sf)
library(showtext)


# get data from OpenStreetMap using overpass API -----------
# keys and values to build osm query https://taginfo.openstreetmap.org/keys
# admin level boundaries https://osm-boundaries.com/Map

benches <- getbb("Melbourne") %>%
  opq() %>%
  add_osm_feature("amenity", "bench") %>%
  osmdata_sf()

boundary <- getbb("Melbourne") %>%
  opq() %>%
  add_osm_feature("admin_level", "7") %>%
  osmdata_sf()

# wrangling-------------

benches_osm <- benches$osm_points

benches_tidy <- benches_osm %>%
  select(osm_id, amenity, backrest, geometry) %>% 
  filter(!is.na(amenity)) %>% 
  filter(!is.na(backrest))
 # mutate(backrest_modified = replace_na(backrest, "unknown"))

melb <- boundary$osm_multipolygons

# remove benches outside of melb
melb_benches <- benches_tidy %>% 
  mutate(within = as.integer(st_intersects(benches_tidy, melb))) %>% 
  filter(!is.na(within))

# plot! ---------

# labs
title <- "BENCHES"
subtitle <- "<span style = 'color:orange;'>with</span> and 
            <span style = 'color:steelblue4;'>without</span> backrests"
caption <- "Shandiya Balasubramaniam | © OpenStreetMap contributors"

# fonts
font_add_google("Noto Sans", "noto")
font_add_google("Quicksand", "quicksand")
showtext_auto()

p <- ggplot() +
  geom_sf(data = melb, fill = "slategray3", colour = NA, alpha = 0.5)+
  geom_sf(data = filter(melb_benches, backrest == "yes"), 
                        colour = "orange", 
                        alpha = 0.6) +
  geom_sf(data = filter(melb_benches, backrest == "no"), 
                        colour = "steelblue4", 
                        alpha = 0.6) + 
  coord_sf() +
  labs(title = title, subtitle = subtitle, caption = caption) +
  theme_void() +
  theme(
    legend.position = "none",
    panel.background = element_rect(colour = "moccasin", fill = "moccasin"),
    plot.background = element_rect(colour = "moccasin", fill = "moccasin"),
    plot.title = element_text(
      hjust = 0.5, 
      family = "noto", 
      face = "bold", 
      size = 75, 
      colour = "darkgreen"),
    plot.subtitle = element_textbox_simple(
      family = "quicksand",
      colour = "olivedrab",
      size = 30, 
      halign = 0.5),
    plot.caption = element_text(
      family = "quicksand",
      hjust = 0.95,
      size = 20, 
      colour = "darkgreen"))

ggsave(here("submissions", "05_osm", "05_osm.png"), 
       p, dpi = 300, type = "cairo", width = 1500, height = 1500, units = "px")
  
