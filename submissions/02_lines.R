
# 30DayMapChallenge Day 2: Lines - Streets Within 5km Lockdown Radius
# Shandiya Balasubramaniam
# 2 November 2021

library(dplyr)
library(sfnetworks)
library(sf)
library(VicmapR)
library(ggplot2)
library(here)

# get centre of circle point as sf
centre <- data.frame(latitude = -37.79, longitude = 144.87)
centre_sf <- st_as_sf(centre, coords = c("longitude", "latitude"), crs = 4326)

# roads within 5km radius
my_roads <- vicmap_query(layer = "datavic:VMTRANS_TR_ROAD") %>%
  filter(DWITHIN(centre_sf, distance = 5000, units = "meters")) %>%
  dplyr::select(FEATURE_TYPE_CODE, ROAD_TYPE) %>%
  dplyr::collect()

# turn it into sfnetwork object
my_roads_net <- as_sfnetwork(my_roads)

# extract edges only
edges_only <- my_roads_net %>%
  st_geometry(., "edges") %>%
  st_as_sf()

# plot!
ggplot(edges_only) +
  geom_sf(colour = "#bbbbbb", size = 0.2) +
  coord_sf() +
  theme_void() +
  theme(panel.background = element_rect(fill = "#555555", colour = "#555555"))

ggsave(here("submissions", "02_lines.png"), width = 3, height = 3, units = "in")
