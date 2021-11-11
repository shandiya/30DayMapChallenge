
# 30DayMapChallenge Day 9: Monochrome - Potato cake/fritter/scallop
# Shandiya Balasubramaniam
# 9 November 2021

library(here)
library(osmextract)
library(dplyr)
library(ggplot2)
library(sf)
library(showtext)
library(patchwork)


# get city maps

melb <- oe_get("Melbourne", query = "SELECT highway, geometry FROM 'lines'")
adelaide <- oe_get("Adelaide", query = "SELECT highway, geometry FROM 'lines'")
sydney <- oe_get("Sydney", query = "SELECT highway, geometry FROM 'lines'")

# filter to roads only

road_types <- c("primary", "secondary", "tertiary", "unclassified")
melb_roads <- melb %>% 
  filter(highway %in% road_types) 
adel_roads <- adelaide %>% 
  filter(highway %in% road_types)
syd_roads <- sydney %>% 
  filter(highway %in% road_types)

# central points (from google)

centres <- data.frame(
  city = c("melb", "adelaide", "sydney"),
  latitude = c(-37.840935, -34.921230, -33.865143), 
  longitude = c(144.946457, 138.599503, 151.209900)
)
centres_sf <- st_as_sf(centres, coords = c("longitude", "latitude"), crs = 4326)

# 10 km buffers around each central point

buffers <- centres_sf %>% 
  mutate(buff_geom = st_buffer(geometry, 10000))

# intersections

melb_int <- st_intersection(melb_roads, buffers$buff_geom[1])
adel_int <- st_intersection(adel_roads, buffers$buff_geom[2])
syd_int <- st_intersection(syd_roads, buffers$buff_geom[3])

# potato data at https://tinyurl.com/22e97fjx

# plot

font_add_google("Josefin Sans", "josefin")
font_add_google("Dosis", "dosis")
showtext_auto()

mp <- ggplot() +
  geom_sf(data = melb_int,
          aes(size = highway),
          colour = "white") +
  scale_size_manual(values = c(0.5, 0.3, 0.2, 0.1)) +
  coord_sf() +
  labs(title = "...cake", caption = "MELBOURNE") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = 'black', colour = 'black'),
        panel.background = element_rect(fill = 'black', colour = 'black'),
        plot.caption = element_text(
          family = "josefin",
          colour = "white",
          hjust = 0.5, 
          size = rel(2)),
        plot.title = element_text(
          family = "dosis",
          colour = "white",
          hjust = 0.5, 
          size = rel(2))) 

ap <- ggplot() +
  geom_sf(data = adel_int,
          aes(size = highway),
          colour = "white") +
  scale_size_manual(values = c(0.5, 0.3, 0.2, 0.1)) +
  coord_sf() +
  labs(title = "...fritter", caption = "ADELAIDE") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = 'black', colour = 'black'),
        panel.background = element_rect(fill = 'black', colour = 'black'),
        plot.caption = element_text(
          family = "josefin",
          colour = "white",
          hjust = 0.5, 
          size = rel(2)),
        plot.title = element_text(
          family = "dosis",
          colour = "white",
          hjust = 0.5, 
          size = rel(2))) 

sp <- ggplot() +
  geom_sf(data = syd_int,
          aes(size = highway),
          colour = "white") +
  scale_size_manual(values = c(0.5, 0.3, 0.2, 0.1)) +
  coord_sf() +
  labs(title = "...scallop", caption = "SYDNEY") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = 'black', colour = 'black'),
        panel.background = element_rect(fill = 'black', colour = 'black'),
        plot.caption = element_text(
          family = "josefin",
          colour = "white",
          hjust = 0.5, 
          size = rel(2)),
        plot.title = element_text(
          family = "dosis",
          colour = "white",
          hjust = 0.5, 
          size = rel(2)))

patch <- mp + ap + sp

p <- patch + plot_annotation(
  title = "Potato...",
  caption = "Shandiya Balasubramaniam • Data: © OpenStreetMap contributors",
  theme = theme(
    plot.title = element_text(size = 50, hjust = 0.5, family = "dosis", colour = "white"),
    plot.caption = element_text(size = 10, family = "josefin", colour = "white"),
    plot.background = element_rect(colour = 'black', fill = "black")
    )
  )

ggsave(here("submissions", "09_monochrome", "09_monochrome.png"), 
       p, dpi = 300, type = "cairo", width = 6, height = 3, units = "in")


