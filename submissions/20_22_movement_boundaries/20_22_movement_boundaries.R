
# 30DayMapChallenge Days 20, 22: Movement & Boundaries - Isochrones 
# Shandiya Balasubramaniam
# 20, 22 November 2021

library(osrm)
library(osmextract)
library(dplyr)
library(here)
library(showtext)
library(ggtext)
library(ggmap)
library(ggplot2)

# data ------------
iso_melb <- osrmIsochrone(loc = c(144.9631,-37.8136),
                     breaks = seq(from = 0, to = 60, by = 15),
                     osrm.profile = "foot")

melb <- oe_get("Melbourne", query = "SELECT highway, geometry FROM 'lines'")

base_melb <- melb %>%
  filter(highway %in% c(
      "residential",
      "primary",
      "secondary",
      "living_street",
      "pedestrian",
      "path",
      "footway",
      "track",
      "corridor")
  )

#  plot ------------------
pal <- c("#012a4a", "#01497c", "#2a6f97", "#61a5c2")

title <- "ISOCHRONES"
subtitle <- "Walking boundaries within
<span style='color:#012a4a;'>15</span>,
<span style='color:#01497c;'>30</span>,
<span style='color:#2a6f97;'>45</span>, and
<span style='color:#61a5c2;'>60</span> minutes"
caption <- "Shandiya Balasubramaniam | Data: © OpenStreetMap contributors"

font_add_google("Spartan", "spartan")
font_add_google("Crimson Pro", "crimson")
showtext_auto()

p <- ggplot() + 
  geom_sf(data = base_melb, colour = "grey", size = 0.2) + 
  geom_sf(data = iso_melb, 
          aes(fill = as.factor(id)),
          colour = NA, alpha = 0.7) +
  geom_point(aes(x = 144.9631, y = -37.8136), 
             colour = "black") +
  coord_sf(xlim = c(144.9, 145.03), 
           ylim = c(-37.85, -37.77)) + 
  scale_fill_manual(values = pal) +
  labs(title = title,
       subtitle = subtitle,
       caption = caption) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(
      family = "spartan",
      size = 50,
      colour = "#444444"), 
    plot.subtitle = element_markdown(
      family = "crimson",
      colour = "#444444",
      size = 28), 
    plot.caption = element_text(
      family = "crimson",
      hjust = 0.95,
      size = 18,
      colour = "#444444"))

ggsave(here("submissions", "20_22_movement_boundaries", "20_22_movement_boundaries.png"), 
       p, dpi = 300, type = "cairo", width = 4, height = 4, units = "in")







