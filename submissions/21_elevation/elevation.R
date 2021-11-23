
# 30DayMapChallenge Day 21: Elevation - Hillshade relief of Taiwan
# Shandiya Balasubramaniam
# 21 November 2021

library(ggplot2)
library(here)
library(basemaps)
library(showtext)

font_add_google("Rajdhani", "raj")
showtext_auto()

# get basemap boundaries interactively
ext <- draw_ext()

# hillshade
p <- ggplot() + 
  basemap_gglayer(ext, 
                  map_service = "esri", 
                  map_type = "world_hillshade") + 
  coord_sf() +
  scale_fill_identity() +
  labs(title = "Elevation of Taiwan",
       caption = "Shandiya Balasubramaniam") +
  theme_void() +
  theme(
    plot.title = element_text(
      family = "raj",
      size = rel(4),
      colour = "saddlebrown",
      vjust = -20,
      hjust = 0.18
    ),
    plot.caption = element_text(
      family = "raj",
      size = rel(1.2),
      colour = "saddlebrown",
      vjust = 55,
      hjust = 0.9
    )
  )

ggsave(here("submissions", "21_elevation", "21_elevation.png"), 
       p, dpi = 300, type = "cairo", width = 3, height = 6, units = "in")



