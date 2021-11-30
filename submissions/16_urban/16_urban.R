
# 30DayMapChallenge Day 16: Urban - Water flow
# Shandiya Balasubramaniam
# 16 November 2021

library(here)
library(sf)
library(RSocrata)
library(ggplot2)
library(showtext)
library(forcats)

# data

wf <- st_read(here("data", 
                   "water_flow", 
                   "geo_export_38c13833-c78a-4b7d-ba6a-af2f235fff72.shp"))

# plot

title <- "-URBAN FOREST-"
subtitle <- "water flow routes over land"
caption <- "Shandiya Balasubramaniam | Data: City of Melbourne"

font_add_google("Alegreya Sans", "aleg_sans")
font_add_google("Playfair Display", "playfair")
showtext_auto()   

my_pal <- c("#caf0f8", "#ade8f4", "#90e0ef", "#48cae4", "#00b4d8")

p <- ggplot() +
  geom_sf(data = wf, aes(colour = as_factor(grid_code)), size = 0.2) +
  scale_color_manual(values = my_pal) +
  coord_sf() +
  labs(title = title, subtitle = subtitle, caption = caption) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(colour = "#414833", 
                                       fill = "#414833"),
        panel.background = element_rect(colour = "#414833", 
                                        fill = "#414833"),
        plot.title = element_text(family = "playfair", 
                                  colour = "#bbbbbb",
                                  hjust = 0.5,
                                  size = rel(4),
                                  vjust = -0.5),
        plot.subtitle = element_text(family = "aleg_sans", 
                                     colour = "#bbbbbb",
                                     hjust = 0.5,
                                     size = rel(2.5), 
                                     vjust = -0.2), 
        plot.caption = element_text(family = "aleg_sans", 
                                    colour = "#bbbbbb",
                                    hjust = 0.5,
                                    size = rel(1),
                                    vjust = 3))


ggsave(here("submissions", "16_urban", "16_urban.png"), 
       p, dpi = 300, type = "cairo", width = 5, height = 5, units = "in")

