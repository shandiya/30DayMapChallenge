
# 30DayMapChallenge Day 28: The earth is not flat - projections
# Shandiya Balasubramaniam
# 28 November 2021

library(ggplot2)
library(mapproj)
library(purrr)
library(patchwork)
library(extrafont)
library(here)


# get data 
world_raw <- map_data("world")

world <- world_raw[world_raw$long <= 180,] # messes with the polygons otherwise

# cartesian coordinates
world_map <- ggplot(world, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "slategrey", colour = NA) 

# list of projections for looping through
projections <- list("mercator", "sinusoidal", "mollweide",
                    "gilbert", "gnomonic", "vandergrinten", 
                    "orthographic", "globular", "lagrange")

# custom theme to reduce the amount of code
theme_map <- function(){ 
  font <- "Lato"   
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.subtitle = element_text(
        family = font, 
        size = 8,
        colour = "#777777", 
        hjust = 1)
    )
}

# create all the maps 
plot_fun <- function(x) {
  world_map + 
    coord_map(projection = x) + 
    labs(subtitle = x) +
    theme_map()
  }

all_maps <- map(projections, ~plot_fun(.x))

p1 <- world_map + 
  coord_equal() + 
  labs(subtitle = "cartesian coordinates") + 
  theme_map()
p2 <- wrap_plots(all_maps)

patch <-  p1 / p2

ggsave(here("submissions", "28_flat_earth", "28_flat_earth.png"),
       dpi = 300, type = "cairo", width = 5, height = 8, units = "in")    

