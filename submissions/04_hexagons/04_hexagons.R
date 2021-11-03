
# 30DayMapChallenge Day 4: Hexagons - Superb Fairy-wren Sightings in Victoria
# Shandiya Balasubramaniam
# 4 November 2021

library(here)
library(readr)
library(dplyr)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(scales)
library(showtext)

# font_paths()
font_add(family = "lato", regular = "Lato-Regular.ttf") 
showtext_auto()


# get SFW records and tidy up-----------
# all records of superb fairy-wrens from the Atlas of Living Australia (ala.org.au)
sfw <- read_csv(here("data", "records-2021-10-11.csv"), show_col_types = FALSE)

# observations from Vic only
sfw_vic <- sfw %>%
  select(basisOfRecord, year, decimalLatitude, decimalLongitude, stateProvince) %>%
  filter(stateProvince == "Victoria") %>%
  filter(basisOfRecord %in% c("OBSERVATION", "HUMAN_OBSERVATION"))

# turn it into an sf object
sfw_sf <- sfw_vic %>%
  st_as_sf(., coords = c("decimalLongitude", "decimalLatitude"), remove = FALSE) %>%
  st_set_crs(., 4326)      


# eyeball locations-----------
# shapefile of Vic boundary
vic <- ne_states(country = 'australia', returnclass = 'sf') %>%
  filter(name == 'Victoria')

ggplot() +
  geom_sf(data = vic) +
  geom_sf(data = sfw_sf) +
  coord_sf()


# tidy up suspect records outside of Vic---------
# identify records outside Vic
sfw_intersection <- sfw_sf %>%
  mutate(within = as.integer(st_intersects(sfw_sf, vic)))

# confirm all suspect records removed
sfw_tidy <- sfw_intersection %>%
  filter(!is.na(within))

ggplot() +
  geom_sf(data = vic) +
  geom_sf(data = sfw_tidy) +
  coord_sf()


# hex plot!----------
p <- ggplot() +
  geom_hex(data = sfw_tidy,
           aes(x = decimalLongitude, y = decimalLatitude),
           bins = 70) +
  annotate("text", x = 147, y = -34.5, 
           label = "Where are Superb Fairy-wrens seen in Victoria?",
           colour = "white", size = 7) +
  annotate("text", x = c(144.9, 149.1), y = c(-34.83, -34.83), 
           label = c("Few", "Many"),
           colour = "white", size = 5) +
  scale_fill_gradient(high = "#BBEDF2", low = "#0D1360", trans = "log", 
                      guide = guide_legend(title = NULL, nrow = 1, label.position = "bottom",
                                           keyheight = unit(1, units = "mm"), keywidth = unit(5, units = "mm"))) +
  labs(caption = "Shandiya Balasubramaniam ● Data: Atlas of Living Australia ● DOI: https://doi.org/10.26197/ala.798be699-5e11-4486-9e01-417ddc37972e") +
  theme_void() +
  coord_sf() +
  theme(text = element_text(family = "lato"),
    plot.background = element_rect(fill = "black", colour = NA),
    plot.caption = element_text(colour = "white", hjust = 0.05, size = 12),
    legend.position = c(0.65, 0.8))

ggsave(here("submissions", "04_hexagons", "04_hexagons.png"), 
       p, dpi = 300, type = "cairo",width = 4, height = 3, units = "in")
 

