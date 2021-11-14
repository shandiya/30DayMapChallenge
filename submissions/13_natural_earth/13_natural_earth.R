
library(sf)
library(here)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthhires)
library(showtext)

# get data ------------

playas <- ne_download(
  scale = 10, 
  type = 'playas', 
  category = 'physical', 
  returnclass = "sf")

place_names <- ne_download(
  scale = 10, 
  type = 'populated_places', 
  category = 'cultural', 
  returnclass = "sf")

countries <- ne_download(
  scale = 10, 
  type = 'countries', 
  category = 'cultural', 
  returnclass = "sf")

# some wrangling ------------

rann <- playas %>% 
  filter(name == "Rann of Kutch")

india_pakistan <- countries %>% 
  filter(ADMIN %in% c("India", "Pakistan"))

surround_names <- place_names %>% 
  filter(SOV0NAME %in% c("India", "Pakistan"))
         
# plot -------------

font_add_google("Mukta", "mukta")
font_add_google("Halant", "halant")
showtext_auto()

p <- ggplot() +
  geom_sf(data = india_pakistan,
          colour = "#9F653F",
          fill = "#9F653F") +
  geom_sf(
    data = rann,
    fill = "#D2B486",
    colour = "#D2B486") +
  geom_text(
    data = filter(surround_names, FEATURECLA == "Admin-1 capital"),
    aes(x = LONGITUDE, y = LATITUDE, label = NAME, check_overlap = TRUE),
    family = "mukta",
    fontface = "bold",
    colour = "#FBFBE9",
    size = 12,
    check_overlap = TRUE) +
  geom_text(
    data = filter(surround_names, FEATURECLA == "Populated place"),
    aes(x = LONGITUDE, y = LATITUDE, label = NAME, check_overlap = TRUE),
    family = "mukta",
    fontface = "italic",
    colour = "#FBFBE9",
    size = 10) +
  geom_text(
    aes(x = c(69.5, 71.2),
        y = c(24.4, 23),
        label = c("Great Rann of Kutch", "Little Rann of Kutch")),
    family = "halant",
    color = "#451009",
    size = 9) +
  geom_text(
    aes(x = 67.7,
        y = 20.5,
        label = "Rann of Kutch"),
    family = "halant",
    color = "#FBFBE9",
    size = 30) +
  coord_sf(xlim = c(66, 75), ylim = c(20, 25)) +
  theme_void() +
  labs(caption = "Shandiya Balasubramaniam | Data: Natural Earth") + 
  theme(
    panel.background = element_rect(colour = "#451009", fill = "#451009"),
    plot.background = element_rect(colour = "#451009", fill = "#451009"),
    plot.caption = element_text(family = "mukta", size = 12, colour = "#FBFBE9"))

ggsave(here("submissions", "13_natural_earth", "13_natural_earth.png"), 
       p, dpi = 300, type = "cairo", width = 8, height = 5, units = "in")  
