
# 30DayMapChallenge Days 6, 7, 8: Red, Green, Blue - trees (green), drinking fountains (blue), and mailboxes (red) in Melbourne
# Shandiya Balasubramaniam
# 6-8 November 2021

library(here)
library(tidyverse)
library(janitor)
library(jsonlite)
library(RSocrata)
library(sf)
library(feathers)
library(showtext)
library(patchwork)


# blue -----------
# data from CoM
# https://data.melbourne.vic.gov.au/City-Council/Street-furniture-including-bollards-bicycle-rails-/8fgn-5q6t

street_furniture <- read_csv(here("data", "street_furniture.csv"))

drink_fount <- street_furniture %>% 
  clean_names() %>% 
  filter(asset_type == "Drinking Fountain") %>% 
  mutate(geometry = str_remove_all(coordinate_location, "[()]")) %>% 
  separate(geometry, into = c("lat", "lon"), sep = ",") %>% 
  mutate(lat = as.numeric(lat), 
         lon = as.numeric(lon))


# green --------
# trees data from day 1

trees_raw <- read_csv(here("data", "Trees__with_species_and_dimensions__Urban_Forest_.csv"))

trees <- trees_raw %>% 
  clean_names() %>% 
  filter(!is.na(family))

green_col <- get_pal("rose_crowned_fruit_dove")[c(6,7)]  
green_pal <- colorRampPalette(green_col)(60)


# red ---------
# mail boxes data from Australia Post API for postcode 3000
# messy, but it works :\

pboxes_list <- jsonlite::fromJSON('{
  "points": [
    {
      "location_code": "RED0015577",
      "partner_code": "AP",
      "type": "R_SPB",
      "address": {
        "address_line_1": "764 Elizabeth Street",
        "suburb": "MELBOURNE",
        "state": "VIC",
        "postcode": "3000",
        "country_name": "Australia",
        "country_code": "AU"
      },
      "geo_location": {
        "lat": -37.800841,
        "lon": 144.957548
      },
      "hours": [],
      "service_codes": []
    },
    {
      "location_code": "RED0015596",
      "partner_code": "AP",
      "type": "R_SPB",
      "address": {
        "address_line_1": "St Patricks Alley",
        "address_line_2": "451 Little Bourke Street",
        "suburb": "MELBOURNE",
        "state": "VIC",
        "postcode": "3000",
        "country_name": "Australia",
        "country_code": "AU"
      },
      "geo_location": {
        "lat": -37.814234,
        "lon": 144.95946
      },
      "hours": [],
      "service_codes": []
    },
    {
      "location_code": "RED0006145",
      "partner_code": "AP",
      "type": "R_SPB",
      "address": {
        "address_line_1": "110-130 Swanston Street",
        "suburb": "MELBOURNE",
        "state": "VIC",
        "postcode": "3000",
        "country_name": "Australia",
        "country_code": "AU"
      },
      "geo_location": {
        "lat": -37.814513,
        "lon": 144.966172
      },
      "hours": [],
      "service_codes": []
    },
    {
      "location_code": "RED0006126",
      "partner_code": "AP",
      "type": "R_SPB",
      "address": {
        "address_line_1": "114 Russell Street",
        "suburb": "MELBOURNE",
        "state": "VIC",
        "postcode": "3000",
        "country_name": "Australia",
        "country_code": "AU"
      },
      "geo_location": {
        "lat": -37.813805,
        "lon": 144.968665
      },
      "hours": [],
      "service_codes": []
    },
    {
      "location_code": "RED0007556",
      "partner_code": "AP",
      "type": "R_SPB",
      "address": {
        "address_line_1": "53-57 Queen Street",
        "suburb": "MELBOURNE",
        "state": "VIC",
        "postcode": "3000",
        "country_name": "Australia",
        "country_code": "AU"
      },
      "geo_location": {
        "lat": -37.81721787,
        "lon": 144.96162524
      },
      "hours": [],
      "service_codes": []
    },
    {
      "location_code": "RED0006456",
      "partner_code": "AP",
      "type": "R_SPB",
      "address": {
        "address_line_1": "Corner Abeckett Street",
        "address_line_2": "353 Queen Street",
        "suburb": "MELBOURNE",
        "state": "VIC",
        "postcode": "3000",
        "country_name": "Australia",
        "country_code": "AU"
      },
      "geo_location": {
        "lat": -37.81016483,
        "lon": 144.95837299
      },
      "hours": [],
      "service_codes": []
    },
    {
      "location_code": "RED0006149",
      "partner_code": "AP",
      "type": "R_SPB",
      "address": {
        "address_line_1": "410 Elizabeth Street",
        "suburb": "MELBOURNE",
        "state": "VIC",
        "postcode": "3000",
        "country_name": "Australia",
        "country_code": "AU"
      },
      "geo_location": {
        "lat": -37.809124,
        "lon": 144.960913
      },
      "hours": [],
      "service_codes": []
    },
    {
      "location_code": "RED0006118",
      "partner_code": "AP",
      "type": "R_SPB",
      "address": {
        "address_line_1": "105 Queen Street",
        "suburb": "MELBOURNE",
        "state": "VIC",
        "postcode": "3000",
        "country_name": "Australia",
        "country_code": "AU"
      },
      "geo_location": {
        "lat": -37.815976,
        "lon": 144.961059
      },
      "hours": [],
      "service_codes": []
    },
    {
      "location_code": "RED0006125",
      "partner_code": "AP",
      "type": "R_SPB",
      "address": {
        "address_line_1": "296 Russell Street",
        "suburb": "MELBOURNE",
        "state": "VIC",
        "postcode": "3000",
        "country_name": "Australia",
        "country_code": "AU"
      },
      "geo_location": {
        "lat": -37.80959,
        "lon": 144.966733
      },
      "hours": [],
      "service_codes": []
    },
    {
      "location_code": "RED0006135",
      "partner_code": "AP",
      "type": "R_SPB",
      "address": {
        "address_line_1": "672 Bourke Street",
        "suburb": "MELBOURNE",
        "state": "VIC",
        "postcode": "3000",
        "country_name": "Australia",
        "country_code": "AU"
      },
      "geo_location": {
        "lat": -37.817096,
        "lon": 144.953249
      },
      "hours": [],
      "service_codes": []
    },
    {
      "location_code": "RED0017834",
      "partner_code": "AP",
      "type": "R_SPB",
      "address": {
        "address_line_1": "687 Elizabeth Street",
        "suburb": "MELBOURNE",
        "state": "VIC",
        "postcode": "3000",
        "country_name": "Australia",
        "country_code": "AU"
      },
      "geo_location": {
        "lat": -37.80205068,
        "lon": 144.95724385
      },
      "work_centre_id": 352188,
      "hours": [],
      "service_codes": []
    }
  ]
}')

pboxes_coords <- pboxes_list$points$geo_location


# background map -------------
# https://data.melbourne.vic.gov.au/Transport/Road-corridors/9mdh-8yau

streets <- read.socrata("https://data.melbourne.vic.gov.au/resource/wzzt-avwf.csv")  
streets_sf <- st_as_sf(streets, wkt = "the_geom", crs = 4326)


# plot --------

font_add_google("Nunito", "nunito")
font_add_google("Jost", "jost")
showtext_auto()

caption <-"Shandiya Balasubramaniam • Data: City of Melbourne, Australia Post"

red_plot <- ggplot() +
  geom_sf(data = streets_sf, fill = "#ededed", colour = NA) +
  geom_point(data = pboxes_coords,
             aes(x = lon, y = lat),
             colour = "#DC1928",
             shape = 15,
             size = 4) +
  coord_sf(xlim = c(144.950, 144.975), ylim = c(-37.808, -37.821)) +
  labs(title = "RED") +
  theme_void() +
  theme(
    plot.title = element_text(
      family = "jost",
      colour = "#555555",
      size = 50,
      hjust = 0.5),
    panel.border = element_rect(
      colour = "#555555", 
      fill = NA, 
      size = 1))

 
green_plot <- ggplot() +
  geom_sf(data = streets_sf, fill = "#ededed", colour = NA) +
  geom_point(data = trees, 
             aes(x = longitude, y = latitude, colour = family),
             alpha = 0.6, 
             shape = 16) +
  scale_colour_manual(values = green_pal) +
  coord_sf(xlim = c(144.950, 144.975), ylim = c(-37.808, -37.821)) +
  labs(title = "GREEN") +
  theme_void() +
  theme(
    plot.title = element_text(
      family = "jost",
      colour = "#555555",
      size = 50,
      hjust = 0.5),
    panel.border = element_rect(
      colour = "#555555", 
      fill = NA, 
      size = 1),
    legend.position = "none")

 
blue_plot <- ggplot() +
  geom_sf(data = streets_sf, fill = "#ededed", colour = NA) +
  geom_point(data = drink_fount, 
             aes(x = lon, y = lat),
             colour = "#3b7da4", 
             shape = 21,
             stroke = 2,
             size = 4) +
  coord_sf(xlim = c(144.950, 144.975), ylim = c(-37.808, -37.821)) +
  labs(title = "BLUE") +
  theme_void() +
  theme(
    plot.title = element_text(
      family = "jost",
      colour = "#555555",
      size = 50,
      hjust = 0.5),
    panel.border = element_rect(
      colour = "#555555", 
      fill = NA, 
      size = 1))


three_in_one <- ggplot() +
  geom_sf(data = streets_sf, fill = "#ededed", colour = NA) +
  geom_point(data = trees, 
             aes(x = longitude, y = latitude, colour = family),
             alpha = 0.6, 
             shape = 16) +
  scale_colour_manual(values = green_pal) +
  geom_point(data = drink_fount, 
             aes(x = lon, y = lat),
             colour = "#3b7da4", 
             shape = 21,
             stroke = 2,
             size = 4) +
  geom_point(data = pboxes_coords,
             aes(x = lon, y = lat),
             colour = "#DC1928",
             shape = 15,
             size = 4) +
  coord_sf(xlim = c(144.950, 144.975), ylim = c(-37.808, -37.821)) +
  labs(caption = caption) +
  theme_void() +
  theme(legend.position = "none",
        plot.caption = element_text(
          family = "nunito",
          colour = "#555555",
          size = 20,
          hjust = 0.9),
        panel.border = element_rect(
          colour = "#555555", 
          fill = NA, 
          size = 1))


patchwork <- (red_plot + green_plot + blue_plot) / three_in_one
assembled_plot <- patchwork + plot_layout(heights = c(1, 3))

ggsave(here("submissions", "06-08_red-green-blue", "06-08_red-green-blue.png"),
       assembled_plot, dpi = 300, type = "cairo", width = 15, height = 12, units = "in")

