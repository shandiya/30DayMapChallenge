
# 30DayMapChallenge Day 11: 3D - Galah occurrence data
# Shandiya Balasubramaniam
# 11 November 2021

library(tidyverse)
library(here)
library(rayshader)
library(rnaturalearth)
library(galah)
library(feathers)
library(showtext)

# get the data ----------

# credentials: email in .Renviron file
galah_config(email = Sys.getenv('EMAIL'), atlas = "Australia") 

# available columns
fields <- search_fields()

# filter parameters
taxa_filter <- select_taxa("Eolophus roseicapilla")
filters <- select_filters(basisOfRecord = "HumanObservation",
                          year >= 2018,
                          country = "Australia",
                          profile = "ALA")
columns <- select_columns("recordID", 
                          "decimalLatitude", 
                          "decimalLongitude", 
                          "eventDate")
# check query size
ala_counts(taxa_filter, filters)

# download the data
galah_occ <- ala_occurrences(taxa = taxa_filter, filters = filters, columns = columns)
saveRDS(galah_occ, here("data", "galah_occ"))

# plot ----------

# check points are where they should be
basemap <- ne_states(country = 'australia', returnclass = 'sf')

# ggplot() +
#   geom_sf(data = basemap) +
#   geom_point(data = galah_occ, aes(decimalLongitude, decimalLatitude)) +
#   coord_sf()

# 2d plot
p <- ggplot() +
  geom_sf(
    data = basemap,
    colour = NA,
    fill = get_pal("galah")[4],
    alpha = 0.2) +
  stat_density_2d(
    data = galah_occ,
    aes(decimalLongitude, decimalLatitude, fill = ..level..),
    geom = "polygon") +
  scale_fill_gradient(low = get_pal("galah")[1],
                      high = get_pal("galah")[3]) +
  coord_sf() +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "josefin", colour = get_pal("galah")[5]),
        panel.grid = element_blank(),
        axis.text = element_blank(), 
        axis.title = element_blank())

# 3d plot
plot_gg(p, multicore = TRUE, width = 10, height = 10)
render_snapshot(here("submissions", "11_3d", "11_3d.png"))





