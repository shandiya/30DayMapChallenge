
# 30DayMapChallenge Day 12: Population - Cartogram of 2021 Popoulation in Asia
# Shandiya Balasubramaniam
# 12 November 2021

library(here)
library(maptools)
library(cartogram)
library(sf)
library(tidyverse)
library(countrycode)
library(extrafont)

# data---------

# spdf from maptools
data(wrld_simpl) 
# world pop data from gapminder
pop <- read_csv(here("data", "population_total.csv"), show_col_types = FALSE)

# wrangling------------

asia <- wrld_simpl[wrld_simpl$REGION == 142, ]

asia_sf <- asia %>% 
  st_as_sf(.,) %>% 
  st_set_crs(., 4326) %>% 
  st_transform(., 3395) 

asia_iso2 <- as.vector(asia$ISO2)

pop_asia_2021 <- pop %>%
  select(country, pop_2021 = '2021') %>% 
  mutate(ISO2 = countrycode(country, origin = 'country.name', destination = "iso2c")) %>% 
  filter(ISO2 %in% asia_iso2) %>% 
  separate(pop_2021, into = c('numbers', 'multipl'), sep = -1, remove = FALSE, convert = TRUE) %>% 
  mutate(actual_pop = case_when(
    multipl == "M" ~ numbers*1000000,
    multipl == "k" ~ numbers*1000,
    multipl == "B" ~ numbers*1000000000,
  )) %>% 
  mutate(log_pop = log(actual_pop))

all_data <- asia_sf %>% 
  left_join(., pop_asia_2021, by = "ISO2") %>% 
  select(ISO2, NAME, country, actual_pop, log_pop)

cart <- cartogram_cont(all_data, weight = "actual_pop")

# plot------------------

ggplot(data = cart) +
  geom_sf(aes(fill = log_pop), colour = "#cc7722", lwd = 0.1) +
  coord_sf() +
  theme_void() +
  labs(
    title = "Asia in the Roaring Twenties",
    subtitle = "Cartogram based on 2021 population data",
    caption = "Data: GAPMINDER.ORG | Shandiya Balasubramaniam"
    ) +
  scale_fill_gradient(
    low = "#fffdd0",  
    high = "#fda50f", 
    n.breaks = 5,
    guide = guide_colorsteps(
      barwidth = 15,
      barheight = 0.5,
      title = "logarithmic scale",
      title.position = "right",
      title.vjust = 0.1)
    ) +
  theme(
    text = element_text(family = "Montserrat", colour = "#222222"),
    legend.position = "top",
    legend.margin = margin(t = 10, b = 2),
    legend.title = element_text(size = 7),
    legend.text = element_text(margin = margin(t = 3))
    )

ggsave(here("submissions", "12_population", "12_population.png"),
       dpi = 300, type = "cairo", width = 4, height = 4, units = "in")    
