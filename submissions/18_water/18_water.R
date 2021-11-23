
library(here)
library(sf)
library(ggplot2)
library(showtext)

# data ----------
lbg <- st_read(here(
  "data",
  "bathy",
  "2015_Bathymetry_Lake_Burley_Griffin_Contours.shp"
))

# plot -----------

title <- "LAKE BURLEY GRIFFIN"
subtitle <- "-0.25m Contour Lines-"
caption <- "Shandiya Balasubramaniam | Data: data.gov.au"
font_add_google("Raleway", "raleway")
font_add_google("Josefin Sans", "josefin")
showtext_auto()

p <- ggplot() +
  geom_sf(
    data = lbg, 
    colour = "dodgerblue4",
    alpha = 0.4) +
  coord_sf() +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption
    ) +
  theme_void() +
  theme(
    plot.title = element_text(
      family = "josefin", 
      colour = "#454545",
      hjust = 0.5,
      size = rel(3)
    ),
    plot.subtitle = element_text(
      family = "josefin", 
      colour = "#454545",
      hjust = 0.5,
      size = rel(2)
    ), 
    plot.caption = element_text(
      family = "josefin", 
      colour = "#777777",
      hjust = 0.5,
      size = rel(1)
    ),
    plot.background = element_rect(
      colour = "#efefef", 
      fill = "#efefef"
    )
  )

ggsave(here("submissions", "18_water", "18_water.png"), 
       p, dpi = 300, type = "cairo", width = 9, height = 5, units = "in")

  
  
  
  
  
  
