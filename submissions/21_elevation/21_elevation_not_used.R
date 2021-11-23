
library(here)
library(raster)
library(RColorBrewer)

# see here for a good tutorial 
# https://rallydatajunkie.com/visualising-rally-stages/determining-terrain-features-from-raster-data.html

# data
elev <- raster(here("data", "elev.tif"))

# plot with and without contours
par(mfrow = c(2, 2), mar = c(0.1, 0.1, 0.1, 0.1))
my.palette <- brewer.pal(n = 9, name = "PuRd")

# elev
plot(elev, col = my.palette, bty = "n", box = FALSE, axes = F, frame.plot = F, legend = F)

# contour
plot(elev, col = my.palette, bty = "n", box = FALSE, axes = F, frame.plot = F, legend = F)
plot(rasterToContour(elev), col = "purple4", lwd = 0.2, add = TRUE)

# terrain roughness index
plot(terrain(elev, "TRI"), col = my.palette, bty="n", box=FALSE, axes=F, frame.plot=F, legend=F)

# hillshade
plot(hillShade(slope = terrain(elev, "slope"), aspect = terrain(elev, "aspect"), angle = 315),
     col = my.palette, bty="n", box=FALSE, axes=F, frame.plot=F, legend=F)

# save plot manually since png() doesn't seem to be working at the moment :\



