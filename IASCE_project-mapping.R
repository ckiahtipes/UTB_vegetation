#IASCE Project Global Map

#Libraries

library(terra)
library(geodata)
library(maps)
library(wesanderson)
library(spData)
library(sf)
library(oce)
library(png)
data()

#Color function

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}

#Make softer green for map.

mp_color = t_col("darkgreen", 50)

#Pull countries data and change projection


countries <- world(resolution = 5, path = "maps")
cntry_codes <- country_codes()
countries <- merge(countries, cntry_codes, by.x = "GID_0", by.y = "ISO3", all.x = TRUE)

#Project Locations

locations = read.csv("project-locations.csv", header = TRUE)
locationsUTM = lonlat2utm(locations$Long, locations$Lat)

continents <- aggregate(countries, by = "continent")

###Terra package testing

newcrs = "+proj=eck4 +datum=WGS84"

plot(0,0, xlim = c(-100,50), ylim = c(-40,50), pch = NA, axes = FALSE, ann = FALSE)
plot(countries, col = mp_color, add = TRUE)
points(locations$Long, locations$Lat, pch = 21, bg = "gold", cex = 1.5)

