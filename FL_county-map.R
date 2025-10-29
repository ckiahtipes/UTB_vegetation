#Making a FL map

#Libraries

library(terra)
library(geodata)
library(maps)
library(wesanderson)
library(spData)
library(sf)
library(png)
data("us_states")

#Let's read a shapefile with couty polygons.

counties = read_sf("mapping/cb_2024_us_county_500k/cb_2024_us_county_500k.shp")

logo = readPNG("Herbarium_Logo_Orchid_Square_Black_Trans.png")

#Subset by state

FL = counties$STATE_NAME == "Florida"
FL_counties = counties[FL, ]

#Plot outlines and add Hillsborough county as fill. This is where you can pull from a file or list of some kind to automate.

plot(FL_counties$geometry)
plot(FL_counties$geometry[FL_counties$NAME == "Hillsborough"], add = TRUE, col = "red")

#Add herbarium logo - needs adjustment.

rasterImage(logo, -87,25,-85,27)
