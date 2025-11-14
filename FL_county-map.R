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

#Set some logical variables

save_figs = TRUE

#Let's read a shapefile with couty polygons.

counties = read_sf("mapping/cb_2024_us_county_500k/cb_2024_us_county_500k.shp")

logo = readPNG("Herbarium_Logo_Orchid_Square_Black_Trans.png")

#Subset by state

FL = counties$STATE_NAME == "Florida"
FL_counties = counties[FL, ]

#Plot outlines and add Hillsborough county as fill. This is where you can pull from a file or list of some kind to automate.

if(save_figs == TRUE){
  setEPS()
  tiff("SV_specimen-map.tiff", height = 2200, width = 2300, res = 300)
}

plot(0, 0, xlim = c(-88, -78), ylim = c(24,32), axes = FALSE, ann = FALSE)
plot(FL_counties$geometry, add = TRUE)
plot(FL_counties$geometry[FL_counties$NAME == "Palm Beach"], add = TRUE, col = "red")

#Add herbarium logo - needs adjustment.

rasterImage(logo, -81,29,-79,31)

if(save_figs == TRUE){
  dev.off()
}
