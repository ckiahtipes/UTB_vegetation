### Mapping Carex distributions

#Libraries

library(terra)
library(geodata)
library(maps)
library(wesanderson)
library(spData)
library(sf)
library(png)
data("us_states")

#Functions

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

#Read county list and list of carex species with distributions...

county_list <- read.csv("FL_counties.csv", header = TRUE)

#Read Carex table

carex <- read.csv("carex_dist.csv", header = TRUE)

carex_species = unique(carex$Scientific.Name)

#Let's read a shapefile with couty polygons.

counties = read_sf("mapping/cb_2024_us_county_500k/cb_2024_us_county_500k.shp")

#Subset county polygons by state

FL = counties$STATE_NAME == "Florida"
FL_counties = counties[FL, ]

#Make transparency

mp_color = t_col("darkgreen", 50)

#Can we loop through the list of species and map counties?

par(mfrow = c(4,3))

for(i in 1:length(carex_species)){
  plot(0, 0, xlim = c(-88, -78), ylim = c(24,32), axes = FALSE, ann = FALSE)
  
  list_counties = unique(carex$County.Code[carex$Scientific.Name == carex_species[i]])
  
  plot(FL_counties$geometry, add = TRUE)
  
  for(j in 1:length(list_counties)){
    plot(FL_counties$geometry[FL_counties$NAME == county_list$Name[county_list$Code == list_counties[j]]], add = TRUE, col = mp_color)
  }
  
  title(main = paste0(carex_species[i]))
  
}

par(mfrow = c(1,1))

#Heatmap of species occurences

county_count = vector("numeric", length = length(county_list$Name))

for(i in 1:length(county_count)){
  
  grab = county_list$Code[i]
  
  county_count[i] = length(carex$County.Code[carex$County.Code == grab])
  
}

plot(0, 0, xlim = c(-88, -78), ylim = c(24,32), axes = FALSE, ann = FALSE)

palette(rev(heat.colors(max(county_count), alpha = 0.6)))
plot(FL_counties$geometry, add = TRUE, col = county_count, lty = 0)
palette("default")
plot(FL_counties$geometry, add = TRUE)







