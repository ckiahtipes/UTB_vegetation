#Notes for geocomputation tutorial.

library(sf)
library(terra)
library(spData)
library(spDataLarge)
library(oce)

#Building sf object

lnd_point = st_point(c(0.1, 51.5))
lnd_geom = st_sfc(lnd_point, crs = "EPSG:4326")
lnd_attrib = data.frame(
  name = "London",
  temperature = 25,
  date = as.Date("2023-06-21")
)
lnd_sf = st_sf(lnd_attrib, geometry = lnd_geom)

#Can use it to build out points with geometries, right?

spec_matrix = matrix(c(easting = specimens_UTM$easting, northing = specimens_UTM$northing), nrow = length(specimens_UTM$easting), ncol = 2)

UTB_points = st_multipoint(spec_matrix)

UTB_geom = st_sfc(UTB_points, crs = "EPSG:3747")

UTB_attrib = data.frame(
  name = sapply(c(1:nrow(spec_matrix)),function(x){
    paste0("specimen ", x)
  }),
  date = specimens$CollDate_Text,
  family = specimens$family,
  genus = specimens$genus
)

UTB_sf = st_sf(UTB_attrib, geometry = UTB_geom)

#Rasters with terra

landcover_path = "~/Dropbox/Temporary/UTB_Plant-Inventory/UTB_code/mapping/Annual_NLCD_H25V17_LndCov_2024_CU_C1V1.tif" 

landcover_rast = rast(landcover_path)

landcover_UTM = project(landcover_rast, "EPSG:3747", method = "near")

#Need this to crop

lat_min = 27.95
lat_max = 28.03

lon_min = -82.675
lon_max = -82.55

map_SW = lonlat2utm(lon_min, lat_min)
map_NE = lonlat2utm(lon_max, lat_max)

UTB_extent = c(c(map_SW$easting,map_NE$easting),c(map_SW$northing, map_NE$northing))

#Cropping from defined extent

landcover_crop = crop(landcover_UTM, UTB_extent)

plot(landcover_crop)
points(specimens_UTM$easting, specimens_UTM$northing, pch = 21, bg = "gold")











