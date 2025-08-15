###Sandbox for idea development

#Libraries

library(raster)
library(tmap)
library(maps)
library(oce)

#First up, I want to map out where specimens have already been collected.

specimens = read.csv("UTB_specimens_2010_2022.csv", header = TRUE)

landcover_path = "~/Dropbox/Temporary/UTB_Plant-Inventory/UTB_code/mapping/Annual_NLCD_H25V17_LndCov_2024_CU_C1V1.tif" 
landcover = raster(landcover_path)

lat_min = 27.90
lat_max = 28.05

lon_min = -82.7
lon_max = -82.55

map_SW = lonlat2utm(lon_min, lat_min)
map_NE = lonlat2utm(lon_max, lat_max)

UTB_extent = c(c(map_SW$easting,map_NE$easting),c(map_SW$northing, map_NE$northing))

UTB_cover = crop(landcover, UTB_extent)

specimens_UTM = lonlat2utm(specimens$LongDecL, specimens$LatDecL)

plot(0, 0, xlim = c(map_SW$easting,map_NE$easting), ylim=c(map_SW$northing,map_NE$northing), pch = NA)
map("county", add = TRUE)
plot(UTB_cover)




