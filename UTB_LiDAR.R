###UTB LiDAR DEM

#Libraries

library(geodata)
library(sf)
library(terra)
library(spData)
library(spDataLarge)
library(oce)

#Load and make raster

landcover_path = "~/Dropbox/Temporary/UTB_Plant-Inventory/UTB_code/maps/UTB_LiDAR-DEM_2019_ngs_tampabay_topobath_dem_J1287063/UTB_LiDAR-DEM_2019_ngs_tampabay_topobath_dem_J1287063tR0_C0.tif" 

landcover_rast = rast(landcover_path)
