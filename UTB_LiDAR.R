###UTB LiDAR DEM

#Libraries

library(geodata)
library(sf)
library(terra)
library(spData)
library(spDataLarge)
library(oce)

#Load and make raster

LiDAR_path = "~/Dropbox/Temporary/UTB_Plant-Inventory/UTB_code/maps/UTB_LiDAR-DEM_2019_ngs_tampabay_topobath_dem_J1287063/UTB_LiDAR-DEM_2019_ngs_tampabay_topobath_dem_J1287063tR0_C0.tif" 
landcover_path = "~/Dropbox/Temporary/UTB_Plant-Inventory/UTB_code/maps/UTB_LandCover2021_2021_TampaFL_HighResLandCoverv2_J1287129/UTB_LandCover2021_2021_TampaFL_HighResLandCoverv2_J1287129tR0_C0.tif"


LiDAR_rast = rast(LiDAR_path)
landcover_rast = rast(landcover_path)

landcover_mod = project(landcover_rast, "EPSG:6443", method = "near")

plot(landcover_mod)
plot(LiDAR_rast, add = TRUE, alpha = 0.75, col = "grayscale")
