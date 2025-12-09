###UTB LiDAR DEM

#Libraries

library(geodata)
library(sf)
library(terra)
library(spData)
library(spDataLarge)
library(oce)
library(tmap)

#Load and make raster

UTB_LiDAR_path = "~/Dropbox/Temporary/UTB_Plant-Inventory/UTB_code/maps/UTB_LiDAR-DEM_2019_ngs_tampabay_topobath_dem_J1287063/UTB_LiDAR-DEM_2019_ngs_tampabay_topobath_dem_J1287063tR0_C0.tif" 
UTB_landcover_path = "~/Dropbox/Temporary/UTB_Plant-Inventory/UTB_code/maps/UTB_LandCover2021_2021_TampaFL_HighResLandCoverv2_J1287129/UTB_LandCover2021_2021_TampaFL_HighResLandCoverv2_J1287129tR0_C0.tif"

WDI_LiDAR_path0 = "~/Dropbox/Temporary/UTB_Plant-Inventory/UTB_code/maps/LiDAR_DEM_2019_ngs_topobathy_tampaBay_J1292430/LiDAR_DEM_2019_ngs_topobathy_tampaBay_J1292430tR0_C0.tif"
WDI_LiDAR_path1 = "~/Dropbox/Temporary/UTB_Plant-Inventory/UTB_code/maps/LiDAR_DEM_2019_ngs_topobathy_tampaBay_J1292430/LiDAR_DEM_2019_ngs_topobathy_tampaBay_J1292430tR0_C1.tif"
WDI_LiDAR_path2 = "~/Dropbox/Temporary/UTB_Plant-Inventory/UTB_code/maps/LiDAR_DEM_2019_ngs_topobathy_tampaBay_J1292430/LiDAR_DEM_2019_ngs_topobathy_tampaBay_J1292430tR1_C0.tif"
WDI_LiDAR_path3 = "~/Dropbox/Temporary/UTB_Plant-Inventory/UTB_code/maps/LiDAR_DEM_2019_ngs_topobathy_tampaBay_J1292430/LiDAR_DEM_2019_ngs_topobathy_tampaBay_J1292430tR1_C1.tif"
WDI_landcover_path = "~/Dropbox/Temporary/UTB_Plant-Inventory/UTB_code/maps/Land_Cover_Weedon_Island_2021_TampaFL_HighResLandCoverv2_J1292431/Land_Cover_Weedon_Island_2021_TampaFL_HighResLandCoverv2_J1292431tR0_C0.tif"

UTB_LiDAR_rast = rast(UTB_LiDAR_path)
UTB_landcover_rast = rast(UTB_landcover_path)

WDI_LiDAR_rast0 = rast(WDI_LiDAR_path0)
WDI_LiDAR_rast1 = rast(WDI_LiDAR_path1)
WDI_LiDAR_rast2 = rast(WDI_LiDAR_path2)
WDI_LiDAR_rast3 = rast(WDI_LiDAR_path3)

WDI_LiDAR_group = sprc(WDI_LiDAR_rast0, WDI_LiDAR_rast1, WDI_LiDAR_rast2, WDI_LiDAR_rast3)
WDI_LiDAR_rast = mosaic(WDI_LiDAR_group)
WDI_landcover_rast = rast(WDI_landcover_path)

UTB_landcover_mod = project(UTB_landcover_rast, "EPSG:6443", method = "near")
WDI_landcover_mod = project(WDI_landcover_rast, "EPSG:6443", method = "near")

plot(UTB_landcover_mod)
plot(UTB_LiDAR_rast, breaks=25, col=gray.colors(25), reset=FALSE, alpha = 0.50, add = TRUE, legend = FALSE)
contour(UTB_LiDAR_rast, add = TRUE, nlevels = 20)

plot(WDI_landcover_mod)
plot(WDI_LiDAR_rast, breaks = 25, col=gray.colors(25), reset = FALSE, alpha = 0.5, add = TRUE, legend = FALSE)
contour(WDI_LiDAR_rast, add = TRUE, nlevels = 20)
