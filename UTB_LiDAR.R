###UTB LiDAR DEM

#Libraries

library(geodata)
library(sf)
library(terra)
library(spData)
#library(spDataLarge)
library(oce)
library(tmap)
library(terra)

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

UTB_LiDAR_UTM = project(UTB_LiDAR_rast, "EPSG:32617", method = "near")
WDI_LiDAR_UTM = project(WDI_LiDAR_rast, "EPSG:32617", method = "near")

UTB_landcover_mod = project(UTB_landcover_rast, "EPSG:32617", method = "near")
WDI_landcover_mod = project(WDI_landcover_rast, "EPSG:32617", method = "near")

#Read sampling points and results

PFAS_sites = read.csv("PFAS_sites.csv", header = TRUE, row.names = "SITE")
PFAS_reads = read.csv("PFAS_ALL.csv", header = TRUE)
UTB_sites = PFAS_sites[grep("UPS", row.names(PFAS_sites)),]
WDI_sites = PFAS_sites[grep("WIP", row.names(PFAS_sites)),]

UTB_points = vect(as.matrix(UTB_sites), crs = "EPSG:32617")
WDI_points = vect(as.matrix(WDI_sites), crs = "EPSG:32617")

plot(UTB_landcover_mod, main = "Upper Tampa Bay Conservation Park", xlim = c(338000, 341000), ylim = c(3098000, 3101000))
plot(UTB_LiDAR_UTM, breaks=25, col=gray.colors(25), reset=FALSE, alpha = 0.50, add = TRUE, legend = FALSE)
contour(UTB_LiDAR_UTM, add = TRUE, nlevels = 20)
points(UTB_points, pch = 21, bg = "gold", cex = 1.25)


plot(WDI_landcover_mod, main = "Weedon Island Park", xlim = c(341000, 343000), ylim = c(3081000, 3083000))
plot(WDI_LiDAR_UTM, breaks = 25, col=gray.colors(25), reset = FALSE, alpha = 0.5, add = TRUE, legend = FALSE)
contour(WDI_LiDAR_UTM, add = TRUE, nlevels = 20)
points(WDI_points, pch = 21, bg = "gold", cex = 1.25)

#Extract elevations and landcover types from points

UTB_elev = extract(UTB_LiDAR_UTM, UTB_points)
UTB_lndc = extract(UTB_landcover_rast, UTB_points)

WDI_elev = extract(WDI_LiDAR_UTM, WDI_points)
WDI_lndc = extract(WDI_landcover_rast, WDI_points)

#Pull PFAS results and combine into one table for each location.

UTB_root = PFAS_reads$Fixed[PFAS_reads$AREA == "UTB" & PFAS_reads$PART == "Root"]
UTB_shoot = PFAS_reads$Fixed[PFAS_reads$AREA == "UTB" & PFAS_reads$PART == "Shoot"]

WDI_root = PFAS_reads$Fixed[PFAS_reads$AREA == "WIP" & PFAS_reads$PART == "Root"]
WDI_shoot = PFAS_reads$Fixed[PFAS_reads$AREA == "WIP" & PFAS_reads$PART == "Shoot"]

UTB_PFAS = data.frame(UTB_elev, UTB_lndc, UTB_root, UTB_shoot, row.names = row.names(UTB_sites))
WDI_PFAS = data.frame(WDI_elev, WDI_lndc, WDI_root, WDI_shoot, row.names = row.names(WDI_sites))

#Some comparisons

par(mfrow = c(2,1))

plot(UTB_PFAS$UTB_LiDAR.DEM_2019_ngs_tampabay_topobath_dem_J1287063tR0_C0, UTB_PFAS$UTB_root,
     xlim = c(1,6),
     pch = 21, 
     bg = "gold",
     main = "PFAS Readings",
     ylab = "PFOSK (ng/g)",
     xlab = "Elevation m")

points(UTB_PFAS$UTB_LiDAR.DEM_2019_ngs_tampabay_topobath_dem_J1287063tR0_C0, UTB_PFAS$UTB_shoot, pch = 21, bg = "forestgreen")

points(WDI_PFAS$mean, WDI_PFAS$WDI_root, pch = 22, bg = "gold")
points(WDI_PFAS$mean, WDI_PFAS$WDI_shoot, pch = 22, bg = "forestgreen")

legend('topright', c("UTB roots", "UTB shoots", "WDI roots", "WDI shoots"), pch = c(21, 21, 22, 22), pt.bg = c("gold", "forestgreen","gold","forestgreen"))

#Log plotting

plot(UTB_PFAS$UTB_LiDAR.DEM_2019_ngs_tampabay_topobath_dem_J1287063tR0_C0, log(UTB_PFAS$UTB_root), 
     xlim = c(1,6),
     pch = 21, 
     bg = "gold",
     main = "Log-Transformed PFAS Readings",
     ylab = "PFOSK (ng/g)",
     xlab = "Elevation m")

points(UTB_PFAS$UTB_LiDAR.DEM_2019_ngs_tampabay_topobath_dem_J1287063tR0_C0, log(UTB_PFAS$UTB_shoot), pch = 21, bg = "forestgreen")

points(WDI_PFAS$mean, log(WDI_PFAS$WDI_root), pch = 22, bg = "gold")
points(WDI_PFAS$mean, log(WDI_PFAS$WDI_shoot), pch = 22, bg = "forestgreen")
legend('topright', c("UTB roots", "UTB shoots", "WDI roots", "WDI shoots"), pch = c(21, 21, 22, 22), pt.bg = c("gold", "forestgreen","gold","forestgreen"))


par(mfrow = c(1,1))

#Okay let's combine approaches and look for patterns in the PFAS readings in space

#Turn PFAS readings into colors

par(mfrow = c(1,2))

plot(UTB_landcover_mod, main = "UTB Root Samples", xlim = c(338000, 341000), ylim = c(3098000, 3101000))
plot(UTB_LiDAR_UTM, breaks=25, col=gray.colors(25), reset=FALSE, alpha = 0.50, add = TRUE, legend = FALSE)
contour(UTB_LiDAR_UTM, add = TRUE, nlevels = 20)
palette(colorRampPalette(c("darkorange","purple"))(100))
points(UTB_points, pch = 21, bg = UTB_PFAS$UTB_root*100, cex = 1.25)

plot(UTB_landcover_mod, main = "UTB Shoot Samples", xlim = c(338000, 341000), ylim = c(3098000, 3101000))
plot(UTB_LiDAR_UTM, breaks=25, col=gray.colors(25), reset=FALSE, alpha = 0.50, add = TRUE, legend = FALSE)
contour(UTB_LiDAR_UTM, add = TRUE, nlevels = 20)
palette(colorRampPalette(c("darkorange","purple"))(100))
points(UTB_points, pch = 21, bg = UTB_PFAS$UTB_shoot*100, cex = 1.25)

par(mfrow = c(1,1))

par(mfrow = c(1,2))

plot(WDI_landcover_mod, main = "WIP Root Samples", xlim = c(341000, 343000), ylim = c(3081000, 3083000))
plot(WDI_LiDAR_UTM, breaks = 25, col=gray.colors(25), reset = FALSE, alpha = 0.5, add = TRUE, legend = FALSE)
contour(WDI_LiDAR_UTM, add = TRUE, nlevels = 20)
palette(colorRampPalette(c("darkorange","purple"))(100))
points(WDI_points, pch = 21, bg = WDI_PFAS$WDI_root*100, cex = 1.25)

plot(WDI_landcover_mod, main = "WIP Shoot Samples", xlim = c(341000, 343000), ylim = c(3081000, 3083000))
plot(WDI_LiDAR_UTM, breaks = 25, col=gray.colors(25), reset = FALSE, alpha = 0.5, add = TRUE, legend = FALSE)
contour(WDI_LiDAR_UTM, add = TRUE, nlevels = 20)
palette(colorRampPalette(c("darkorange","purple"))(100))
points(WDI_points, pch = 21, bg = WDI_PFAS$WDI_shoot*100, cex = 1.25)

par(mfrow = c(1,1))
