#Making maps with counties or other natural area boundaries.

#Libraries

library(terra)
library(geodata)
library(maps)
library(wesanderson)
library(spData)
library(sf)
library(png)
library(oce)
data("us_states")

#Set some logical variables

save_figs = FALSE

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

#Plotting conservation lands boundaries for plotting or research.

#Pulling conservation lands from FNAI

cons_lands = st_read("maps/flma_202512/FloridaConservationLands.gdb")
WithlacoocheeSF = cons_lands[2889,] #This is Withlacoochee State Forest

plot(WithlacoocheeSF$Shape) #This just plots the area.

WithlacoocheeSFt = st_transform(WithlacoocheeSF, "EPSG:4326")

#Import USF Herbarium collections with LAT/LON data

WTL_sp <- read.csv("WTL_specimens.csv", header = TRUE, row.names = "barcode")

WTL_sploc = WTL_sp[is.na(as.numeric(WTL_sp$LatDecL))==FALSE,]
WTL_points = data.frame(as.numeric(WTL_sploc$LatDecL),as.numeric(WTL_sploc$LongDecL))

WTL_UTMpt = lonlat2utm(as.numeric(WTL_points$WTL_sploc.LongDecL), as.numeric(WTL_points$WTL_sploc.LatDecL))

#This is complicated by the geodatabase thing which isn't playing nice with R...try this method to convert points.

#Need numeric matrix of locations first...

WTL_spmx = matrix(c(easting = WTL_UTMpt$easting, northing = WTL_UTMpt$northing), nrow = length(WTL_UTMpt$easting), ncol = 2)

WTL_Cpoint = st_multipoint(WTL_spmx)

WTL_geom = st_sfc(WTL_Cpoint, crs = "EPSG:4326")


WTL_attrib = data.frame(
  name = row.names(WTL_sp)[is.na(as.numeric(WTL_sp$LatDecL))==FALSE],
  genus = WTL_sp$genus[is.na(as.numeric(WTL_sp$LatDecL))==FALSE],
  date = WTL_sp$Collection.Date[is.na(as.numeric(WTL_sp$LatDecL))==FALSE]
)

WTL_sf = st_sf(WTL_attrib, geometry = WTL_geom)

#Plotting

plot(0,0, xlim = c(-83.00, -81.5), ylim = c(28,29.5), pch = NA)

plot(counties_LatLon, add = TRUE, lty = 2, col = NA)

plot(WithlacoocheeSFt$Shape, add = TRUE, col = "lightblue")

points(WTL_points$as.numeric.WTL_sploc.LongDecL., WTL_points$as.numeric.WTL_sploc.LatDecL., pch = 21, bg = "gold")

#plot(FL_counties, xlim = c())
#plot(WithlacoocheeSFt$Shape)
#points(WTL_points$as.numeric.WTL_sploc.LongDecL., WTL_points$as.numeric.WTL_sploc.LatDecL., pch = 21, bg = "lightblue")
#plot(counties_LatLon, add = TRUE, lty = 2, col = NA)
