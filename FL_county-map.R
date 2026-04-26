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
plot(FL_counties$geometry[FL_counties$NAME == "Hernando"], add = TRUE, col = "red")

#Add herbarium logo - needs adjustment.

rasterImage(logo, -81,29,-79,31)

if(save_figs == TRUE){
  dev.off()
}

#Plotting conservation lands boundaries for plotting or research.

#Define window - format: xmin, ymin, xmax, ymax OR Lon_min, Lat_min, Lon_max, Lat_max OR UTMe_win[1], UTMn_win[1], UTMe_win[2], UTMn_win[2]

Lat_win = c(28,29.5)
Lon_win = c(-83.00, -81.5)
UTM_min = lonlat2utm(Lon_win[1], Lat_win[1])
UTM_max = lonlat2utm(Lon_win[2], Lat_win[2])
LL_extent = c(-83.0, 28, -81.5, 29.5)
UTM_extent = c(UTM_min[[1]], UTM_min[[2]], UTM_max[[1]], UTM_max[[2]])
names(UTM_extent) = c("xmin", "ymin", "xmax", "ymax")
names(LL_extent) = c("xmin", "ymin", "xmax", "ymax")

extentUTM <- raster::extent(UTM_min[[1]], UTM_max[[1]], UTM_min[[2]], UTM_max[[2]])
extentLND <- raster::extent(52652.5, 799595.9, 45555.78, 781583)
extentLND_sf <- st_set_crs(st_as_sf(as(extentLND, "SpatialPolygons")), "EPSG:3087")
extentUTM_sf <- st_set_crs(st_as_sf(as(extentUTM, "SpatialPolygons")), "EPSG:3087")
extentLL_sf <- st_set_crs(st_as_sf(as(raster::extent(-83.00, -81.5, 28, 29.5), "SpatialPolygons")), "EPSG:4269")


#Pulling conservation lands from FNAI

cons_lands = st_read("maps/flma_202512/FloridaConservationLands.gdb")
WithlacoocheeSF = cons_lands[2889,] #This is Withlacoochee State Forest

plot(WithlacoocheeSF$Shape) #This just plots the area.

WithlacoocheeSFt = st_transform(WithlacoocheeSF, "EPSG:4269")

#Read landcover from FWC, file is huge and will need to be trimmed. UPDATE: Colleen may have saved the day

WTL_landcover = read_sf("maps/Export_WSF_Current_NC_Pys_2024/Export_WSF_Current_NC_Pys_2024.shp") #This only plots inside the forest area! 

state_landcover = st_read("maps/Polygon/CLC_v4_Poly.gdb")
sf_use_s2(FALSE)
stld_LatLon = st_transform(state_landcover, "EPSG:4269")
LL_crop = st_intersection(stld_LatLon, extentLL_sf)


#hydric_flatwoods = state_landcover[state_landcover$NAME_SITE == "Hydric Pine Flatwoods",]
#stld_crop = st_crop(hydric_flatwoods, UTM_extent)




#Transform counties

#counties_UTMGDL = st_transform(FL_counties: "EPSG:3087")
counties_LatLon = st_transform(FL_counties, "EPSG:4269")

#Import USF Herbarium collections with LAT/LON data

WTL_sp <- read.csv("WTL_specimens.csv", header = TRUE, row.names = "barcode")

WTL_sploc = WTL_sp[is.na(as.numeric(WTL_sp$LatDecL))==FALSE,]
WTL_points = data.frame(as.numeric(WTL_sploc$LatDecL),as.numeric(WTL_sploc$LongDecL))

WTL_UTMpt = lonlat2utm(as.numeric(WTL_points$WTL_sploc.LongDecL), as.numeric(WTL_points$WTL_sploc.LatDecL))

#This is complicated by the geodatabase thing which isn't playing nice with R...try this method to convert points.

#Need numeric matrix of locations first...

WTL_spmx = matrix(c(easting = WTL_UTMpt$easting, northing = WTL_UTMpt$northing), nrow = length(WTL_UTMpt$easting), ncol = 2)

WTL_Cpoint = st_multipoint(WTL_spmx)

WTL_geom = st_sfc(WTL_Cpoint, crs = "EPSG:4269")


WTL_attrib = data.frame(
  name = row.names(WTL_sp)[is.na(as.numeric(WTL_sp$LatDecL))==FALSE],
  genus = WTL_sp$genus[is.na(as.numeric(WTL_sp$LatDecL))==FALSE],
  date = WTL_sp$Collection.Date[is.na(as.numeric(WTL_sp$LatDecL))==FALSE]
)

WTL_sf = st_sf(WTL_attrib, geometry = WTL_geom)

#We need to extract vegetation communities for plotting.

types = unique(LL_crop$NAME_SITE)

#Summarize by area

type_area = sapply(types, function(x){
  n = sum(LL_crop$Shape_Area[LL_crop$NAME_SITE == x])
})

type_area = type_area[rev(order(type_area))]

#Let's make a table of the most common types and map them.

type_table = data.frame(type_area[1:20])

#Wrote a table that I will fill with color and class definitions to ease plotting. Will read it at the top and use it here...

type_table = read.csv("WTL_regional-types.csv", header = TRUE)

#Method for combining things based on site types...


plot(st_union(LL_crop[LL_crop$NAME_SITE == type_table$TYPE[1] |
                      LL_crop$NAME_SITE == type_table$TYPE[2] |
                      LL_crop$NAME_SITE == type_table$TYPE[3] ,]), add = TRUE, col = "darkorange")

impacted = st_combine(LL_crop[LL_crop$NAME_SITE == type_table$TYPE[1] |
                              LL_crop$NAME_SITE == type_table$TYPE[2] |
                              LL_crop$NAME_SITE == type_table$TYPE[3] ,])

impacted = sapply(type_table$TYPE[type_table$CLASS == type_table$CLASS[1]], function(x){
  
})

#Plotting

#plot(0,0, xlim = c(-83.0, -82.0), ylim = c(28.0,29.0), pch = NA) #Big View
plot(0,0, xlim = c(-82.6, -82.4), ylim = c(28.6,28.8), pch = NA) #Close View

plot(counties_LatLon$geometry, add = TRUE, lty = 2, col = NA)

for(i in 1:nrow(type_table)){
  plot(LL_crop$Shape[LL_crop$NAME_SITE == type_table$TYPE[i]], add = TRUE, col = type_table$COLOR[i], lwd = 0.5)
  print(paste0(i, " of ", nrow(type_table), " types plotted"))
}

plot(WithlacoocheeSFt$Shape, add = TRUE, col = "lightblue")

#plot(LL_crop$Shape[LL_crop$NAME_SITE == "Mixed Hardwood-Coniferous"], add = TRUE, col = "darkgreen", lwd = 0.5)
#plot(LL_crop$Shape[LL_crop$NAME_SITE == "Sand Pine Scrub"], add = TRUE, col = "lightgreen", lwd = 0.5)
#plot(LL_crop$Shape[LL_crop$NAME_SITE == "Coniferous Plantations"], add = TRUE, col = "darkorange", lwd = 0.5)
#plot(LL_crop$Shape[LL_crop$NAME_SITE == "Sandhill"], add = TRUE, col = "gold", lwd = 0.5)
#plot(LL_crop$Shape[LL_crop$NAME_SITE == "Residential, Low Density"], add = TRUE, col = "red", lwd = 0.5)
#plot(LL_crop$Shape[LL_crop$NAME_SITE == "Marine"], add = TRUE, col = "turquoise", lwd = 0.5)
#plot(LL_crop$Shape[LL_crop$NAME_SITE == "Lacustrine"], add = TRUE, col = "darkblue", lwd = 0.5)

points(WTL_points$as.numeric.WTL_sploc.LongDecL., WTL_points$as.numeric.WTL_sploc.LatDecL., pch = 21, bg = "gold")

#plot(FL_counties, xlim = c())
#plot(WithlacoocheeSFt$Shape)
#points(WTL_points$as.numeric.WTL_sploc.LongDecL., WTL_points$as.numeric.WTL_sploc.LatDecL., pch = 21, bg = "lightblue")
#plot(counties_LatLon, add = TRUE, lty = 2, col = NA)
