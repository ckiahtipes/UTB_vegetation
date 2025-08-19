###Plotting USF Herbarium Region Colors on World Map

library(sf)
library(spData)
library(terra)
library(geodata)

world_proj = st_transform(world, "+proj=eck4")
#world_cents = st_centroid(world_proj, of_largest_polygon = TRUE)
par(mar = c(0, 0, 0, 0))
plot(world_proj["continent"], reset = FALSE, main = "", key.pos = NULL)
#g = st_graticule()
#g = st_transform(g, crs = "+proj=eck4")
#plot(g$geometry, add = TRUE, col = "lightgray")
#cex = sqrt(world$pop) / 10000
#plot(st_geometry(world_cents), add = TRUE, cex = cex, lwd = 2, graticule = TRUE)

world_amcs = world_proj[world_proj$region_un == "Americas", ]
amcs = st_union(world_amcs)

world_euro = world_proj[world_proj$continent == "Europe", ]
euro = st_union(world_euro)

plot(euro, add = TRUE, col = "lightblue")
plot(amcs, add = TRUE, col = "darkgreen")

###Another method

countries <- world(resolution = 5, path = "maps")
cntry_codes <- country_codes()
countries <- merge(countries, cntry_codes, by.x = "GID_0", by.y = "ISO3", all.x = TRUE)
count_proj <- st_transform(countries, "+proj=eck4")
plot(countries, "continent", lwd = 0.2, main = "Countries by continent")


continents <- aggregate(countries, by = "continent")
plot(continents, "continent", lwd = 0.2)