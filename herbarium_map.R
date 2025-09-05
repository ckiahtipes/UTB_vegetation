###Plotting USF Herbarium Region Colors on World Map

#library(sf)
#library(spData)
#library(spDataLarge)

#world_proj = st_transform(world, "+proj=eck4")
##world_cents = st_centroid(world_proj, of_largest_polygon = TRUE)
#par(mar = c(0, 0, 0, 0))
#plot(world_proj["continent"], reset = FALSE, main = "", key.pos = NULL)
##g = st_graticule()
##g = st_transform(g, crs = "+proj=eck4")
##plot(g$geometry, add = TRUE, col = "lightgray")
##cex = sqrt(world$pop) / 10000
##plot(st_geometry(world_cents), add = TRUE, cex = cex, lwd = 2, graticule = TRUE)
#
#world_amcs = world_proj[world_proj$region_un == "Americas", ]
#amcs = st_union(world_amcs)
#
#world_euro = world_proj[world_proj$continent == "Europe", ]
##euro = st_union(world_euro)
#
##plot(euro, add = TRUE, col = "lightblue")
#plot(amcs, add = TRUE, col = "darkgreen")

###Another method

library(terra)
library(geodata)
library(geodata)
library(maps)
library(wesanderson)
library(spData)
library(sf)
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

#Now we set up some map basics

se_states = us_states[us_states$NAME == "Arkansas" |
                        us_states$NAME == "Louisiana" |
                        us_states$NAME == "Mississippi"|
                        us_states$NAME == "Tennessee" |
                        us_states$NAME == "Alabama"|
                        us_states$NAME == "Georgia"|
                        us_states$NAME == "North Carolina"|
                        us_states$NAME == "South Carolina",]

se_proj = st_transform(se_states, "+proj=eck4")

AR_poly = us_states[us_states$NAME == "Arkansas",]
LA_poly = us_states[us_states$NAME == "Louisiana",]
MS_poly = us_states[us_states$NAME == "Mississippi",]
TN_poly = us_states[us_states$NAME == "Tennessee",]
AL_poly = us_states[us_states$NAME == "Alabama",]
GA_poly = us_states[us_states$NAME == "Georgia",]
NC_poly = us_states[us_states$NAME == "North Carolina",]
SC_poly = us_states[us_states$NAME == "South Carolina",]

AR_proj = st_transform(AR_poly, "+proj=eck4")
LA_proj = st_transform(LA_poly, "+proj=eck4")
MS_proj = st_transform(MS_poly, "+proj=eck4")
TN_proj = st_transform(TN_poly, "+proj=eck4")
AL_proj = st_transform(AL_poly, "+proj=eck4")
GA_proj = st_transform(GA_poly, "+proj=eck4")
NC_proj = st_transform(NC_poly, "+proj=eck4")
SC_proj = st_transform(SC_poly, "+proj=eck4")

AR_centr = st_centroid(AR_proj)
LA_centr = st_centroid(LA_proj)
MS_centr = st_centroid(MS_proj)
TN_centr = st_centroid(TN_proj)
AL_centr = st_centroid(AL_proj)
GA_centr = st_centroid(GA_proj)
NC_centr = st_centroid(NC_proj)
SC_centr = st_centroid(SC_proj)


FL_poly = us_states[us_states$NAME == "Florida",]
FL_proj = st_transform(FL_poly, "+proj=eck4")


#Next we modify Louisiana which doesn't quite plot right

LA_geo = st_point(c(-8199975, 3968894))
LA_geom = st_sfc(LA_geo, crs = "+proj=eck4")
LA_centr$geometry = LA_geom

#Check this section, if nothing is useful then cut.

countries <- world(resolution = 5, path = "maps")
cntry_codes <- country_codes()
countries <- merge(countries, cntry_codes, by.x = "GID_0", by.y = "ISO3", all.x = TRUE)
#count_proj <- st_transform(countries, "+proj=eck4")
#plot(countries, "continent", lwd = 0.2, main = "Countries by continent")


continents <- aggregate(countries, by = "continent")
#plot(continents, "continent", lwd = 0.2)

###Terra package testing

newcrs = "+proj=eck4 +datum=WGS84"

continents_proj = terra::project(continents, newcrs)
#plot(continents_proj)

#Cool, this works. Now, we need to create the geographic regions that matter to the USF Herbarium, combine them, then transform.

NAregion = countries[countries$continent == "North America",]
NA_proj = terra::project(NAregion, newcrs)

CAregion = countries[countries$UNREGION1 == "Central America"|countries$UNREGION1 == "South America",]
CA_proj = terra::project(CAregion, newcrs)

CRregion = countries[countries$UNREGION1 == "Caribbean",]
CR_proj = terra::project(CRregion, newcrs)

#USregion = countries[US_]

#Want to modify the alpha on the colors to make them easier on my eyes.

plot_colors = wes_palette("Cavalcanti1", 100, "continuous")

NA_clcode = "gold"
WR_clcode = "lightblue"
CA_clcode = "darkgreen"
CR_clcode = "gray"
SE_clcode = "red"
FL_clcode = "burlywood"

map_clcodes = c(NA_clcode, WR_clcode, CA_clcode, CR_clcode, SE_clcode, FL_clcode)
map_trans = c(30, 30, 30, 30, 30, 50)

map_colors = sapply(map_clcodes, function(x){
  t_col(x, 30)
})

map_colors[6] = t_col(FL_clcode, 10)

#Outline map of countries

countries_proj = terra::project(countries, newcrs)

#Plotting

par(mfrow = c(2,1), mar = c(1, 1, 1, 1) + 0.1)

#NA_crop = crop(NA_proj, c(-15000000, -3800000, 2000000, 8000000))
NA_crop = crop(NA_proj, c(-12000000, -1000000, 1000000, 7000000))
plot(NA_crop, axes = FALSE, ann = FALSE, ylim = c(1100000, 5500000), xlim = c(-12000000, -3000000))
plot(NA_proj, col = map_colors[1], add = TRUE)
plot(CA_proj, col = map_colors[3], add = TRUE)
plot(CR_proj, col = map_colors[4], add = TRUE)
plot(se_proj, col = map_colors[5], add = TRUE)
plot(FL_proj, col = map_colors[6], add = TRUE)

text(AR_centr, "AR", cex = 0.5)
text(LA_centr, "LA", cex = 0.5)
text(MS_centr, "MS", cex = 0.5)
text(TN_centr, "TN", cex = 0.5)
text(AL_centr, "AL", cex = 0.5)
text(GA_centr, "GA", cex = 0.5)
text(NC_centr, "NC", cex = 0.5)
text(SC_centr, "SC", cex = 0.5)

legend(-6500000, 4800000, 
       c("Florida", "Southeast US", "US/Canada", "Caribbean","Americas", "World"), 
       fill = c(map_colors[6], map_colors[5], map_colors[1], map_colors[4], map_colors[3], map_colors[2]),
       cex = 0.75,
       title = "Folder Color",
       bty = "n")

plot(continents_proj, col = map_colors[2], axes = FALSE, ann = FALSE)
plot(countries_proj, add = TRUE)
plot(NA_proj, col = map_colors[1], add = TRUE)
plot(CA_proj, col = map_colors[3], add = TRUE)
plot(CR_proj, col = map_colors[4], add = TRUE)
plot(se_proj, col = map_colors[5], add = TRUE)
plot(FL_proj, col = map_colors[6], add = TRUE)


par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)


