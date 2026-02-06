#Withlacoochee Data Exploration

WTL_sp <- read.csv("WTL_specimens.csv", header = TRUE)

par(mar = c(5, 6, 4, 2) + 0.1)

barplot(table(WTL_sp$county), horiz = TRUE, las = 1, xlim = c(-50,600))

title(main = "N 'Withlacoochee' Specimens in USF Herbarium by County")

par(mar = c(5, 4, 4, 2) + 0.1)

coll_dates = as.Date(WTL_sp$Collection.Date, format = "%d/%m/%Y")

barplot(table(WTL_sp$Collection.Date), horiz = TRUE, las = 1)

collector = table(WTL_sp$collector_Text)

par(mar = c(5, 8, 4, 2) + 0.1)

barplot(collector[order(collector)], horiz = TRUE, las = 1, main = "N 'Withlacoochee' Specimens in USF Herbarium by Collector", cex.text = 0.8)

par(mar = c(5, 8, 4, 2) + 0.1)

barplot(table(WTL_sp$genus), horiz = TRUE, las = 1, main = "N Withlacoochee Specimens by Genera")

par(mar = c(5, 8, 4, 2) + 0.1)

#Okay, time for some more serious aggregation work. Need Family, Genus, Species, Collector, and Collection Date.

WTL_sp <- read.csv("WTL_specimens.csv", header = TRUE) #Reading "Withlacoochee" as Location
WSF_sp = read.csv("WSF_specimens.csv", header = TRUE) #Reading "Withlacoochee State Forest as Location"

comb_sp = rbind(WTL_sp, WSF_sp)

doubles_check = table(comb_sp$barcode)

cut_list = doubles_check[doubles_check > 1]

for(i in 1:length(cut_list)){
  cut = grep(names(cut_list[i]), comb_sp$barcode)
  comb_sp = comb_sp[-c(cut[1], cut[2]),]
  pull = WTL_sp[cut_list[i],]
  comb_sp = rbind(comb_sp, pull)
}

WTL_taxa = data.frame(comb_sp$family, comb_sp$genus, comb_sp$species)

WTL_dates = comb_sp$Collection.Date

coll_dates = as.Date(comb_sp$Collection.Date, format = "%d/%m/%Y")
coll_yr = as.Date(coll_dates, format = "%Y")

#We've extracted collection years, going to go back and backfill some of the shitty data.
#Should probably do this after we've got an official export from the database.

genus = WTL_data$comb_sp.genus
specs = WTL_data$comb_sp.species
epithet = vector("character", length(genus))

for(i in 1:length(epithet)){
  epithet[i] = paste0(genus[i]," ",specs[i])
}




