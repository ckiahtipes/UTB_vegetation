#Withlacoochee Data Exploration

WTL_sp <- read.csv("WTL_specimens.csv", header = TRUE, row.names = "barcode")

par(mar = c(5, 6, 4, 2) + 0.1)

barplot(table(WTL_sp$county), horiz = TRUE, las = 1, xlim = c(-50,600))

title(main = "N 'Withlacoochee' Specimens in USF Herbarium by County")

par(mar = c(5, 4, 4, 2) + 0.1)

coll_dates = as.Date(WTL_sp$Collection.Date, format = "%d/%m/%Y")

barplot(table(WTL_sp$Collection.Date), horiz = TRUE, las = 1)

collector = table(WTL_sp$collector_Text)

par(mar = c(5, 8, 4, 2) + 0.1)

barplot(collector[order(collector)], horiz = TRUE, las = 1, main = "N 'Withlacoochee' Specimens in USF Herbarium by Collector", cex.text = 0.8)

par(mar = c(5, 4, 4, 2) + 0.1)