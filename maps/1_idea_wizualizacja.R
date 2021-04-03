# install the packages that will be required
install.packages("rgdal")
install.packages("sp")

# set working directory - the same where we unpacked the downloaded files
setwd("D:/ekonometria_przestrzenna/!materialy")

# clear the workspace, plots and console
rm(list = ls())
if(!is.null(dev.list())) dev.off()
cat("\014") 

# packages for i.a. working with maps and visualising data on maps
library(rgdal)
library(sp)


# import map 1 - level of poviats, correct projection (source: Centralny Oœrodek Dokumentacji Geodezyjnej i Kartograficznej, 
#                                                       http://www.codgik.gov.pl/index.php/darmowe-dane/prg.html)
mapa1 <- readOGR(".", "powiaty")
#This map is accurate and well-described, but the coordinates are coded in a different way than we need.
#We should recalculate them into degrees of longitude and latitude.
mapa1 <- spTransform(mapa1, "+proj=longlat")
plot(mapa1)

#import map 2 - many levels and countries at a time
#http://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts#nuts13
mapa2 <- readOGR(".", "NUTS_RG_01M_2013")
mapa2 <- spTransform(mapa2, "+proj=longlat")
plot(mapa2)
#select Poland
mapa2@data$NUTS_ID_char <- as.character(mapa2@data$NUTS_ID)
mapa2@data$country <- substr(mapa2@data$NUTS_ID_char, 1, 2) 
mapa2 <- mapa2[mapa2@data$country == "PL", ]
plot(mapa2)
#wider borders of voivodships
mapa2_NUTS2 <- mapa2[mapa2@data$STAT_LEVL_ == 2, ]
mapa2_NUTS3 <- mapa2[mapa2@data$STAT_LEVL_ == 3, ]
plot(mapa2_NUTS3, lwd = 0.3, border = rgb(0.7, 0.7, 0.7))
plot(mapa2_NUTS2, lwd = 2, add = TRUE)

#import map 3 - format R (rds; source: gadm.org)
mapa3 <- readRDS("POL_adm2.rds")
plot(mapa3)
#Vide gadm.org - "known problems"...

#Ultimately we use the map of poviats provided by CODGiK:
mapa <- mapa1
rm(mapa1, mapa2, mapa3, mapa2_NUTS2, mapa2_NUTS3)

#Import other data
dane <- read.csv("BDL_dane.csv", header = TRUE, sep = ";", dec = ",")
mapa@data$kod <- as.numeric(as.character(mapa@data$jpt_kod_je))

#Put the spatial and economic databases together, remove the partial databases
spatial_data <- merge(y = dane, x = mapa, by.y = "kod", by.x = "kod")
rm(mapa)
rm(dane)

#illustrate variable
green_area <- rgb(24, 121, 104, 80, names = NULL, maxColorValue = 255)
pal <- colorRampPalette(c("white", green_area), bias = 1)
spplot(spatial_data, zcol = "bezrobocie", colorkey = TRUE, col.regions = pal(100), cuts = 99,
       par.settings = list(axis.line = list(col =  'transparent')),
       main = "Unemployment")
