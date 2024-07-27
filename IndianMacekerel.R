# Install necessary packages
install.packages("ridigbio")
install.packages("sp")
install.packages("raster")

# Load libraries
library(ridigbio)
library(sp)
library(sf)
library(ggplot2)
library(raster)

# Retrieve data for Rastrelliger kanagurta
mackerel.idigbio <- idig_search_records(list("scientificname" = "Rastrelliger kanagurta"))
View(mackerel.idigbio)
class(mackerel.idigbio)
mackerel.idigbio$geopoint.lat

# Clean data for Rastrelliger kanagurta
mackerel.lat <- mackerel.idigbio[!is.na(mackerel.idigbio$geopoint.lat)==TRUE,]
mackerel.has.year <- mackerel.lat[!is.na(mackerel.lat$`data.dwc:year`)==TRUE,]
mackerel.1970.2024 <- mackerel.has.year[mackerel.has.year$`data.dwc:year`>=1970,]
View(mackerel.1970.2024)

# Mapping and shapefile creation for Rastrelliger kanagurta
mackerel.map <- mackerel.1970.2024
coordinates(mackerel.map)=~geopoint.lon+geopoint.lat
proj4string(mackerel.map) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
mackerel.shape <- spTransform(mackerel.map,CRS("+proj=longlat"))
raster::shapefile(mackerel.shape,"MackerelShapeFile.shp",overwrite=TRUE)
mackerel.shp <- read_sf("MackerelShapeFile.shp")
coastline <- read_sf("us_medium_shoreline.shp")

# Set working directory for shoreline data
setwd("C:/Users/hp/OneDrive/Documents/us_medium_shoreline (1)-20240716T194158Z-001/us_medium_shoreline (1)")

# Plot map for Rastrelliger kanagurta
ggplot() + 
  geom_sf(data=coastline[,1],col="#999999") +
  geom_sf(aes(col = as.numeric(data.dwc.y)),data=mackerel.shp) + scale_y_continuous(position='right')+
  coord_sf(xlim = c(68,92), ylim = c(5,25)) + 
  ggtitle("Indian Mackerel Map")

# Regression analysis for Rastrelliger kanagurta
plot(as.numeric(mackerel.1970.2024$`data.dwc:year`),mackerel.1970.2024$geopoint.lat,xlab="Year",ylab="Latitude",col="orange")
mackerel.reg <- lm(mackerel.1970.2024$geopoint.lat~as.numeric(mackerel.1970.2024$`data.dwc:year`))
abline(mackerel.reg)
summary(mackerel.reg)

# Read sediment data
sediment <- read.csv(file="US9_EXT.csv")
View(sediment)

# Combine sediment data with Mackerel data
mackerel.all <- data.frame()
for(i in 1:length(mackerel.1970.2024$uuid)){
  lat <- round(mackerel.1970.2024$geopoint.lat[i],digits=2)
  lon <- round(mackerel.1970.2024$geopoint.lon[i],digits=2)
  sed.sub.lat <- sediment[round(sediment$Latitude,digits=2)==lat,]
  sed.sub.lat.lon <- sed.sub.lat[round(sed.sub.lat$Longitude,digit=2)==lon,]
  sed.sub.final <- sed.sub.lat.lon[!sed.sub.lat.lon$Grainsze == -99, ]
  grain.max <- ifelse(is.na(sed.sub.final$Grainsze[1]), NA,max(sed.sub.final$Grainsze))
  grain.min <- ifelse(is.na(sed.sub.final$Grainsze[1]), NA,min(sed.sub.final$Grainsze))
  mackerel.all[i, 1] <- mackerel.1970.2024$catalognumber[i]
  mackerel.all[i, 2] <- lat
  mackerel.all[i, 3] <- lon
  mackerel.all[i, 4] <- grain.max
  mackerel.all[i, 5] <- grain.min
  mackerel.all[i,6] <- mackerel.1970.2024$`data.dwc:year`[i]
}

View(mackerel.all)
colnames(mackerel.all) <- c("Catalog Number","Latitude","Longitude","Grain.Max","Grain.Min","Year")
mackerel.complete <- mackerel.all[!is.na(mackerel.all$Grain.Max),]

View(mackerel.complete)

# Plot grain size max for Rastrelliger kanagurta
plot(mackerel.complete$Year,mackerel.complete$Grain.Max, col = "orange", xlab = "Year", ylab = "Grain Size Max")
grain.max.reg.mackerel <- lm(mackerel.complete$Grain.Max~as.numeric(mackerel.complete$Year))
abline(grain.max.reg.mackerel)
summary(grain.max.reg.mackerel)
