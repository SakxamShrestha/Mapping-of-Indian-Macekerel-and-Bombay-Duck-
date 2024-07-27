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

# Retrieve data for Harpadon nehereus
bombayduck.idigbio <- idig_search_records(list("scientificname" = "Harpadon nehereus"))
View(bombayduck.idigbio)
class(bombayduck.idigbio)
bombayduck.idigbio$geopoint.lat

# Clean data for Harpadon nehereus
bombayduck.lat <- bombayduck.idigbio[!is.na(bombayduck.idigbio$geopoint.lat)==TRUE,]
bombayduck.has.year <- bombayduck.lat[!is.na(bombayduck.lat$`data.dwc:year`)==TRUE,]
bombayduck.1970.2024 <- bombayduck.has.year[bombayduck.has.year$`data.dwc:year`>=1970,]
View(bombayduck.1970.2024)

# Mapping and shapefile creation for Harpadon nehereus
bombayduck.map <- bombayduck.1970.2024
coordinates(bombayduck.map)=~geopoint.lon+geopoint.lat
proj4string(bombayduck.map) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
bombayduck.shape <- spTransform(bombayduck.map,CRS("+proj=longlat"))
raster::shapefile(bombayduck.shape,"BombayDuckShapeFile.shp",overwrite=TRUE)
bombayduck.shp <- read_sf("BombayDuckShapeFile.shp")
coastline <- read_sf("us_medium_shoreline.shp")

# Set working directory for shoreline data
setwd("C:/Users/hp/OneDrive/Documents/us_medium_shoreline (1)-20240716T194158Z-001/us_medium_shoreline (1)")

# Plot map for Harpadon nehereus
ggplot() + 
  geom_sf(data=coastline[,1],col="#999999") +
  geom_sf(aes(col = as.numeric(data.dwc.y)),data=bombayduck.shp) + scale_y_continuous(position='right')+
  coord_sf(xlim = c(68,92), ylim = c(5,25)) + 
  ggtitle("Bombay Duck Map")

# Regression analysis for Harpadon nehereus
plot(as.numeric(bombayduck.1970.2024$`data.dwc:year`),bombayduck.1970.2024$geopoint.lat,xlab="Year",ylab="Latitude",col="orange")
bombayduck.reg <- lm(bombayduck.1970.2024$geopoint.lat~as.numeric(bombayduck.1970.2024$`data.dwc:year`))
abline(bombayduck.reg)
summary(bombayduck.reg)

# Read sediment data
sediment <- read.csv(file="US9_EXT.csv")
View(sediment)

# Combine sediment data with Bombay Duck data
bombayduck.all <- data.frame()
for(i in 1:length(bombayduck.1970.2024$uuid)){
  lat <- round(bombayduck.1970.2024$geopoint.lat[i],digits=2)
  lon <- round(bombayduck.1970.2024$geopoint.lon[i],digits=2)
  sed.sub.lat <- sediment[round(sediment$Latitude,digits=2)==lat,]
  sed.sub.lat.lon <- sed.sub.lat[round(sed.sub.lat$Longitude,digit=2)==lon,]
  sed.sub.final <- sed.sub.lat.lon[!sed.sub.lat.lon$Grainsze == -99, ]
  grain.max <- ifelse(is.na(sed.sub.final$Grainsze[1]), NA,max(sed.sub.final$Grainsze))
  grain.min <- ifelse(is.na(sed.sub.final$Grainsze[1]), NA,min(sed.sub.final$Grainsze))
  bombayduck.all[i, 1] <- bombayduck.1970.2024$catalognumber[i]
  bombayduck.all[i, 2] <- lat
  bombayduck.all[i, 3] <- lon
  bombayduck.all[i, 4] <- grain.max
  bombayduck.all[i, 5] <- grain.min
  bombayduck.all[i,6] <- bombayduck.1970.2024$`data.dwc:year`[i]
}

View(bombayduck.all)
colnames(bombayduck.all) <- c("Catalog Number","Latitude","Longitude","Grain.Max","Grain.Min","Year")
bombayduck.complete <- bombayduck.all[!is.na(bombayduck.all$Grain.Max),]

View(bombayduck.complete)

# Plot grain size max for Harpadon nehereus
plot(bombayduck.complete$Year,bombayduck.complete$Grain.Max, col = "orange", xlab = "Year", ylab = "Grain Size Max")
grain.max.reg.bombayduck <- lm(bombayduck.complete$Grain.Max~as.numeric(bombayduck.complete$Year))
abline(grain.max.reg.bombayduck)
summary(grain.max.reg.bombayduck)
