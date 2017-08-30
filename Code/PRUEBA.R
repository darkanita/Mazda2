# loading the required packages
library(ggplot2)
library(ggmap)

# creating a sample data.frame with your lat/lon points
lon <- c(-75.5760017134)
lat <- c(6.24858636743)
df <- as.data.frame(cbind(lon,lat))

# getting the map
mapgilbert <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = 50,
                      maptype = "satellite", scale = 2)

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = df, aes(x = lon, y = lat, fill = "red", alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)


par(mar = c(0,0,0,0))
plot(ldnoa)


library(maptools)
library(rgeos)
library(tidyverse)
library(rgdal)
library(ggthemes)

ldnoa <- readOGR(dsn = "D:/UNAL/Modelos Lineales/Mazda2/Solucion/Data/in/Departamentos.shp") %>%
  spTransform(CRS("+proj=longlat +datum=WGS84"))

par(mar = c(0,0,0,0))
plot(ldnoa)

library(raster)
s <- shapefile("D:/UNAL/Modelos Lineales/Mazda2/Solucion/Data/in/country.shp")

mazda2[,"Lat"] <- c(NA)
mazda2[,"Lon"] <- c(NA)
mazda2$Ubicacion <- toupper(mazda2$Ubicacion)

densidadDpto <- data.frame(
                            Ubicacion=NULL,
                            Longitud=NULL,
                            Latitud=NULL,
                            Cantidad=NULL)

Ubicacion <- unique(mazda2$Ubicacion)
divipola
mazda2[,Lon:=NULL]

for (i in 1:length(divipola$nombre_departamento)){
  for(j in 1:length(mazda2$Ubicacion))
    if (mazda2[j,"Ubicacion"] == divipola[i,"nombre_departamento"]){
      mazda2[j,"Longitud"] <- divipola[i,"Longitud"]
      mazda2[j,"Latitud"] <- divipola[i,"Latitud"]
    }
}

install.packages("RgoogleMaps")
library(RgoogleMaps)
set.seed(500)
df <- round(data.frame(
  x = jitter(rep(-75.5760017134,50), amount = .3),
  y = jitter(rep(6.24858636743,50), amount = .3)
), digits = 2)
map <- get_googlemap('colombia', markers = df,
                     path = df, scale = 2)
ggmap(map, extent = 'device')

install.packages("rgdal")
install.packages("sp")
install.packages("raster")
install.packages("rasterVIS")
install.packages("rworldmap")
install.packages("RgoogleMaps")

