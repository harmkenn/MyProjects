library(sp)
library(rgdal)
library(mgrs)
library(tidyverse)


nzones <- c("C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","W","X")
nnzone <- c(3:22)
ntlzones <- data.frame(nnzone,nzones)

esqls <- as.matrix(rbind(c("A","B","C","D","E","F","G","H"),
                         c("J","K","L","M","N","P","Q","R"),
                         c("S","T","U","V","W","X","Y","Z")))
colnames(esqls) <- 1:8
rownames(esqls) <- c(1,2,0)

nsqls <- c("B","C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","A","B","C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","A","B","C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","A","B","C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","A","B","C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V")

#Function
LL2UTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

# Example
x<--64
y<-29
zone<-floor(1/6*x)+31
xy <- data.frame(ID = 1:length(x), X = x, Y = y)
coordinates(xy) <- c("X", "Y")
proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
res
LL2UTM(-64,29,20)


latlng_to_mgrs<-mgrs::latlng_to_mgrs
mgrs::latlng_to_mgrs(17,44)

mgrs::mgrs_to_latlng("11SNV3000010000")

# my LL to UTM to MGRS fUNCTION

#Function
LL2UTM2mgrs<-function(lng,lat){
  LL <- data.frame(LNG = lng, LAT = lat)
  zone <- floor(1/6*lng)+31
  nz <- data.frame("nnzone" = floor(1/8*lat)+13)
  lz <- left_join(nz,ntlzones)
  coordinates(LL) <- c("LNG", "LAT")
  proj4string(LL) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  UTM <- spTransform(LL, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  esq <- plyr::round_any(UTM@coords[1],100000,floor)/100000
  esql <- esqls[2,esq]
  east <- plyr::round_any(UTM@coords[1],1) - plyr::round_any(UTM@coords[1],100000,floor)
  nsq <-  plyr::round_any(UTM@coords[2],100000,floor)/100000
  nsql <- nsqls[nsq]
  north <- plyr::round_any(UTM@coords[2],1) - plyr::round_any(UTM@coords[2],100000,floor)
  final <- as.data.frame(c(LL,UTM, paste(zone,lz[1,2],esql,nsql,east,north, sep = "")))
  colnames(final) <- c("Long","Lat","Easting","Northing","MGRS")
  return(final)
}

LL2UTM2mgrs(-120,50)


lng <- -120
lat <- 50
LL <- data.frame(LNG = lng, LAT = lat)
zone <- floor(1/6*lng)+31
nz <- data.frame("nnzone" = floor(1/8*lat)+13)
lz <- left_join(nz,ntlzones)
coordinates(LL) <- c("LNG", "LAT")
proj4string(LL) <- CRS("+proj=longlat +datum=WGS84")  ## for example
UTM <- spTransform(LL, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
esq <- plyr::round_any(UTM@coords[1],100000,floor)/100000
esql <- esqls[2,esq]
east <- plyr::round_any(UTM@coords[1],1) - plyr::round_any(UTM@coords[1],100000,floor)
nsq <-  plyr::round_any(UTM@coords[2],100000,floor)/100000
nsql <- nsqls[nsq]
north <- plyr::round_any(UTM@coords[2],1) - plyr::round_any(UTM@coords[2],100000,floor)
final <- as.data.frame(c(LL,UTM, paste(zone,lz[1,2],esql,nsql,east,north, sep = "")))
colnames(final) <- c("Long","Lat","Easting","Northing","MGRS")
final


