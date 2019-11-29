pacman::p_load(tidyverse, sp, rgdal, mgrs, numbers, numform)

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
LL2UTM2mgrs<-function(lng,lat){
  LL <- data.frame(LNG = lng, LAT = lat)
  zone <- floor(1/6*lng)+31
  nz <- data.frame("nnzone" = floor(1/8*lat)+13)
  lz <- left_join(nz,ntlzones,by = "nnzone")
  coordinates(LL) <- c("LNG", "LAT")
  proj4string(LL) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  UTM <- spTransform(LL, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  esq <- plyr::round_any(UTM@coords[1],100000,floor)/100000
  emod <- mod(zone,3)
  if (emod == 0) {emod <- 3}
  esql <- esqls[emod,esq]
  east <- plyr::round_any(UTM@coords[1],1) - plyr::round_any(UTM@coords[1],100000,floor)
  nsq <-  plyr::round_any(UTM@coords[2],100000,floor)/100000
  if (nsq < 0) {nsq <- 100+nsq}
  if((zone %% 2) == 0) {nsq <- nsq +5}
  nsql <- nsqls[nsq]
  north <- plyr::round_any(UTM@coords[2],1) - plyr::round_any(UTM@coords[2],100000,floor)
  easting <- f_pad_zero(round(UTM@coords[1],0), width = 6, pad.char = "0")
  northing <- round(UTM@coords[2],0)
  if (northing < 0){northing <- 10000000 + northing}
  northing <- f_pad_zero(northing, width = 7, pad.char = "0")
  zone <- f_pad_zero(zone, width = 2, pad.char = "0")
  final <- as.data.frame(c(LL, easting,northing,zone, 
              paste(zone,lz[1,2],esql,nsql,f_pad_zero(east, width = 5, pad.char = "0"),
                    f_pad_zero(north, width = 5, pad.char = "0"), sep = "")))
  colnames(final) <- c("Long","Lat","Easting","Northing","zone","MGRS")
  return(final)
}

## Here is the function that turns

LL2UTM2mgrs(-138.23,-34.55)

longv <- (sample.int(36000,size=20,replace=TRUE)-1)/100-180
latv <- (sample.int(16000,size=20,replace=TRUE)-1)/100-80
samplell <- data.frame(cbind(longv,latv))


samplell$UTME <- NA
samplell$UTMN <- NA
samplell$gz <- NA
samplell$mgrs <- NA

for (i in 1:nrow(samplell)){
  samplell$UTME[i] <- as.matrix(LL2UTM2mgrs(samplell$longv[i],samplell$latv[i]))[3]
  samplell$UTMN[i] <- as.matrix(LL2UTM2mgrs(samplell$longv[i],samplell$latv[i]))[4]
  samplell$gz[i] <- as.matrix(LL2UTM2mgrs(samplell$longv[i],samplell$latv[i]))[5]
  samplell$mgrs[i] <- as.matrix(LL2UTM2mgrs(samplell$longv[i],samplell$latv[i]))[6]
}
