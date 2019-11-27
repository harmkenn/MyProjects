library(sp)
library(rgdal)
library(mgrs)

#Function
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

# Example
x<-c( -94.99729,-94.99726,-94.99457,-94.99458,-94.99729)
y<-c( 29.17112, 29.17107, 29.17273, 29.17278, 29.17112)
LongLatToUTM(x,y,15)


latlng_to_mgrs<-mgrs::latlng_to_mgrs
mgrs::latlng_to_mgrs(17,44)

mgrs::mgrs_to_latlng("11SNV3000010000")
