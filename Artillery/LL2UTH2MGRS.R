pacman::p_load(tidyverse, sp, rgdal, mgrs, numbers, numform)

#Function
LL2UTM2mgrs<-function(lng,lat){
  lngr <- lng*pi/180 # Longitude in radians
  latr <- lat*pi/180 # Latitude in radians
  
  # Lets go find Easting
  
  sin1 <- pi/(180*3600) #One Second
  a <- 6378137 # Equitorial Radius
  b <- 6356752.3142 #Polar Radius
  k0 <- .9996 #Scalar Factor Constant
  zone <- floor(1/6*lng)+31 #Longitude Zone
  Czone <- 6*zone - 183 #Longitude of the center of the zone
  dlng <- lng - Czone # Longitude from the center of the zone
  p <- dlng*3600/10000 #Hecta seconds?
  e <- sqrt(1-(b/a)^2) #eccentricity
  e1sq <- e^2/(1-e^2)
  nu <- a/sqrt(1-(e*sin(latr))^2) #r curv 2
  Kiv <- nu*cos(latr)*sin1*k0*10000 #Coef for UTM 4
  Kv <- (sin1*cos(latr))^3*(nu/6)*
    (1-tan(latr)^2+e1sq*cos(latr)^2)*k0*10^12 #Coef for UTM 5
  Easting <- 500000+Kiv*p+Kv*p^3
  
  # Now let's go find Northing
  n <- (a-b)/(a+b)
  A0 <- a*(1-n+(5*n^2/4)*(1-n)+(81*n^4/64)*(1-n)) # Meridional Arc Length
  B0 <- (3*a*n/2)*(1-n-(7*n^2/8)*(1-n)+55*n^4/64) # Meridional Arc Length
  C0 <- (15*a*n^2/16)*(1-n+(3*n^2/4)*(1-n)) # Meridional Arc Length
  D0 <- (35*a*n^3/48)*(1-n+11*n^2/16) # Meridional Arc Length
  E0 <- (315*a*n^4/51)*(1-n) # Meridional Arc Length
  S <- A0*latr - B0*sin(2*latr) + C0*sin(4*latr) - D0*sin(6*latr) + E0*sin(8*latr) # Meridional Arc
  Ki <- S*k0 #Coef for UTM 1
  Kii <- nu*sin(latr)*cos(latr)*sin1^2*k0*100000000/2 #Coef for UTM 2
  Kiii <- ((sin1^4*nu*sin(latr)*cos(latr)^3)/24)*(5-tan(latr)^2+9*e1sq*cos(latr)^2*cos(latr)^4)*k0*10^16 #Coef for UTM 2
  Northing <- Ki + Kii * p^2 + Kiii * p^4 
  if (lat < 0) {Northing <- 10000000 + Northing} # In the Southern Hemisphere is Northing is measured from the south pole instead of from the equator
  
  Easting <- round(Easting,0)
  Northing <- round(Northing,0)

  ## Now let's turn UTM into MGRS
  
  #Latitude gridzone letters
  lgzl <- c("C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","W","X")
  
  # 100km grid Square easting letter
  esqls <- as.matrix(rbind(c("A","B","C","D","E","F","G","H"),
                           c("J","K","L","M","N","P","Q","R"),
                           c("S","T","U","V","W","X","Y","Z")))
  colnames(esqls) <- 1:8
  rownames(esqls) <- c(1,2,0)
  
  nsqls <- c("B","C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","A","B","C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","A","B","C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","A","B","C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","A","B","C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V")
  
  lzc <- (lat+80-(lat+80)%%8)/8+1 # LAtitude letter zone count
  lz <- lgzl[lzc]
  esq <- plyr::round_any(Easting,100000,floor)/100000 #Grab the first digit off of easting
  emod <- mod(zone,3) # Zone mod 3 for 100km lookup
  if (emod == 0) {emod <- 3} #turn mod zero into row 3
  esql <- esqls[emod,esq] #Here is the 100km easting grid letter
  nsq <-  plyr::round_any(Northing,100000,floor)/100000 #Grab off the first two digits off the Northing
  if((zone %% 2) == 0) {nsq <- nsq +5} #Even grid zones increase the letter by 5
  if(nsq>99){nsq <- nsq - 20} #If it goes over the top then cycle back to "A"
  nsql <- nsqls[nsq]
  east <- plyr::round_any(Easting,1) - plyr::round_any(Easting,100000,floor) #Snatch the right 5 digits off the easting
  easting5 <- f_pad_zero(round(east,0), width = 5, pad.char = "0") #fill the front with zeros if needed
  north <- plyr::round_any(Northing,1) - plyr::round_any(Northing,100000,floor) #Snatch the right 5 digits off the northing
  northing5 <- f_pad_zero(north, width = 5, pad.char = "0") #fill the front with zeros if needed
  utmz <- f_pad_zero(zone, width = 2, pad.char = "0") #pad the zone with a zero, if needed
  MGRS <- paste(utmz,lz,esql,nsql,easting5,northing5,sep = "") #Paste all six pieces together
  
  
  hem <- "N"
  if (lat < 0) {hem <- "S"}
  
  check <- latlng_to_mgrs(lat,lng)
  
  final <- as.data.frame(rbind(c(lng,lat, Easting,Northing,utmz,hem,MGRS,check))) 
  colnames(final) <- c("Long","Lat","Easting","Northing","zone","Hemisphere","MGRS","Check")  
  
   return(final)
}

## Here is the function that turns

LL2UTM2mgrs(-138.23,-34.55)

longv <- (sample.int(36000,size=200,replace=TRUE)-1)/100-180
latv <- (sample.int(16000,size=200,replace=TRUE)-1)/100-80
samplell <- data.frame(cbind(longv,latv))


samplell$UTME <- NA
samplell$UTMN <- NA
samplell$gz <- NA
samplell$hem <- NA
samplell$mgrs <- NA
samplell$check <- NA

for (i in 1:nrow(samplell)){
  samplell$UTME[i] <- as.matrix(LL2UTM2mgrs(samplell$longv[i],samplell$latv[i]))[3]
  samplell$UTMN[i] <- as.matrix(LL2UTM2mgrs(samplell$longv[i],samplell$latv[i]))[4]
  samplell$gz[i] <- as.matrix(LL2UTM2mgrs(samplell$longv[i],samplell$latv[i]))[5]
  samplell$hem[i] <- as.matrix(LL2UTM2mgrs(samplell$longv[i],samplell$latv[i]))[6]
  samplell$mgrs[i] <- as.matrix(LL2UTM2mgrs(samplell$longv[i],samplell$latv[i]))[7]
  samplell$check[i] <- as.matrix(LL2UTM2mgrs(samplell$longv[i],samplell$latv[i]))[8]
}
