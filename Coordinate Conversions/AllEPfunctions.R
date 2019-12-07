LL2UTM2mgrs<-function(lng,lat){
  
  hem <- "N"
  if (lat < 0){hem <- "S"}
  
  
  # 100km grid Square easting letter
  esqls <- as.matrix(rbind(c("A","B","C","D","E","F","G","H"),
                           c("J","K","L","M","N","P","Q","R"),
                           c("S","T","U","V","W","X","Y","Z")))
  colnames(esqls) <- 1:8
  rownames(esqls) <- c(1,2,0)
  
  
  lngr <- lng*pi/180 # Longitude in radians
  latr <- lat*pi/180 # Latitude in radians
  
  # Lets go find Easting
  
  sin1 <- pi/(180*3600) #One Second
  a <- 6378137 # Equitorial Radius
  b <- 6356752.31424518 #Polar Radius
  k0 <- .9996 #Scalar Factor Constant
  gzn <- floor(1/6*lng)+31 #Longitude Zone
  Czone <- 6*gzn - 183 #Longitude of the center of the zone
  dlng <- lng - Czone # Longitude from the center of the zone
  p <- dlng*3600/10000 #Hecta seconds?
  e <- sqrt(1-(b/a)^2) #eccentricity
  e1 <- sqrt(a^2-b^2)/b
  e1sq <- e1^2
  c <- a^2/b
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
  gze <- "Odd"
  if((gzn %% 2) == 0) {gze <- "Even"}
  
  #Latitude gridzone letters
  gznls <- c("C","D","E","F","G","H","J","K","L","M",
             "N","P","Q","R","S","T","U","V","W","X")
  lzc <- (lat+80-(lat+80)%%8)/8+1 # Latitude letter zone count
  gzl <- gznls[lzc]
  
  gsen <-  plyr::round_any(Easting,100000,floor)/100000 #Grab off the first digit off the Easting
  gsnn <-  plyr::round_any(Northing,100000,floor)/100000 #Grab off the first two digits off the Northing
  
  emod <- mod(gzn,3) # gzn mod 3 for 100km lookup
  if (emod == 0) {emod <- 3} #turn mod zero into row 3
  gsel <- esqls[emod,gsen] #Here is the 100km easting grid letter
  
  alln <- data.frame(hem = hem, gze = gze, gzl = gzl, gsnl = NA, gsnn = gsnn)
  fulln<-match_df(northdes,alln,on=c("hem","gze","gzl","gsnn"))
  
  gsnl <- as.character(fulln[1,4])
  
  east <- plyr::round_any(Easting,1) - plyr::round_any(Easting,100000,floor) #Snatch the right 5 digits off the easting
  easting5 <- f_pad_zero(round(east,0), width = 5, pad.char = "0") #fill the front with zeros if needed
  north <- plyr::round_any(Northing,1) - plyr::round_any(Northing,100000,floor) #Snatch the right 5 digits off the northing
  northing5 <- f_pad_zero(north, width = 5, pad.char = "0") #fill the front with zeros if needed
  
  utmz <- f_pad_zero(gzn, width = 2, pad.char = "0") #pad the zone with a zero, if needed
  MGRS <- paste(utmz,gzl,gsel,gsnl,easting5,northing5,sep = "") #Paste all six pieces together
  
  final <- as.data.frame(rbind(c(lng,lat, Easting,Northing,utmz,hem,MGRS))) 
  colnames(final) <- c("Long","Lat","Easting","Northing","zone","Hemisphere","MGRS") 
  
  
  return(final)
}

MGRS2UTM2LL <-function(MGRS){

  # 100km grid Square easting letter
  esqls <- as.matrix(rbind(c("A","B","C","D","E","F","G","H"),
                           c("J","K","L","M","N","P","Q","R"),
                           c("S","T","U","V","W","X","Y","Z")))
  colnames(esqls) <- 1:8
  rownames(esqls) <- c(1,2,0)
  
  sin1 <- pi/(180*3600) #One Second
  a <- 6378137 # Equitorial Radius
  b <- 6356752.31424518 #Polar Radius
  k0 <- .9996 #Scalar Factor Constant
  e1 <- sqrt(a^2-b^2)/b
  e1sq <- e1^2
  c <- a^2/b
  
  gl <- gsub("[^a-zA-Z]", "", MGRS)
  gll <- str_locate(MGRS, gl)
  gzn <- as.numeric(substr(MGRS, 1, gll[1]-1))
  gze <- "Odd"
  if((gzn %% 2) == 0) {gze <- "Even"}
  gzl <- substr(MGRS, gll[1],gll[1])
  gsel <- substr(MGRS, gll[1]+1,gll[1]+1)
  gsnl <- substr(MGRS, gll[1]+2,gll[1]+2)
  hem <- "S"
  if (grepl(gzl, "NPQRSTUVWX")){hem <- "N"}
  
  east5 <- as.numeric(substr(MGRS, gll[1]+3,gll[1]+7))
  Easting <- east5 + 100000*which(esqls == gsel,arr.ind=T)[2]
  north5 <- as.numeric(substr(MGRS, gll[1]+8,gll[1]+12))
  alln <- data.frame(hem = NA, gze = gze, gzl = gzl, gsnl = gsnl, ngsn = NA)
  fulln<-match_df(northdes,alln,on=c("gze","gzl","gsnl"))
  Northing <- 100000*fulln[1,5]+north5
  
  NfEQ <- Northing
  if (hem == "S"){NfEQ <- Northing - 10000000}
  Fi <- (NfEQ)/(6366197.724*k0)
  Ni <- (c/(1+e1sq*(cos(Fi))^2)^(1/2))*k0
  Czone <- 6*gzn-183
  dln <- (Easting-500000)/Ni
  A1 <- sin(2*Fi)
  A2 <- A1*(cos(Fi))^2
  J2 <- Fi+(A1/2)
  J4 <- (3*J2+A2)/4
  J6 <- (5*J4+A2*(cos(Fi))^2)/3
  alfa <- 3/4*e1sq
  beta <- 5/3*alfa^2
  gamma <- 35/27*alfa^3
  Bfi <- k0*c*(Fi-(alfa*J2)+(beta*J4)-(gamma*J6))
  BB <- (NfEQ-Bfi)/Ni
  zeta <- ((e1sq*dln^2)/2)*(cos(Fi))^2
  Xi <- dln*(1-(zeta/3))
  Eta <- BB*(1-zeta)+Fi
  ShXi <- (exp(Xi)-exp(-Xi))/2
  dLam <- atan(ShXi/cos(Eta))
  Tau <- atan(cos(dLam)*tan(Eta))
  FiR <- Fi+(1+e1sq*(cos(Fi))^2-(3/2)*e1sq*sin(Fi)*cos(Fi)*(Tau-Fi))*(Tau-Fi)
  lat <- FiR/pi*180
  lng <- dLam/pi*180+Czone 
  
  as.data.frame(rbind(c("MGRS"=MGRS,"Easting"=Easting,"Northing"=Northing,"Zone"=gzn,"Hemisphere"=hem,"Latitude"=round(lat,2),"Longitude"=round(lng,2))))
}

UTM2MGRS2LL <- function(Easting,Northing,gzn,hem){

  Easting <- round(Easting,0)
  Northing <- round(Northing,0)
  
  sin1 <- pi/(180*3600) #One Second
  a <- 6378137 # Equitorial Radius
  b <- 6356752.31424518 #Polar Radius
  k0 <- .9996 #Scalar Factor Constant
  e1 <- sqrt(a^2-b^2)/b
  e1sq <- e1^2
  c <- a^2/b
  
  NfEQ <- Northing
  if (hem == "S"){NfEQ <- Northing - 10000000}
  Fi <- (NfEQ)/(6366197.724*k0)
  Ni <- (c/(1+e1sq*(cos(Fi))^2)^(1/2))*k0
  Czone <- 6*gzn-183
  dln <- (Easting-500000)/Ni
  A1 <- sin(2*Fi)
  A2 <- A1*(cos(Fi))^2
  J2 <- Fi+(A1/2)
  J4 <- (3*J2+A2)/4
  J6 <- (5*J4+A2*(cos(Fi))^2)/3
  alfa <- 3/4*e1sq
  beta <- 5/3*alfa^2
  gamma <- 35/27*alfa^3
  Bfi <- k0*c*(Fi-(alfa*J2)+(beta*J4)-(gamma*J6))
  BB <- (NfEQ-Bfi)/Ni
  zeta <- ((e1sq*dln^2)/2)*(cos(Fi))^2
  Xi <- dln*(1-(zeta/3))
  Eta <- BB*(1-zeta)+Fi
  ShXi <- (exp(Xi)-exp(-Xi))/2
  dLam <- atan(ShXi/cos(Eta))
  Tau <- atan(cos(dLam)*tan(Eta))
  FiR <- Fi+(1+e1sq*(cos(Fi))^2-(3/2)*e1sq*sin(Fi)*cos(Fi)*(Tau-Fi))*(Tau-Fi)
  lat <- FiR/pi*180
  lng <- dLam/pi*180+Czone 
  
  ## Now let's turn UTM into MGRS
  gze <- "Odd"
  if((gzn %% 2) == 0) {gze <- "Even"}
  
  #Latitude gridzone letters
  gznls <- c("C","D","E","F","G","H","J","K","L","M",
             "N","P","Q","R","S","T","U","V","W","X")
  lzc <- (lat+80-(lat+80)%%8)/8+1 # Latitude letter zone count
  gzl <- gznls[lzc]
  
  # 100km grid Square easting letter
  esqls <- as.matrix(rbind(c("A","B","C","D","E","F","G","H"),
                           c("J","K","L","M","N","P","Q","R"),
                           c("S","T","U","V","W","X","Y","Z")))
  colnames(esqls) <- 1:8
  rownames(esqls) <- c(1,2,0)
  
  gsen <-  plyr::round_any(Easting,100000,floor)/100000 #Grab off the first digit off the Easting
  gsnn <-  plyr::round_any(Northing,100000,floor)/100000 #Grab off the first two digits off the Northing
  
  emod <- mod(gzn,3) # gzn mod 3 for 100km lookup
  if (emod == 0) {emod <- 3} #turn mod zero into row 3
  gsel <- esqls[emod,gsen] #Here is the 100km easting grid letter
  
  alln <- data.frame(hem = hem, gze = gze, gzl = gzl, gsnl = NA, gsnn = gsnn)
  fulln<-match_df(northdes,alln,on=c("hem","gze","gzl","gsnn"))
  
  gsnl <- as.character(fulln[1,4])
  
  east <- plyr::round_any(Easting,1) - plyr::round_any(Easting,100000,floor) #Snatch the right 5 digits off the easting
  easting5 <- f_pad_zero(round(east,0), width = 5, pad.char = "0") #fill the front with zeros if needed
  north <- plyr::round_any(Northing,1) - plyr::round_any(Northing,100000,floor) #Snatch the right 5 digits off the northing
  northing5 <- f_pad_zero(north, width = 5, pad.char = "0") #fill the front with zeros if needed
  
  utmz <- f_pad_zero(gzn, width = 2, pad.char = "0") #pad the zone with a zero, if needed
  MGRS <- paste(utmz,gzl,gsel,gsnl,easting5,northing5,sep = "") #Paste all six pieces together
  
  as.data.frame(rbind(c("Easting" = Easting,"Northing" = Northing, "Zone" = gzn, "Hemisphere" = hem, "Latitude" = round(lat,2), "Longitude"=round(lng,2), "MGRS" = MGRS)))
}

shot_ll <- function(lat1d,lng1d,lat2d,lng2d){
  #in Radians
  lat1r <- lat1d*pi/180 
  lng1r <- lng1d*pi/180
  lat2r <- lat2d*pi/180 
  lng2r <- lng2d*pi/180
  
  beta <- lng2r - lng1r
  
  #Central angle between points
  theta <- acos(cos(lat1r)*cos(lat2r)*cos(beta)+sin(lat1r)*sin(lat2r)) 
  er <-6371 #mean radius of the earth in km
  
  #Distance between points
  dist <- theta*er
  
  #Bearing of the shot in radians
  shotr <- atan2(sin(lng2r-lng1r)*cos(lat2r),
                 cos(lat1r)*sin(lat2r)-sin(lat1r)*cos(lat2r)*cos(lng2r-lng1r))
  #Bearing of the shot in degrees
  shotd <- shotr*180/pi
  
  #Bearing of the impact in radians
  impr <- pi + atan2(sin(lng1r-lng2r)*cos(lat1r),
                     cos(lat2r)*sin(lat1r)-sin(lat2r)*cos(lat1r)*cos(lng1r-lng2r))
  #Bearing of the impact in degrees
  impd <- impr*180/pi
  
  # Compute midpoint
  Bx <- cos(lat2r)*cos(lng2r-lng1r)
  By <- cos(lat2r)*sin(lng2r-lng1r)
  latmr <- atan2(sin(lat1r) + sin(lat2r), sqrt((cos(lat1r)+Bx)^2+By^2))
  lngmr <- lng1r + atan2(By, cos(lat1r) + Bx)
  
  latmd <- latmr*180/pi
  lngmd <- lngmr*180/pi
  
  
  data.frame("Launch Latitude" = lat1d, "Launch Longitude" = lng1d, 
             "Landing Latitude" = lat2d, "Landing Longitude" = lng2d,
             "Distance" = paste(round(dist,0),"km"),
             "Launch Bearing" = paste(round(shotd,2)),
             "Landing Bearing" = paste(round(impd,2)),
             "Midpoint Lat" = round(latmd,2),
             "Midpoint Lng" = round(lngmd,2))
}

polar_ll <- function(lat1,lng1,dist,shotd){
  #in Radians
  lat1r <- lat1d*pi/180 
  lng1r <- lng1d*pi/180
  shotr <- shotd*pi/180
  er <- 6371 #Earth Radius in km
  delta <- dist/er
  
  lat2r <- asin(sin(lat1r)*cos(delta)+cos(lat1r)*sin(delta)*cos(shotr))
  lng2r <- lng1r + atan2(sin(shotr)*sin(delta)*cos(lat1r),
                         cos(delta)-sin(lat1r)*sin(lat2r))
  
  lat2d <- lat2r*180/pi
  lng2d <- lng2r*180/pi
  
  #Bearing of the impact in radians
  impr <- pi + atan2(sin(lng1r-lng2r)*cos(lat1r),
                     cos(lat2r)*sin(lat1r)-sin(lat2r)*cos(lat1r)*cos(lng1r-lng2r))
  #Bearing of the impact in degrees
  impd <- impr*180/pi
  
  # Compute midpoint
  Bx <- cos(lat2r)*cos(lng2r-lng1r)
  By <- cos(lat2r)*sin(lng2r-lng1r)
  latmr <- atan2(sin(lat1r) + sin(lat2r), sqrt((cos(lat1r)+Bx)^2+By^2))
  lngmr <- lng1r + atan2(By, cos(lat1r) + Bx)
  
  latmd <- latmr*180/pi
  lngmd <- lngmr*180/pi
  
  data.frame("Launch Latitude" = lat1, 
             "Launch Longitude" = lng1, 
             "Distance" = paste(round(dist,0),"km"), 
             "Launch Bearing" = paste(round(shotd,2)),
             "Landing Latitude" = lat2d,
             "Landing Longitude" = lng2d,
             "Landing Bearing" = round(impd,2),
             "Midpoint Lat" = round(latmd,2),
             "Midpoint Lng" = round(lngmd,2))
}

