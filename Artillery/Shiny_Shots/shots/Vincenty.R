lat1r <- lat1d*pi/180 # Start Lat to radians
lng1r <- lng1d*pi/180 # Start Longitude to radians
lat2r <- lat2d*pi/180 # End Lat to radians
lng2r <- lng2d*pi/180 # End Longitude to radians

a <- 6378137.0 # length of the semi-major axis radius of the equator WGS-84
f <- 1/298.257223563 #Flattening of the ellipsoid
b <- (1-f)*a
U1 <- atan((1-f)*tan(lat1r))
U2 <- atan((1-f)*tan(lat2r))
L <- lng2r - lng1r
lamda <- L
i <- 1:20

#Loop to get best lamda
for (val in i) {
  Ss <- sqrt((cos(U2)*sin(lamda))^2+(cos(U1)*sin(U2)-sin(U1)*cos(U2)*cos(lamda))^2)
  Cs <- sin(U1)*sin(U2)+cos(U1)*cos(U2)*cos(lamda)
  sigma <- atan2(Ss,Cs)
  Sa <- cos(U1)*cos(U2)*sin(lamda)/Ss
  C2sm <- cos(sigma)-2*sin(U1)*sin(U2)/(1-Sa^2)
  C <- f/16*(1-Sa^2)*(4+f*(1+3*Sa^2))
  lamda <- L+(1-C)*f*Sa*(sigma+C*Sa*(C2sm+C*Cs*(-1+2*C2sm^2)))
}

# now continue with good lamda
Usq <- (1-Sa^2)*((a^2-b^2)/b^2)
A <- 1+Usq/16384*(4096+Usq*(-768+Usq*(320-175*Usq)))
B <- Usq/1024*(256+Usq*(-128+Usq*(74-47*Usq)))
Ds <- B*Ss*(C2sm+1/4*B*(Cs*(-1+2*C2sm^2)-B/6*C2sm*(-3+4*Ss^2)*(-3+4*C2sm^2)))
dist <- b*A*(sigma-Ds)

#Bearing of the shot in radians


shotr <- atan2(cos(U2)*sin(lamda),cos(U1)*sin(U2)-sin(U1)*cos(U2)*cos(lamda))
#Bearing of the shot in degrees
shotd <- shotr*180/pi

#Bearing of the impact in radians
impr <- atan2(cos(U1)*sin(lamda),-1*sin(U1)*cos(U2)+cos(U1)*sin(U2)*cos(lamda))
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
           "Distance km" = dist,
           "Launch Bearing" = paste(round(shotd,2)),
           "Landing Bearing" = paste(round(impd,2)),
           "Midpoint Lat" = round(latmd,2),
           "Midpoint Lng" = round(lngmd,2))
