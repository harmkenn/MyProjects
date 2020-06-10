lat1d <- 54+8/60+53.17/3600
lat2d <- 53+30/60+28.07/3600
lng1d <- -117
lng2d <- -115-56/60-1.66/3600

#in Radians
lat1r <- lat1d*pi/180 # Start Lat to radians
lng1r <- lng1d*pi/180 # Start Longitude to radians
lat2r <- lat2d*pi/180 # End Lat to radians
lng2r <- lng2d*pi/180 # End Longitude to radians

#Constants https://en.wikipedia.org/wiki/Latitude#The_geometry_of_the_ellipsoid
a <- 6378137.0 #equitorial radius in meters (exact)
f <- 1/298.257223563 #flattening of the earth (exact)
b <- (1-f)*a #polar radius (shorter than a)
esq <- 2*f-f^2 #eccentricity squared

#Reduced Latitudes https://en.wikipedia.org/wiki/Latitude#Parametric_(or_reduced)_latitude
lat1rr <- atan((1-f)*tan(lat1r))
lat2rr <- atan((1-f)*tan(lat2r))

#Central Angle between the 2 points (latrr,lngr)using the Haversine formula
# https://en.wikipedia.org/wiki/Haversine_formula
HavC <- (1-cos(lat2rr-lat1rr))/2 + cos(lat1rr) * cos(lat2rr) * (1-cos(lng2r-lng1r))/2
ca <- acos(1-2*HavC)

#Lamberts Distance inverse Geodesic https://en.wikipedia.org/wiki/Geographical_distance#Lambert's_formula_for_long_lines
P <- (lat2rr + lat1rr)/2
Q <- (lat2rr - lat1rr)/2
X <- (ca-sin(ca))*(sin(P)*cos(Q)/cos(ca/2))^2
Y <- (ca+sin(ca))*(sin(Q)*cos(P)/sin(ca/2))^2
dist <- a*(ca-f/2*(X+Y))
