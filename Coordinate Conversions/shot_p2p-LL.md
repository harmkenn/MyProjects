---
title: "Compute Distance from Lat Long"
author: "Ken Harmon"
date: "2019 December 05"
output:
  html_document:
    code_folding: hide
    fig_align: center
    fig_height: 6
    fig_width: 12
    keep_md: yes
editor_options:
  chunk_output_type: console
---

# {.tabset .tabset-fade}







## Point to Point Direction Steps


```r
#Points
lat1 <- -30
lng1 <- -40
lat2 <- 40
lng2 <- 30

#in Radians
lat1r <- lat1*pi/180 
lng1r <- lng1*pi/180
lat2r <- lat2*pi/180 
lng2r <- lng2*pi/180

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


points <- data.frame("Launch Latitude" = lat1, "Launch Longitude" = lng1, 
                     "Landing Latitude" = lat2, "Landing Longitude" = lng2)
pander(points)
```


---------------------------------------------------------------------------
 Launch.Latitude   Launch.Longitude   Landing.Latitude   Landing.Longitude 
----------------- ------------------ ------------------ -------------------
       -30               -40                 40                 30         
---------------------------------------------------------------------------

```r
stats <- data.frame("Distance" = paste(round(dist,0),"km"),
                    "Launch Bearing" = paste(shotd,"°"))
pander(stats)
```


------------------------------
 Distance    Launch.Bearing   
---------- -------------------
 10610 km  46.309501928312 Â° 
------------------------------

## Point to Point Direction Function


```r
dist_p2p_ll <- function(lat1,lng1,lat2,lng2){
  #in Radians
lat1r <- lat1*pi/180 #alpha
lng1r <- lng1*pi/180
lat2r <- lat2*pi/180 #gamma
lng2r <- lng2*pi/180
beta <- lng2r - lng1r

#Central angle between points
theta <- acos(cos(lat1r)*cos(lat2r)*cos(beta)+sin(lat1r)*sin(lat2r)) 
er <-6371 #mean radius of the earth in km

#Distance between points
dist <- theta*er

dist
}
```


```r
dist_p2p_ll(-30,-40,40,30)
```

```
## [1] 10610.46
```

