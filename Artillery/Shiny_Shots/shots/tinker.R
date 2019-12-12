library(geosphere)
library(tidyverse)

"54SUE7785748874""10TET5020272750"

From <- MGRS2UTM2LL("10TET5020272750")
flat <- as.numeric(as.vector(From[1,6]))
flng <- as.numeric(as.vector(From[1,7]))
dist <- as.numeric(10000)
dir <- as.numeric(300)
polar <- polar_ll(flat,flng,dist,dir)
tlat <- as.numeric(as.vector(polar[1,5]))
tlng <- as.numeric(as.vector(polar[1,6]))
tbear <- as.numeric(as.vector(polar[1,7]))
mlat <- as.numeric(as.vector(polar[1,8]))
mlng <- as.numeric(as.vector(polar[1,9]))

zoom <- 15.3-log(dist,2)
gcroute <- gcIntermediate(c(flng,flat),c(tlng,tlat), breakAtDateLine = T, n = 100, 
                          addStartEnd = TRUE, sp = T)

if (dir < 0){
  flight <- "West"
} else if (dir > 180){
  flight <- "West"
} else {
  flight <- "East"
} #End of if

if (flight == "West"){
  idl <- tlng < -180
} else {
  idl <- tlng > 180
} #End of if

if (flight == "West") {
  if (idl == TRUE){
    mess <- as_tibble(gcroute@lines[[1]]@Lines[[1]]@coords)
    mess <- mess %>% mutate(lon = ifelse(lon >= 0, lon -360, lon))
    #mess$lon[1] <- flng
    mess$lon[101] <- tlng
    gcroute@lines[[1]]@Lines[[1]]@coords <- as.matrix(mess)
  } else if (idl == FALSE) {
    #mess <- as_tibble(gcroute@lines[[1]]@Lines[[1]]@coords)
    #mess <- mess %>% mutate(lon = ifelse(lon < 0, lon + 360, lon))
    #mess$lon[1] <- flng
    #mess$lon[101] <- tlng
    #gcroute@lines[[1]]@Lines[[1]]@coords <- as.matrix(mess)
  } 
} else {
  if (idl == TRUE){
    mess <- as_tibble(gcroute@lines[[1]]@Lines[[1]]@coords)
    mess <- mess %>% mutate(lon = ifelse(lon < 0, lon + 360, lon))
    #mess$lon[1] <- flng
    mess$lon[101] <- tlng
    gcroute@lines[[1]]@Lines[[1]]@coords <- as.matrix(mess)
  } else if (idl == FALSE) {
    #mess <- as_tibble(gcroute@lines[[1]]@Lines[[1]]@coords)
    #mess <- mess %>% mutate(lon = ifelse(lon < 0, lon + 360, lon))
    #mess$lon[1] <- flng
    #mess$lon[101] <- tlng
    #gcroute@lines[[1]]@Lines[[1]]@coords <- as.matrix(mess)
  }
}

TMGRS <- LL2UTM2mgrs(tlat,tlng)
paste("Target MGRS: ", TMGRS[1,7],"    Landing Bearing: ", tbear)
leaflet() %>% 
  setView(mlng,mlat,zoom=zoom) %>%
  addTiles() %>%
  addPolylines(data = gcroute, weight = 2, color = "orange") %>%
  addCircleMarkers(lng = flng, lat = flat, radius = 5, color = "Blue") %>%
  addCircleMarkers(lng = tlng, lat = tlat, radius = 5, color = "Brown")
