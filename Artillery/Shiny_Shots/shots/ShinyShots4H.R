library(shiny)
library(tidyverse)
library(ggmap)
library(geosphere)
library(leaflet)
library(gridExtra)
library(grid)

#database start
load("northdes.rda")
load("4H.rda")
c.elev <- glm(Elev~poly(Range,16,raw=TRUE), data = M232.4H)
c.TOF <- glm(TOF~poly(Range,16,raw=TRUE), data = M232.4H)
c.drift <- glm(Drift~poly(Range,16,raw=TRUE), data = M232.4H)

#database end

# Functions Begin
LL2UTM2mgrs<-function(lat,lng){
  # Outside Functions I used
  mod <- pracma::mod
  match_df <- plyr::match_df
  f_pad_zero <- numform::f_pad_zero
  # My Stuff
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
  
  Easting <- floor(Easting)
  Northing <- floor(Northing)
  
  
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
  
  final <- as.data.frame(rbind(c(lat,lng, Easting,Northing,utmz,hem,MGRS)))
  colnames(final) <- c("Latitude","Longitude","Easting","Northing","zone","Hemisphere","MGRS")
  
  
  return(final)
}

MGRS2UTM2LL <-function(MGRS){
  
  # Outside Functions I used
  mod <- pracma::mod
  match_df <- plyr::match_df
  f_pad_zero <- numform::f_pad_zero
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
  gll <- stringr::str_locate(MGRS, gl)
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
  # Outside Functions I used
  mod <- pracma::mod
  match_df <- plyr::match_df
  f_pad_zero <- numform::f_pad_zero
  # My Stuff
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
             "Distance km" = dist,
             "Launch Bearing" = paste(round(shotd,2)),
             "Landing Bearing" = paste(round(impd,2)),
             "Midpoint Lat" = round(latmd,2),
             "Midpoint Lng" = round(lngmd,2))
}

polar_ll <- function(lat1d,lng1d,dist,shotd){
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
  
  data.frame("Launch Latitude" = lat1d,
             "Launch Longitude" = lng1d,
             "Distance km" = dist,
             "Launch Bearing" = paste(round(shotd,2)),
             "Landing Latitude" = lat2d,
             "Landing Longitude" = lng2d,
             "Landing Bearing" = round(impd,2),
             "Midpoint Lat" = round(latmd,2),
             "Midpoint Lng" = round(lngmd,2))
}




# Functions End

ui <- fluidPage(
  titlePanel("Shots by Ken Harmon", windowTitle = "Shots"),
  tabsetPanel(
    tabPanel("Deflection",
      sidebarLayout(
        sidebarPanel(
          radioButtons(inputId = "maptype",label = "Pick a Map Type",
                        choices = c("Road Map"="OpenStreetMap.Mapnik",
                                    "Terrain"="Stamen.Terrain",
                                    "Satellite"="Esri.WorldImagery",
                                    "Night Lights" = "NASAGIBS.ViirsEarthAtNight2012")),
          textInput("MGRS","Center MGRS", "11TNH7000080000"),
          numericInput("aof","Azimuth of Fire",2000,min = 0,max = 6399),
          actionButton(inputId = "get_map", label = "Get Map"),
          textOutput("Target"),
          textOutput("dist"),
          textOutput("az"),
          textOutput("defl"),
          textInput("lookup","MGRS lookup"),
          actionButton(inputId = "get_mgrs", label = "Get Grid"),
          textOutput("lMGRS"),
          textOutput("lLngLat")
        ),
        mainPanel(
          leafletOutput("mymap", width = "600px", height = "600px")
        )
      )
    ),

# TAB 2

    tabPanel("Point to Point",
      sidebarLayout(
        sidebarPanel(
          radioButtons(inputId = "maptype2",label = "Pick a Map Type",
            choices = c("Road Map"="OpenStreetMap.Mapnik",
                        "Terrain"="Stamen.Terrain",
                        "Satellite"="Esri.WorldImagery",
                        "Night Lights" = "NASAGIBS.ViirsEarthAtNight2012")),
          textInput("From","Launch From MGRS:","11SNV3000010000"),
          textInput("To","Land at MGRS:","11SNV3000020000"),
          actionButton(inputId = "get_map2", label = "Get Map"),
          textInput("lookup2","MGRS lookup"),
          actionButton(inputId = "get_mgrs2", label = "Get Grid"),
          textOutput("lMGRS2"),
          textOutput("lLngLat2") 
        ), # End of Sidebar Panel P2P
        mainPanel(
          textOutput("shot"),
          leafletOutput("mymap2", width = "600px", height = "600px")
        ) # End of Main Panel P2P
      ) # End of Sidebar Layout P2P
    ), #End of Tabpanel P2P

# TAB 3

    tabPanel("Polar Shot",
      sidebarLayout(
        sidebarPanel(
          radioButtons(inputId = "maptype3",label = "Pick a Map Type",
            choices = c("Road Map"="OpenStreetMap.Mapnik",
                        "Terrain"="Stamen.Terrain",
                        "Satellite"="Esri.WorldImagery",
                        "Night Lights" = "NASAGIBS.ViirsEarthAtNight2012")),
          textInput("From3","Launch From MGRS:","11SNV3000010000"),
          textInput("Direction3","Direction in Degrees:",45),
          textInput("Distance3","Distance in km",10),
          actionButton(inputId = "get_map3", label = "Get Map"),
          textInput("lookup3","MGRS lookup"),
          actionButton(inputId = "get_mgrs3", label = "Get Grid"),
          textOutput("lMGRS3"),
          textOutput("lLngLat3") 
        ),
        mainPanel(
          textOutput("shot3"),
          leafletOutput("mymap3", width = "600px", height = "600px")
        ) # end of the mainPanal 3
      ) # end of the sidebarPanel 3
    ), # end of tabpanel polar

# TAB 4

tabPanel("AFATDS",
       sidebarLayout(
         sidebarPanel(
           textInput("From_4","Launch From MGRS:","11SNV3131314141"),
           textInput("From_alt","Launch Altitude M:","750"),
           textInput("To_4","Land to MGRS:","11SNV2626231313"),
           textInput("To_alt","Impact Altitude M:","650"),
           textInput("AOF4","Azimuth of Fire:","2000"),
           actionButton(inputId = "get_sol", label = "Get Solution"),
           textInput("lookup4","MGRS lookup"),
           actionButton(inputId = "get_mgrs4", label = "Get Grid"),
           textOutput("lMGRS4"),
           textOutput("lLngLat4") 
         ), # End of Sidebar Panel P2P
         mainPanel(
           textOutput("shot4"),
           plotOutput("plot4", width = "600px", height = "600px")
         ) # End of Main Panel AFATDS
       ) # End of Sidebar Layout AFATDS
    ) #End of Tabpanel AFATDS

  ) #end of the tabset panel
) #end of the ui

server <- function(input, output) {
  output$mymap <- renderLeaflet({
    if (input$get_map == 0) 
      return()
      isolate({
        mgrs <- MGRS2UTM2LL(input$MGRS)
        clat <- as.numeric(as.vector(mgrs[1,6]))
        clng <- as.numeric(as.vector(mgrs[1,7]))
        aof <- input$aof
        aoftip <- polar_ll(clat,clng,30,input$aof/6400*360)
        aoftiplat <- as.numeric(as.vector(aoftip[1,5]))
        aoftiplng <- as.numeric(as.vector(aoftip[1,6]))
        aofdf <- data.frame(lng = c(clng,aoftiplng),lat = c(clat,aoftiplat))
        leaflet() %>% 
          setView(clng,clat,zoom=10) %>%
          addProviderTiles(input$maptype) %>%
          addCircleMarkers(lng = clng, lat = clat, radius = 5) %>%
          addPolylines(data = aofdf, ~lng, ~lat, group = "aof", color = "orange")
      })
  })
  observeEvent(input$get_mgrs, {
    output$lMGRS <- renderText({paste(" ")})
    output$lLngLat <- renderText({paste(" ")})
    register_google(key = "AIzaSyAfnLNZjvYdMx-cyga_qA1oJ6P36dRGalA")
    lLatLon <- geocode(input$lookup)
    llng <- lLatLon$lon
    llat <- lLatLon$lat
    lmgrs <- LL2UTM2mgrs(llat,llng)
    output$lMGRS <- renderText({paste("MGRS: ", lmgrs[1,7])})
    output$lLngLat <- renderText({paste("Longitude: ", llng, "Latitude: ", llat)})
  })
  observeEvent(input$mymap_click, {
    click <- input$mymap_click
    tlng <- click$lng
    tlat <- click$lat
    mgrs <- MGRS2UTM2LL(input$MGRS)
    clat <- as.numeric(as.vector(mgrs[1,6]))
    clng <- as.numeric(as.vector(mgrs[1,7]))
    output$Target <-  renderText({paste("Target: ", LL2UTM2mgrs(clat,clng)[1,7])})
    shot <- shot_ll(clat,clng,tlat,tlng)
    output$dist <- renderText({paste("Distance to Target: ", shot[1,5])})
    aof <- input$aof
    az <- as.numeric(as.vector(shot[1,6]))*6400/360
      if (az<0) {az <- az + 6400}
      az <- round(az,1)
    output$az <- renderText({paste("Azimuth to Target: ", az, "mils")})
    defl <- 3200+(aof-az)
      if (defl < 0) {defl <- defl + 6400}
    output$defl <- renderText({paste("Deflection to Target: ", defl, "mils")})
    proxy <- leafletProxy("mymap")
    pointdf <- data.frame(lng = c(clng,tlng),lat = c(clat,tlat))
        ## This displays the pin drop circle
    proxy %>% 
      clearGroup("new_point") %>%
      #clearMarkers(layerId=input$mymap_click$id) %>%
      #addPopups(click$lng, click$lat) %>%
      addCircles(click$lng, click$lat, radius=100, color="red", group = "new_point") %>%
      addPolylines(data = pointdf, ~lng, ~lat, group = "new_point")
  })
  
# TAB 2
  
  output$mymap2 <- renderLeaflet({
    if (input$get_map2 == 0) 
      return()
    isolate({
      From <- MGRS2UTM2LL(input$From)
      flat <- as.numeric(as.vector(From[1,6]))
      flng <- as.numeric(as.vector(From[1,7]))
      To <- MGRS2UTM2LL(input$To)
      tlat <- as.numeric(as.vector(To[1,6]))
      tlng <- as.numeric(as.vector(To[1,7]))
      oflng <- flng
      otlng <- tlng
      flight <- "East"
      
      if (abs(tlng - flng) > 180) {
        if (tlng > flng){
          tlng <- tlng - 360
          flight <- "West"
        }else{
          flng <- flng - 360
          flight <- "West"
        } #End of the else
      } #End of the if
      shot <- shot_ll(flat,flng,tlat,tlng)
      mlat <- as.numeric(as.vector(shot[1,8]))
      mlng <- as.numeric(as.vector(shot[1,9]))
      dist <- as.numeric(as.vector(shot[1,5]))
      zoom <- 15.3-log(dist,2)
      gcroute <- gcIntermediate(c(flng,flat),c(tlng,tlat), breakAtDateLine = T, n = 100, 
                                       addStartEnd = TRUE, sp = T)
      if (flight == "West"){
        mess <- as_tibble(gcroute@lines[[1]]@Lines[[1]]@coords)
        mess <- mess %>% mutate(lon = ifelse(lon > 0, lon -360, lon))
        mess$lon[1] <- flng
        mess$lon[101] <- tlng
        gcroute@lines[[1]]@Lines[[1]]@coords <- as.matrix(mess)
      }
      launchb <- as.numeric(as.vector(shot[1,6]))
      if (launchb < 0) {launchb <- launchb + 360}
      output$shot <- renderText({paste("Distance: ", round(dist,1), "km  Launch Bearing: ",
                                       launchb,
                                       "    Landing Bearing: ", as.numeric(as.vector(shot[1,7])))})
      leaflet() %>% 
        setView(mlng,mlat,zoom=zoom) %>%
        addProviderTiles(input$maptype2) %>%
        addPolylines(data = gcroute, weight = 2, color = "orange") %>%
        addCircleMarkers(lng = flng, lat = flat, radius = 5, color = "Blue") %>%
        addCircleMarkers(lng = tlng, lat = tlat, radius = 5, color = "Brown")
    })
  })
  observeEvent(input$get_mgrs2, {
    output$lMGRS2 <- renderText({paste(" ")})
    output$lLngLat2 <- renderText({paste(" ")})
    register_google(key = "AIzaSyAfnLNZjvYdMx-cyga_qA1oJ6P36dRGalA")
    lLatLon <- geocode(input$lookup2)
    llng <- lLatLon$lon
    llat <- lLatLon$lat
    lmgrs <- LL2UTM2mgrs(llat,llng)
    output$lMGRS2 <- renderText({paste("MGRS: ", lmgrs[1,7])})
    output$lLngLat2 <- renderText({paste("Longitude: ", llng, "Latitude: ", llat)})
  })


# Tab 3

output$mymap3 <- renderLeaflet({
  if (input$get_map3 == 0) 
    return()
  isolate({
    From <- MGRS2UTM2LL(input$From3)
    flat <- as.numeric(as.vector(From[1,6]))
    flng <- as.numeric(as.vector(From[1,7]))
    dist <- as.numeric(input$Distance3)
    dir <- as.numeric(input$Direction3)
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
    
    if (tlng > 180){
      rlng <- tlng - 360
    } else if (tlng < -180){
      rlng <- tlng + 360
    } else {
      rlng <- tlng
    } #End of if
    
    TMGRS <- LL2UTM2mgrs(tlat,rlng)
    output$shot3 <- renderText({paste("Target MGRS: ", TMGRS[1,7],
                                     "    Landing Bearing: ", tbear)})
    leaflet() %>% 
      setView(mlng,mlat,zoom=zoom) %>%
      addProviderTiles(input$maptype3) %>%
      addPolylines(data = gcroute, weight = 2, color = "orange") %>%
      addCircleMarkers(lng = flng, lat = flat, radius = 5, color = "Blue") %>%
      addCircleMarkers(lng = tlng, lat = tlat, radius = 5, color = "Brown")
  })
})
observeEvent(input$get_mgrs3, {
  output$lMGRS3 <- renderText({paste(" ")})
  output$lLngLat3 <- renderText({paste(" ")})
  register_google(key = "AIzaSyAfnLNZjvYdMx-cyga_qA1oJ6P36dRGalA")
  lLatLon <- geocode(input$lookup3)
  llng <- lLatLon$lon
  llat <- lLatLon$lat
  lmgrs <- LL2UTM2mgrs(llat,llng)
  output$lMGRS3 <- renderText({paste("MGRS: ", lmgrs[1,7])})
  output$lLngLat3 <- renderText({paste("Longitude: ", llng, "Latitude: ", llat)})
}) #End of the observeEvent(input$get_mgrs3

# TAB 4 AFATDS
observeEvent(input$get_sol, {
  output$shot4 <- renderText({paste(" ")})
  Flatlng <- MGRS2UTM2LL(input$From_4)
  Flat <- as.numeric(as.vector(Flatlng[1,6]))
  Flng <- as.numeric(as.vector(Flatlng[1,7]))
  Tlatlng <- MGRS2UTM2LL(input$To_4)
  Tlat <- as.numeric(as.vector(Tlatlng[1,6]))
  Tlng <- as.numeric(as.vector(Tlatlng[1,7]))
  out <- shot_ll(Flat,Flng,Tlat,Tlng)
  range <- round(1000*out[1,5],0)
  test <- data.frame(Range=range)
  Elev <- as.numeric(predict(c.elev, test, type = "response"))
  TOF <- as.numeric(predict(c.TOF, test, type = "response"))
  drift <- as.numeric(predict(c.drift, test, type = "response"))
  az <- as.numeric(as.vector(out[1,6]))*3200/180
  if (az<0) {az <- az + 6400}
  az <- round(az,1)
  aof <- as.numeric(input$AOF4)
  defl <- 3200+(aof-az)+drift
  if (defl < 0) {defl <- defl + 6400}
  T_alt <- as.numeric(input$To_alt)
  F_alt <- as.numeric(input$From_alt)
  AOS <- atan((T_alt-F_alt)/range)*3200/pi
  CSF <- 0 
  CAS <- round(abs(AOS)*CSF,1)
  site <- AOS+CAS
  QE <- Elev +site
  FM <- data.frame(cbind(c(range,az,aof,drift,defl,"M795","M232 4H",Elev,site,QE,TOF)))
  colnames(FM) <- c("Fire Mission")
  rownames(FM) <- c("Range","Azimuth to Target","Azimuth of Fire","Drift","Deflection","Shell","Charge","Elevation","Site","QE","TOF")
  output$plot4 <- renderPlot({grid.arrange(tableGrob(FM),ncol=1)})
   
}) #observeEvent get mgrs4

observeEvent(input$get_mgrs4, {
  output$lMGRS4 <- renderText({paste(" ")})
  output$lLngLat4 <- renderText({paste(" ")})
  register_google(key = "AIzaSyAfnLNZjvYdMx-cyga_qA1oJ6P36dRGalA")
  lLatLon <- geocode(input$lookup4)
  llng <- lLatLon$lon
  llat <- lLatLon$lat
  lmgrs <- LL2UTM2mgrs(llat,llng)
  output$lMGRS4 <- renderText({paste("MGRS: ", lmgrs[1,7])})
  output$lLngLat4 <- renderText({paste("Longitude: ", llng, "Latitude: ", llat)})
}) #observeEvent get mgrs4
} #End of the server

shinyApp(ui, server)