library(numform)
library(tidyverse)
library(plyr)
library(shiny)
library(ggmap)
#library(mgrs)
library(geosphere)
#library(sp)
library(leaflet)
library(EarthPoints)


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
        ),
        mainPanel(
          leafletOutput("mymap2", width = "600px", height = "600px")
        )
      )
    )
  )
)

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
      shot <- shot_ll(flat,flng,tlat,tlng)
      mlat <- as.numeric(as.vector(shot[1,8]))
      mlng <- as.numeric(as.vector(shot[1,9]))
      dist <- as.numeric(as.vector(shot[1,5]))
      zoom <- 15.3-log(dist,2)
      gcroute <- df2 <- gcIntermediate(c(flng,flat),c(tlng,tlat), breakAtDateLine = T, n = 100, 
                                       addStartEnd = TRUE, sp = T)
      leaflet() %>% 
        setView(mlng,mlat,zoom=zoom) %>%
        addProviderTiles(input$maptype2) %>%
        addPolylines(data = gcroute, weight = 1) %>%
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
}

shinyApp(ui, server)