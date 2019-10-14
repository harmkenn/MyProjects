---
title: "Working with Maps"
author: "Ken Harmon"
date: "2019 October 14"
output:
  html_document:  
    keep_md: true
    code_folding: hide
    fig_height: 6
    fig_width: 12
    fig_align: 'center'
editor_options: 
  chunk_output_type: console
---

# {.tabset .tabset-fade}







https://www.r-bloggers.com/drawing-beautiful-maps-programmatically-with-r-sf-and-ggplot2-part-1-basics/

## Build the map


```r
world <- getMap(resolution = "high")
class(world)
```

```
## [1] "SpatialPolygonsDataFrame"
## attr(,"package")
## [1] "sp"
```

```r
world <- st_as_sf(world)
class(world)
```

```
## [1] "sf"         "data.frame"
```

```r
# First Big Map
ggplot(data = world) + geom_sf()
```

![](Maps_files/figure-html/btm-1.png)<!-- -->

```r
# Labels on the Big Map
ggplot(data = world) +
    geom_sf() +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))
```

![](Maps_files/figure-html/btm-2.png)<!-- -->

```r
# Turn The Map Green
ggplot(data = world) + 
    geom_sf(color = "black", fill = "lightgreen")
```

![](Maps_files/figure-html/btm-3.png)<!-- -->

```r
# Color by population
ggplot(data = world) +
    geom_sf(aes(fill = POP_EST)) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt")
```

![](Maps_files/figure-html/btm-4.png)<!-- -->

```r
# Pole Projection
ggplot(data = world) +
    geom_sf() +
    coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")
```

![](Maps_files/figure-html/btm-5.png)<!-- -->

```r
ggplot(data = world) +
    geom_sf() +
    coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)
```

![](Maps_files/figure-html/btm-6.png)<!-- -->


```r
# North Arrow and Scale
ggplot(data = world) +
    geom_sf() +
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "bl", which_north = "true", 
        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
        style = north_arrow_fancy_orienteering) +
    coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97))
```

![](Maps_files/figure-html/more-1.png)<!-- -->

```r
# Final Map
ggplot(data = world) +
    geom_sf(fill = "antiquewhite1") +
    geom_text(aes(LON, LAT, label = NAME), size = 4, hjust = "left",
        color = "darkblue", fontface = "bold", check_overlap = TRUE) +
    annotate(geom = "text", x = -90, y = 26, label = "Gulf of Mexico", 
        fontface = "italic", color = "grey22", size = 6) +
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "bl", which_north = "true", 
        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
        style = north_arrow_fancy_orienteering) +
    coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE) +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Map of the Gulf of Mexico and the Caribbean Sea") +
    theme(panel.grid.major = element_line(color = gray(.5),
        linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "aliceblue"))
```

![](Maps_files/figure-html/more-2.png)<!-- -->

