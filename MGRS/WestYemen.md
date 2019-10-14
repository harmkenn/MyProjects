---
title: "Western Yemen"
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

Grabbing the Background Map


```r
wy <- c(lon = 44, lat = 17)

# Get map at zoom level 5: map_5
map_5 <- get_map(wy, zoom = 5, scale = 1)

# Plot map at zoom level 5
ggmap(map_5)
```

![](WestYemen_files/figure-html/gbm-1.png)<!-- -->

```r
# Get map at zoom level 13: corvallis_map
wy_map <- get_map(wy, zoom = 8, scale = 1)

# Plot map at zoom level 13
ggmap(wy_map)
```

![](WestYemen_files/figure-html/gbm-2.png)<!-- -->

