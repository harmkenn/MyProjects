---
title: "Top IMDB Movies"
author: "Ken Harmon"
date: "2021 August 16"
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







## IMDB

https://www.imdb.com/search/title

https://www.imdb.com/search/title/?title_type=feature&user_rating=6.0,10.0&num_votes=20000,&certificates=US%3AG,US%3APG,US%3APG-13,US%3AR,US%3ANC-17&countries=us&languages=en&sort=user_rating,desc

## Scrape

https://rpubs.com/uky994/578161

https://rstudio-pubs-static.s3.amazonaws.com/299685_5ce4f9fb6fa3476e98fad355623a5f1e.html

### Ratings 6.0 to 10.0


```r
IMDB <- data.frame()

# https://www.imdb.com/search/title/?title_type=feature&user_rating=6.0,10.0&num_votes=20000,&certificates=US%3AG,US%3APG,US%3APG-13,US%3AR,US%3ANC-17&countries=us&languages=en&sort=user_rating,desc&start=1&ref_=adv_nxt 

  for (each_page in seq(from = 1, to = 3625, by=50)) {
      link = paste0("https://www.imdb.com/search/title/?title_type=feature&user_rating=6.0,10.0&num_votes=20000,&certificates=US%3AG,US%3APG,US%3APG-13,US%3AR,US%3ANC-17&countries=us&languages=en&sort=user_rating,desc&start=",each_page,"&ref_=adv_nxt")
      page <- read_html(link)
      RK <- page %>% html_nodes(".text-primary") %>% html_text()
      Title <- page %>% html_nodes(".lister-item-header a") %>% html_text()
      Year <- page %>% html_nodes(".text-muted.unbold") %>% html_text()
      Cert <- html_node(html_nodes(page,".lister-item-content"), ".certificate") %>% html_text()
      Runtime <- html_node(html_nodes(page,".lister-item-content"), ".runtime") %>% html_text()
      Genre <- html_node(html_nodes(page,".lister-item-content"), ".genre") %>% html_text()
      Rating <- html_node(html_nodes(page,".lister-item-content"), ".ratings-imdb-rating strong") %>% html_text()
      Meta <- html_node(html_nodes(page,".lister-item-content"), ".favorable") %>% html_text()
      Votes <- html_node(html_nodes(page,".lister-item-content"), ".sort-num_votes-visible span:nth-child(2)") %>% html_text()
      Gross <- html_node(html_nodes(page,".lister-item-content"), ".ghost~ .text-muted+ span") %>% html_text()
      Summary <- html_node(html_nodes(page,".lister-item-content"), ".ratings-bar+ .text-muted") %>% html_text()
      Director <- html_node(html_nodes(page,".lister-item-content"), ".text-muted+ p a:nth-child(1)") %>% html_text()
      Star1 <- html_node(html_nodes(page,".lister-item-content"), ".ghost+ a") %>% html_text()
      print(each_page)
      IMDB <- rbind(IMDB,data.frame(RK,Title,Year,Runtime,Cert,Genre,Rating,Meta,Votes,Gross,Summary,Director,Star1))
  }
```

```
## [1] 1
## [1] 51
## [1] 101
## [1] 151
## [1] 201
## [1] 251
## [1] 301
## [1] 351
## [1] 401
## [1] 451
## [1] 501
## [1] 551
## [1] 601
## [1] 651
## [1] 701
## [1] 751
## [1] 801
## [1] 851
## [1] 901
## [1] 951
## [1] 1001
## [1] 1051
## [1] 1101
## [1] 1151
## [1] 1201
## [1] 1251
## [1] 1301
## [1] 1351
## [1] 1401
## [1] 1451
## [1] 1501
## [1] 1551
## [1] 1601
## [1] 1651
## [1] 1701
## [1] 1751
## [1] 1801
## [1] 1851
## [1] 1901
## [1] 1951
## [1] 2001
## [1] 2051
## [1] 2101
## [1] 2151
## [1] 2201
## [1] 2251
## [1] 2301
## [1] 2351
## [1] 2401
## [1] 2451
## [1] 2501
## [1] 2551
## [1] 2601
## [1] 2651
## [1] 2701
## [1] 2751
## [1] 2801
## [1] 2851
## [1] 2901
## [1] 2951
## [1] 3001
## [1] 3051
## [1] 3101
## [1] 3151
## [1] 3201
## [1] 3251
## [1] 3301
## [1] 3351
## [1] 3401
## [1] 3451
## [1] 3501
## [1] 3551
## [1] 3601
```

```r
datatable(IMDB, rownames = FALSE, extensions = 'Responsive')
```

```{=html}
<div id="htmlwidget-64820e523f737fd964f3" style="width:100%;height:auto;" class="datatables html-widget"></div>
```

```r
save(IMDB,file = "IMDB.rda")
#write.csv(ESPN_BPI,"ESPN_BPI.csv")
```

## Clean up


```r
# Get the dot off the rank
IMDB$RK <- as.integer(rownames(IMDB))

# Get rid of the parentheses of the year
IMDB$Year <- gsub("\\(I)","",IMDB$Year)
IMDB$Year <- gsub("\\(","",IMDB$Year)
IMDB$Year <- gsub("\\)","",IMDB$Year)
IMDB$Year <- as.integer(IMDB$Year)

# Drop min off of runtime

IMDB$Runtime <- gsub(" min","",IMDB$Runtime)
IMDB$Runtime <- as.integer(IMDB$Runtime)

IMDB$Rating <- as.numeric(IMDB$Rating)
IMDB$Meta <- as.numeric(IMDB$Meta)

IMDB$Votes <- as.integer(gsub("\\,","",IMDB$Votes))

IMDB$Gross <- gsub("\\$","",IMDB$Gross)
IMDB$Gross <- gsub("M","",IMDB$Gross)
IMDB$Gross <- as.numeric(IMDB$Gross)*1000000
save(IMDB,file = "IMDB3.rda")
```


```r
boxplot(IMDB$Rating)
```

![](Top-IMDB--v1.0-_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
pacman::p_load(plotly)
load("IMDB3625.rda")
p <- IMDB %>% filter(Rating>=8) %>%
  ggplot( aes(Gross, Meta, size = Rating, color=Cert,text=Title)) +
  geom_point() + theme_bw() 

ggplotly(p)
```

```{=html}
<div id="htmlwidget-ac1e8be6666a1c7f1df1" style="width:1152px;height:576px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-ac1e8be6666a1c7f1df1">{"x":{"data":[{"x":[null,null],"y":[96,73],"text":["Gross:        NA<br />Meta:  96<br />Rating: 8.0<br />Cert: Approved<br />Rosemary's Baby","Gross:        NA<br />Meta:  73<br />Rating: 8.0<br />Cert: Approved<br />Rope"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","opacity":1,"size":3.77952755905512,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)"}},"hoveron":"points","name":"Approved","legendgroup":"Approved","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[422780000,160000,20000,223810000,290000,191800000,56950000,8820000,415000000,null,380840000,289920000,74700000,206450000,6200000,217350000,218970000,80500000,33400000,163210000],"y":[88,96,99,95,null,95,84,99,92,null,90,79,90,96,86,86,95,67,79,63],"text":["Gross: 422780000<br />Meta:  88<br />Rating: 8.5<br />Cert: G<br />The Lion King","Gross:    160000<br />Meta:  96<br />Rating: 8.5<br />Cert: G<br />Modern Times","Gross:     20000<br />Meta:  99<br />Rating: 8.5<br />Cert: G<br />City Lights","Gross: 223810000<br />Meta:  95<br />Rating: 8.4<br />Cert: G<br />WALL·E","Gross:    290000<br />Meta:  NA<br />Rating: 8.4<br />Cert: G<br />The Great Dictator","Gross: 191800000<br />Meta:  95<br />Rating: 8.3<br />Cert: G<br />Toy Story","Gross:  56950000<br />Meta:  84<br />Rating: 8.3<br />Cert: G<br />2001: A Space Odyssey","Gross:   8820000<br />Meta:  99<br />Rating: 8.3<br />Cert: G<br />Singin' in the Rain","Gross: 415000000<br />Meta:  92<br />Rating: 8.2<br />Cert: G<br />Toy Story 3","Gross:        NA<br />Meta:  NA<br />Rating: 8.1<br />Cert: G<br />Hachi: A Dog's Tale","Gross: 380840000<br />Meta:  90<br />Rating: 8.1<br />Cert: G<br />Finding Nemo","Gross: 289920000<br />Meta:  79<br />Rating: 8.1<br />Cert: G<br />Monsters, Inc.","Gross:  74700000<br />Meta:  90<br />Rating: 8.1<br />Cert: G<br />Ben-Hur","Gross: 206450000<br />Meta:  96<br />Rating: 8.0<br />Cert: G<br />Ratatouille","Gross:   6200000<br />Meta:  86<br />Rating: 8.0<br />Cert: G<br />The Straight Story","Gross: 217350000<br />Meta:  86<br />Rating: 8.0<br />Cert: G<br />Aladdin","Gross: 218970000<br />Meta:  95<br />Rating: 8.0<br />Cert: G<br />Beauty and the Beast","Gross:  80500000<br />Meta:  67<br />Rating: 8.0<br />Cert: G<br />Fiddler on the Roof","Gross:  33400000<br />Meta:  79<br />Rating: 8.0<br />Cert: G<br />Planet of the Apes","Gross: 163210000<br />Meta:  63<br />Rating: 8.0<br />Cert: G<br />The Sound of Music"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(183,159,0,1)","opacity":1,"size":[15.4993450005551,15.4993450005551,15.4993450005551,14.2620509522878,14.2620509522878,12.8576591133593,12.8576591133593,12.8576591133593,11.1917909343565,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512],"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(183,159,0,1)"}},"hoveron":"points","name":"G","legendgroup":"G","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[198680000,null],"y":[97,90],"text":["Gross: 198680000<br />Meta:  97<br />Rating: 8.1<br />Cert: Passed<br />Gone with the Wind","Gross:        NA<br />Meta:  90<br />Rating: 8.1<br />Cert: Passed<br />The Circus"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,186,56,1)","opacity":1,"size":9.02078925567145,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,186,56,1)"}},"hoveron":"points","name":"Passed","legendgroup":"Passed","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[290480000,322740000,null,210610000,36760000,1020000,190240000,209730000,248160000,280000,309130000,159600000,3200000,1590000,null,293000000,10000,null,356460000,217580000,125620000,95860000,30860000,null,117240000,null,30930000,44910000,null,341270000,132420000,261440000,23160000,75080000,70910000,52770000,30180000,39200000,70600000,260000000,86300000,4080000,102310000,104950000,9450000,8000000,7630000,2080000],"y":[82,90,89,87,100,100,87,81,85,97,null,83,100,100,65,88,75,83,94,75,90,79,77,78,70,89,77,87,69,78,66,90,85,82,72,79,83,92,84,87,80,null,66,83,89,97,88,92],"text":["Gross: 290480000<br />Meta:  82<br />Rating: 8.7<br />Cert: PG<br />Star Wars: Episode V - The Empire Strikes Back","Gross: 322740000<br />Meta:  90<br />Rating: 8.6<br />Cert: PG<br />Star Wars: Episode IV - A New Hope","Gross:        NA<br />Meta:  89<br />Rating: 8.6<br />Cert: PG<br />It's a Wonderful Life","Gross: 210610000<br />Meta:  87<br />Rating: 8.5<br />Cert: PG<br />Back to the Future","Gross:  36760000<br />Meta: 100<br />Rating: 8.5<br />Cert: PG<br />Rear Window","Gross:   1020000<br />Meta: 100<br />Rating: 8.5<br />Cert: PG<br />Casablanca","Gross: 190240000<br />Meta:  87<br />Rating: 8.4<br />Cert: PG<br />Spider-Man: Into the Spider-Verse","Gross: 209730000<br />Meta:  81<br />Rating: 8.4<br />Cert: PG<br />Coco","Gross: 248160000<br />Meta:  85<br />Rating: 8.4<br />Cert: PG<br />Indiana Jones and the Raiders of the Lost Ark","Gross:    280000<br />Meta:  97<br />Rating: 8.4<br />Cert: PG<br />Dr. Strangelove or: How I Learned to Stop Worrying and Love the Bomb","Gross: 309130000<br />Meta:  NA<br />Rating: 8.3<br />Cert: PG<br />Star Wars: Episode VI - Return of the Jedi","Gross: 159600000<br />Meta:  83<br />Rating: 8.3<br />Cert: PG<br />The Sting","Gross:   3200000<br />Meta: 100<br />Rating: 8.3<br />Cert: PG<br />Vertigo","Gross:   1590000<br />Meta: 100<br />Rating: 8.3<br />Cert: PG<br />Citizen Kane","Gross:        NA<br />Meta:  65<br />Rating: 8.2<br />Cert: PG<br />Klaus","Gross: 293000000<br />Meta:  88<br />Rating: 8.2<br />Cert: PG<br />Up","Gross:     10000<br />Meta:  75<br />Rating: 8.2<br />Cert: PG<br />Dial M for Murder","Gross:        NA<br />Meta:  83<br />Rating: 8.1<br />Cert: PG<br />Soul","Gross: 356460000<br />Meta:  94<br />Rating: 8.1<br />Cert: PG<br />Inside Out","Gross: 217580000<br />Meta:  75<br />Rating: 8.1<br />Cert: PG<br />How to Train Your Dragon","Gross: 125620000<br />Meta:  90<br />Rating: 8.1<br />Cert: PG<br />The Truman Show","Gross:  95860000<br />Meta:  79<br />Rating: 8.1<br />Cert: PG<br />Dead Poets Society","Gross:  30860000<br />Meta:  77<br />Rating: 8.1<br />Cert: PG<br />The Princess Bride","Gross:        NA<br />Meta:  78<br />Rating: 8.1<br />Cert: PG<br />The Elephant Man","Gross: 117240000<br />Meta:  70<br />Rating: 8.1<br />Cert: PG<br />Rocky","Gross:        NA<br />Meta:  89<br />Rating: 8.1<br />Cert: PG<br />Barry Lyndon","Gross:  30930000<br />Meta:  77<br />Rating: 8.1<br />Cert: PG<br />Paper Moon","Gross:  44910000<br />Meta:  87<br />Rating: 8.1<br />Cert: PG<br />The Bridge on the River Kwai","Gross:        NA<br />Meta:  69<br />Rating: 8.0<br />Cert: PG<br />Togo","Gross: 341270000<br />Meta:  78<br />Rating: 8.0<br />Cert: PG<br />Zootopia","Gross: 132420000<br />Meta:  66<br />Rating: 8.0<br />Cert: PG<br />Wonder","Gross: 261440000<br />Meta:  90<br />Rating: 8.0<br />Cert: PG<br />The Incredibles","Gross:  23160000<br />Meta:  85<br />Rating: 8.0<br />Cert: PG<br />The Iron Giant","Gross:  75080000<br />Meta:  82<br />Rating: 8.0<br />Cert: PG<br />The Nightmare Before Christmas","Gross:  70910000<br />Meta:  72<br />Rating: 8.0<br />Cert: PG<br />Groundhog Day","Gross:  52770000<br />Meta:  79<br />Rating: 8.0<br />Cert: PG<br />Gandhi","Gross:  30180000<br />Meta:  83<br />Rating: 8.0<br />Cert: PG<br />Being There","Gross:  39200000<br />Meta:  92<br />Rating: 8.0<br />Cert: PG<br />Annie Hall","Gross:  70600000<br />Meta:  84<br />Rating: 8.0<br />Cert: PG<br />All the President's Men","Gross: 260000000<br />Meta:  87<br />Rating: 8.0<br />Cert: PG<br />Jaws","Gross:  86300000<br />Meta:  80<br />Rating: 8.0<br />Cert: PG<br />Young Frankenstein","Gross:   4080000<br />Meta:  NA<br />Rating: 8.0<br />Cert: PG<br />Sleuth","Gross: 102310000<br />Meta:  66<br />Rating: 8.0<br />Cert: PG<br />Butch Cassidy and the Sundance Kid","Gross: 104950000<br />Meta:  83<br />Rating: 8.0<br />Cert: PG<br />The Graduate","Gross:   9450000<br />Meta:  89<br />Rating: 8.0<br />Cert: PG<br />High Noon","Gross:   8000000<br />Meta:  97<br />Rating: 8.0<br />Cert: PG<br />A Streetcar Named Desire","Gross:   7630000<br />Meta:  88<br />Rating: 8.0<br />Cert: PG<br />Strangers on a Train","Gross:   2080000<br />Meta:  92<br />Rating: 8.0<br />Cert: PG<br />The Wizard of Oz"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,191,196,1)","opacity":1,"size":[17.6466025645104,16.6179443241592,16.6179443241592,15.4993450005551,15.4993450005551,15.4993450005551,14.2620509522878,14.2620509522878,14.2620509522878,14.2620509522878,12.8576591133593,12.8576591133593,12.8576591133593,12.8576591133593,11.1917909343565,11.1917909343565,11.1917909343565,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512],"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,191,196,1)"}},"hoveron":"points","name":"PG","legendgroup":"PG","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[534860000,377850000,292580000,315540000,330250000,342550000,188020000,53090000,5320000,null,858370000,678820000,448140000,85080000,206850000,170740000,197170000,117620000,13660000,381010000,100490000,23530000,164620000,293510000,402450000,151100000,51740000,228430000,91130000,333180000,17740000,169710000,4020000,623280000,4450000,163570000,227470000,167450000,61650000,305410000,66260000,184210000,111720000,2240000],"y":[84,94,74,92,82,87,74,66,80,90,78,68,78,69,70,72,65,81,71,85,86,79,75,64,68,94,69,80,73,76,67,62,null,69,90,64,85,80,69,63,null,72,69,99],"text":["Gross: 534860000<br />Meta:  84<br />Rating: 9.0<br />Cert: PG-13<br />The Dark Knight","Gross: 377850000<br />Meta:  94<br />Rating: 8.9<br />Cert: PG-13<br />The Lord of the Rings: The Return of the King","Gross: 292580000<br />Meta:  74<br />Rating: 8.8<br />Cert: PG-13<br />Inception","Gross: 315540000<br />Meta:  92<br />Rating: 8.8<br />Cert: PG-13<br />The Lord of the Rings: The Fellowship of the Ring","Gross: 330250000<br />Meta:  82<br />Rating: 8.8<br />Cert: PG-13<br />Forrest Gump","Gross: 342550000<br />Meta:  87<br />Rating: 8.7<br />Cert: PG-13<br />The Lord of the Rings: The Two Towers","Gross: 188020000<br />Meta:  74<br />Rating: 8.6<br />Cert: PG-13<br />Interstellar","Gross:  53090000<br />Meta:  66<br />Rating: 8.5<br />Cert: PG-13<br />The Prestige","Gross:   5320000<br />Meta:  80<br />Rating: 8.5<br />Cert: PG-13<br />Once Upon a Time in the West","Gross:        NA<br />Meta:  90<br />Rating: 8.4<br />Cert: PG-13<br />Hamilton","Gross: 858370000<br />Meta:  78<br />Rating: 8.4<br />Cert: PG-13<br />Avengers: Endgame","Gross: 678820000<br />Meta:  68<br />Rating: 8.4<br />Cert: PG-13<br />Avengers: Infinity War","Gross: 448140000<br />Meta:  78<br />Rating: 8.4<br />Cert: PG-13<br />The Dark Knight Rises","Gross:  85080000<br />Meta:  69<br />Rating: 8.2<br />Cert: PG-13<br />Green Book","Gross: 206850000<br />Meta:  70<br />Rating: 8.2<br />Cert: PG-13<br />Batman Begins","Gross: 170740000<br />Meta:  72<br />Rating: 8.2<br />Cert: PG-13<br />A Beautiful Mind","Gross: 197170000<br />Meta:  65<br />Rating: 8.2<br />Cert: PG-13<br />Indiana Jones and the Last Crusade","Gross: 117620000<br />Meta:  81<br />Rating: 8.1<br />Cert: PG-13<br />Ford v Ferrari","Gross:  13660000<br />Meta:  71<br />Rating: 8.1<br />Cert: PG-13<br />Warrior","Gross: 381010000<br />Meta:  85<br />Rating: 8.1<br />Cert: PG-13<br />Harry Potter and the Deathly Hallows: Part 2","Gross: 100490000<br />Meta:  86<br />Rating: 8.1<br />Cert: PG-13<br />Million Dollar Baby","Gross:  23530000<br />Meta:  79<br />Rating: 8.1<br />Cert: PG-13<br />Hotel Rwanda","Gross: 164620000<br />Meta:  75<br />Rating: 8.1<br />Cert: PG-13<br />Catch Me If You Can","Gross: 293510000<br />Meta:  64<br />Rating: 8.1<br />Cert: PG-13<br />The Sixth Sense","Gross: 402450000<br />Meta:  68<br />Rating: 8.1<br />Cert: PG-13<br />Jurassic Park","Gross: 151100000<br />Meta:  94<br />Rating: 8.0<br />Cert: PG-13<br />La La Land","Gross:  51740000<br />Meta:  69<br />Rating: 8.0<br />Cert: PG-13<br />Lion","Gross: 228430000<br />Meta:  80<br />Rating: 8.0<br />Cert: PG-13<br />The Martian","Gross:  91130000<br />Meta:  73<br />Rating: 8.0<br />Cert: PG-13<br />The Imitation Game","Gross: 333180000<br />Meta:  76<br />Rating: 8.0<br />Cert: PG-13<br />Guardians of the Galaxy","Gross:  17740000<br />Meta:  67<br />Rating: 8.0<br />Cert: PG-13<br />The Perks of Being a Wallflower","Gross: 169710000<br />Meta:  62<br />Rating: 8.0<br />Cert: PG-13<br />The Help","Gross:   4020000<br />Meta:  NA<br />Rating: 8.0<br />Cert: PG-13<br />My Name Is Khan","Gross: 623280000<br />Meta:  69<br />Rating: 8.0<br />Cert: PG-13<br />The Avengers","Gross:   4450000<br />Meta:  90<br />Rating: 8.0<br />Cert: PG-13<br />Persepolis","Gross: 163570000<br />Meta:  64<br />Rating: 8.0<br />Cert: PG-13<br />The Pursuit of Happyness","Gross: 227470000<br />Meta:  85<br />Rating: 8.0<br />Cert: PG-13<br />The Bourne Ultimatum","Gross: 167450000<br />Meta:  80<br />Rating: 8.0<br />Cert: PG-13<br />Casino Royale","Gross:  61650000<br />Meta:  69<br />Rating: 8.0<br />Cert: PG-13<br />Cinderella Man","Gross: 305410000<br />Meta:  63<br />Rating: 8.0<br />Cert: PG-13<br />Pirates of the Caribbean: The Curse of the Black Pearl","Gross:  66260000<br />Meta:  NA<br />Rating: 8.0<br />Cert: PG-13<br />Big Fish","Gross: 184210000<br />Meta:  72<br />Rating: 8.0<br />Cert: PG-13<br />Dances with Wolves","Gross: 111720000<br />Meta:  69<br />Rating: 8.0<br />Cert: PG-13<br />Doctor Zhivago","Gross:   2240000<br />Meta:  99<br />Rating: 8.0<br />Cert: PG-13<br />Touch of Evil"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(97,156,255,1)","opacity":1,"size":[20.3538523333612,19.5033126489041,18.604054309658,18.604054309658,18.604054309658,17.6466025645104,16.6179443241592,15.4993450005551,15.4993450005551,14.2620509522878,14.2620509522878,14.2620509522878,14.2620509522878,11.1917909343565,11.1917909343565,11.1917909343565,11.1917909343565,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512],"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(97,156,255,1)"}},"hoveron":"points","name":"PG-13","legendgroup":"PG-13","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[28340000,134970000,57300000,107930000,96900000,37030000,171480000,46840000,112000000,216540000,136800000,100130000,130740000,13090000,132380000,32570000,187710000,6720000,23340000,19500000,204840000,32000000,335450000,162810000,25540000,5320000,44020000,83470000,78900000,159230000,120540000,34400000,30330000,3640000,130100000,138430000,75600000,2830000,46360000,85160000,51970000,45600000,28260000,6210000,128010000,116900000,40220000,64620000,67440000,42440000,101160000,83010000,23380000,13340000,8490000,54510000,226280000,14680000,59100000,167770000,67210000,56670000,26950000,45060000,61000000,154060000,null,148100000,18350000,74280000,70510000,5820000,70100000,17500000,24610000,5540000,52290000,138530000,2180000,13780000,32870000,48980000,null,1010000,2630000,92050000,25570000,183640000,138800000,363070000,141320000,106950000,10000,27300000,57370000,74100000,66210000,1480000,22460000,24480000,57140000,4500000,63900000,70410000,27550000,178800000,38400000,50000000,53270000,232910000,29130000],"y":[80,100,90,94,94,66,73,90,84,91,61,65,85,88,85,85,67,62,77,64,75,97,null,81,80,null,66,94,89,78,69,89,null,68,84,70,68,79,76,84,88,65,94,77,63,75,93,90,76,73,85,72,89,88,92,88,77,86,88,79,71,96,74,93,70,90,null,72,73,91,62,90,69,71,85,77,75,92,78,null,84,86,83,82,71,81,90,76,88,65,84,79,null,80,64,74,83,88,77,84,74,null,null,72,93,65,84,86,null,81,93],"text":["Gross:  28340000<br />Meta:  80<br />Rating: 9.3<br />Cert: R<br />The Shawshank Redemption","Gross: 134970000<br />Meta: 100<br />Rating: 9.2<br />Cert: R<br />The Godfather","Gross:  57300000<br />Meta:  90<br />Rating: 9.0<br />Cert: R<br />The Godfather: Part II","Gross: 107930000<br />Meta:  94<br />Rating: 8.9<br />Cert: R<br />Pulp Fiction","Gross:  96900000<br />Meta:  94<br />Rating: 8.9<br />Cert: R<br />Schindler's List","Gross:  37030000<br />Meta:  66<br />Rating: 8.8<br />Cert: R<br />Fight Club","Gross: 171480000<br />Meta:  73<br />Rating: 8.7<br />Cert: R<br />The Matrix","Gross:  46840000<br />Meta:  90<br />Rating: 8.7<br />Cert: R<br />Goodfellas","Gross: 112000000<br />Meta:  84<br />Rating: 8.7<br />Cert: R<br />One Flew Over the Cuckoo's Nest","Gross: 216540000<br />Meta:  91<br />Rating: 8.6<br />Cert: R<br />Saving Private Ryan","Gross: 136800000<br />Meta:  61<br />Rating: 8.6<br />Cert: R<br />The Green Mile","Gross: 100130000<br />Meta:  65<br />Rating: 8.6<br />Cert: R<br />Se7en","Gross: 130740000<br />Meta:  85<br />Rating: 8.6<br />Cert: R<br />The Silence of the Lambs","Gross:  13090000<br />Meta:  88<br />Rating: 8.5<br />Cert: R<br />Whiplash","Gross: 132380000<br />Meta:  85<br />Rating: 8.5<br />Cert: R<br />The Departed","Gross:  32570000<br />Meta:  85<br />Rating: 8.5<br />Cert: R<br />The Pianist","Gross: 187710000<br />Meta:  67<br />Rating: 8.5<br />Cert: R<br />Gladiator","Gross:   6720000<br />Meta:  62<br />Rating: 8.5<br />Cert: R<br />American History X","Gross:  23340000<br />Meta:  77<br />Rating: 8.5<br />Cert: R<br />The Usual Suspects","Gross:  19500000<br />Meta:  64<br />Rating: 8.5<br />Cert: R<br />Léon: The Professional","Gross: 204840000<br />Meta:  75<br />Rating: 8.5<br />Cert: R<br />Terminator 2: Judgment Day","Gross:  32000000<br />Meta:  97<br />Rating: 8.5<br />Cert: R<br />Psycho","Gross: 335450000<br />Meta:  NA<br />Rating: 8.4<br />Cert: R<br />Joker","Gross: 162810000<br />Meta:  81<br />Rating: 8.4<br />Cert: R<br />Django Unchained","Gross:  25540000<br />Meta:  80<br />Rating: 8.4<br />Cert: R<br />Memento","Gross:   5320000<br />Meta:  NA<br />Rating: 8.4<br />Cert: R<br />Once Upon a Time in America","Gross:  44020000<br />Meta:  66<br />Rating: 8.4<br />Cert: R<br />The Shining","Gross:  83470000<br />Meta:  94<br />Rating: 8.4<br />Cert: R<br />Apocalypse Now","Gross:  78900000<br />Meta:  89<br />Rating: 8.4<br />Cert: R<br />Alien","Gross: 159230000<br />Meta:  78<br />Rating: 8.3<br />Cert: R<br />1917","Gross: 120540000<br />Meta:  69<br />Rating: 8.3<br />Cert: R<br />Inglourious Basterds","Gross:  34400000<br />Meta:  89<br />Rating: 8.3<br />Cert: R<br />Eternal Sunshine of the Spotless Mind","Gross:  30330000<br />Meta:  NA<br />Rating: 8.3<br />Cert: R<br />Snatch","Gross:   3640000<br />Meta:  68<br />Rating: 8.3<br />Cert: R<br />Requiem for a Dream","Gross: 130100000<br />Meta:  84<br />Rating: 8.3<br />Cert: R<br />American Beauty","Gross: 138430000<br />Meta:  70<br />Rating: 8.3<br />Cert: R<br />Good Will Hunting","Gross:  75600000<br />Meta:  68<br />Rating: 8.3<br />Cert: R<br />Braveheart","Gross:   2830000<br />Meta:  79<br />Rating: 8.3<br />Cert: R<br />Reservoir Dogs","Gross:  46360000<br />Meta:  76<br />Rating: 8.3<br />Cert: R<br />Full Metal Jacket","Gross:  85160000<br />Meta:  84<br />Rating: 8.3<br />Cert: R<br />Aliens","Gross:  51970000<br />Meta:  88<br />Rating: 8.3<br />Cert: R<br />Amadeus","Gross:  45600000<br />Meta:  65<br />Rating: 8.3<br />Cert: R<br />Scarface","Gross:  28260000<br />Meta:  94<br />Rating: 8.3<br />Cert: R<br />Taxi Driver","Gross:   6210000<br />Meta:  77<br />Rating: 8.3<br />Cert: R<br />A Clockwork Orange","Gross: 128010000<br />Meta:  63<br />Rating: 8.2<br />Cert: R<br />Shutter Island","Gross: 116900000<br />Meta:  75<br />Rating: 8.2<br />Cert: R<br />The Wolf of Wall Street","Gross:  40220000<br />Meta:  93<br />Rating: 8.2<br />Cert: R<br />There Will Be Blood","Gross:  64620000<br />Meta:  90<br />Rating: 8.2<br />Cert: R<br />L.A. Confidential","Gross:  67440000<br />Meta:  76<br />Rating: 8.2<br />Cert: R<br />Heat","Gross:  42440000<br />Meta:  73<br />Rating: 8.2<br />Cert: R<br />Casino","Gross: 101160000<br />Meta:  85<br />Rating: 8.2<br />Cert: R<br />Unforgiven","Gross:  83010000<br />Meta:  72<br />Rating: 8.2<br />Cert: R<br />Die Hard","Gross:  23380000<br />Meta:  89<br />Rating: 8.2<br />Cert: R<br />Raging Bull","Gross:  13340000<br />Meta:  88<br />Rating: 8.2<br />Cert: R<br />A Woman Under the Influence","Gross:   8490000<br />Meta:  92<br />Rating: 8.2<br />Cert: R<br />Chinatown","Gross:  54510000<br />Meta:  88<br />Rating: 8.1<br />Cert: R<br />Three Billboards Outside Ebbing, Missouri","Gross: 226280000<br />Meta:  77<br />Rating: 8.1<br />Cert: R<br />Logan","Gross:  14680000<br />Meta:  86<br />Rating: 8.1<br />Cert: R<br />Room","Gross:  59100000<br />Meta:  88<br />Rating: 8.1<br />Cert: R<br />The Grand Budapest Hotel","Gross: 167770000<br />Meta:  79<br />Rating: 8.1<br />Cert: R<br />Gone Girl","Gross:  67210000<br />Meta:  71<br />Rating: 8.1<br />Cert: R<br />Hacksaw Ridge","Gross:  56670000<br />Meta:  96<br />Rating: 8.1<br />Cert: R<br />12 Years a Slave","Gross:  26950000<br />Meta:  74<br />Rating: 8.1<br />Cert: R<br />Rush","Gross:  45060000<br />Meta:  93<br />Rating: 8.1<br />Cert: R<br />Spotlight","Gross:  61000000<br />Meta:  70<br />Rating: 8.1<br />Cert: R<br />Prisoners","Gross: 154060000<br />Meta:  90<br />Rating: 8.1<br />Cert: R<br />Mad Max: Fury Road","Gross:        NA<br />Meta:  NA<br />Rating: 8.1<br />Cert: R<br />Zack Snyder's Justice League","Gross: 148100000<br />Meta:  72<br />Rating: 8.1<br />Cert: R<br />Gran Torino","Gross:  18350000<br />Meta:  73<br />Rating: 8.1<br />Cert: R<br />Into the Wild","Gross:  74280000<br />Meta:  91<br />Rating: 8.1<br />Cert: R<br />No Country for Old Men","Gross:  70510000<br />Meta:  62<br />Rating: 8.1<br />Cert: R<br />V for Vendetta","Gross:   5820000<br />Meta:  90<br />Rating: 8.1<br />Cert: R<br />Before Sunset","Gross:  70100000<br />Meta:  69<br />Rating: 8.1<br />Cert: R<br />Kill Bill: Vol. 1","Gross:  17500000<br />Meta:  71<br />Rating: 8.1<br />Cert: R<br />The Big Lebowski","Gross:  24610000<br />Meta:  85<br />Rating: 8.1<br />Cert: R<br />Fargo","Gross:   5540000<br />Meta:  77<br />Rating: 8.1<br />Cert: R<br />Before Sunrise","Gross:  52290000<br />Meta:  75<br />Rating: 8.1<br />Cert: R<br />Stand by Me","Gross: 138530000<br />Meta:  92<br />Rating: 8.1<br />Cert: R<br />Platoon","Gross:   2180000<br />Meta:  78<br />Rating: 8.1<br />Cert: R<br />Paris, Texas","Gross:  13780000<br />Meta:  NA<br />Rating: 8.1<br />Cert: R<br />The Thing","Gross:  32870000<br />Meta:  84<br />Rating: 8.1<br />Cert: R<br />Blade Runner","Gross:  48980000<br />Meta:  86<br />Rating: 8.1<br />Cert: R<br />The Deer Hunter","Gross:        NA<br />Meta:  83<br />Rating: 8.1<br />Cert: R<br />Network","Gross:   1010000<br />Meta:  82<br />Rating: 8.0<br />Cert: R<br />Short Term 12","Gross:   2630000<br />Meta:  71<br />Rating: 8.0<br />Cert: R<br />The Raid 2","Gross:  92050000<br />Meta:  81<br />Rating: 8.0<br />Cert: R<br />Blade Runner 2049","Gross:  25570000<br />Meta:  90<br />Rating: 8.0<br />Cert: R<br />Her","Gross: 183640000<br />Meta:  76<br />Rating: 8.0<br />Cert: R<br />The Revenant","Gross: 138800000<br />Meta:  88<br />Rating: 8.0<br />Cert: R<br />The King's Speech","Gross: 363070000<br />Meta:  65<br />Rating: 8.0<br />Cert: R<br />Deadpool","Gross: 141320000<br />Meta:  84<br />Rating: 8.0<br />Cert: R<br />Slumdog Millionaire","Gross: 106950000<br />Meta:  79<br />Rating: 8.0<br />Cert: R<br />Black Swan","Gross:     10000<br />Meta:  NA<br />Rating: 8.0<br />Cert: R<br />Elite Squad","Gross:  27300000<br />Meta:  80<br />Rating: 8.0<br />Cert: R<br />Dallas Buyers Club","Gross:  57370000<br />Meta:  64<br />Rating: 8.0<br />Cert: R<br />Blood Diamond","Gross:  74100000<br />Meta:  74<br />Rating: 8.0<br />Cert: R<br />Sin City","Gross:  66210000<br />Meta:  83<br />Rating: 8.0<br />Cert: R<br />Kill Bill: Vol. 2","Gross:   1480000<br />Meta:  88<br />Rating: 8.0<br />Cert: R<br />Donnie Darko","Gross:  22460000<br />Meta:  77<br />Rating: 8.0<br />Cert: R<br />Magnolia","Gross:  24480000<br />Meta:  84<br />Rating: 8.0<br />Cert: R<br />Sling Blade","Gross:  57140000<br />Meta:  74<br />Rating: 8.0<br />Cert: R<br />12 Monkeys","Gross:   4500000<br />Meta:  NA<br />Rating: 8.0<br />Cert: R<br />Blood In, Blood Out","Gross:  63900000<br />Meta:  NA<br />Rating: 8.0<br />Cert: R<br />Scent of a Woman","Gross:  70410000<br />Meta:  72<br />Rating: 8.0<br />Cert: R<br />JFK","Gross:  27550000<br />Meta:  93<br />Rating: 8.0<br />Cert: R<br />Do the Right Thing","Gross: 178800000<br />Meta:  65<br />Rating: 8.0<br />Cert: R<br />Rain Man","Gross:  38400000<br />Meta:  84<br />Rating: 8.0<br />Cert: R<br />The Terminator","Gross:  50000000<br />Meta:  86<br />Rating: 8.0<br />Cert: R<br />Dog Day Afternoon","Gross:  53270000<br />Meta:  NA<br />Rating: 8.0<br />Cert: R<br />Papillon","Gross: 232910000<br />Meta:  81<br />Rating: 8.0<br />Cert: R<br />The Exorcist","Gross:  29130000<br />Meta:  93<br />Rating: 8.0<br />Cert: R<br />The Last Picture Show"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(245,100,227,1)","opacity":1,"size":[22.6771653543307,21.9357906676634,20.3538523333612,19.5033126489041,19.5033126489041,18.604054309658,17.6466025645104,17.6466025645104,17.6466025645104,16.6179443241592,16.6179443241592,16.6179443241592,16.6179443241592,15.4993450005551,15.4993450005551,15.4993450005551,15.4993450005551,15.4993450005551,15.4993450005551,15.4993450005551,15.4993450005551,15.4993450005551,14.2620509522878,14.2620509522878,14.2620509522878,14.2620509522878,14.2620509522878,14.2620509522878,14.2620509522878,12.8576591133593,12.8576591133593,12.8576591133593,12.8576591133593,12.8576591133593,12.8576591133593,12.8576591133593,12.8576591133593,12.8576591133593,12.8576591133593,12.8576591133593,12.8576591133593,12.8576591133593,12.8576591133593,12.8576591133593,11.1917909343565,11.1917909343565,11.1917909343565,11.1917909343565,11.1917909343565,11.1917909343565,11.1917909343565,11.1917909343565,11.1917909343565,11.1917909343565,11.1917909343565,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,9.02078925567145,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512,3.77952755905512],"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(245,100,227,1)"}},"hoveron":"points","name":"R","legendgroup":"R","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":30.6118721461187,"r":7.30593607305936,"b":44.5662100456621,"l":43.1050228310502},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-42908000,901288000],"tickmode":"array","ticktext":["0.0e+00","2.5e+08","5.0e+08","7.5e+08"],"tickvals":[0,250000000,500000000,750000000],"categoryorder":"array","categoryarray":["0.0e+00","2.5e+08","5.0e+08","7.5e+08"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"Gross","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[59.05,101.95],"tickmode":"array","ticktext":["60","70","80","90","100"],"tickvals":[60,70,80,90,100],"categoryorder":"array","categoryarray":["60","70","80","90","100"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Meta","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":0.891732283464567},"annotations":[{"text":"Rating<br />Cert","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"e9873ac3c76":{"x":{},"y":{},"size":{},"colour":{},"text":{},"type":"scatter"}},"cur_data":"e9873ac3c76","visdat":{"e9873ac3c76":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```
