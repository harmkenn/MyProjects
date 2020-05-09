---
title: "Pick the Next Etrade Mutual Fund"
author: "Ken Harmon"
date: "2020 April 24"
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







###ETRADE All

```r
EMF <- load_prices(c("AKREX","AMAGX","AMANX","BGAFX","BGSAX","BMGAX","BPTRX","DXQLX","DXSLX","ETGLX","FKDNX","FNCMX","GOLDX","INPIX","JAGTX","LDVAX","MFEGX","NBGEX","NNLEX","NNTWX","OBCHX","PGRTX","PGTAX","PRMTX","PYVAX","PYVLX","RMQHX","ROGSX","RYOCX","RYVYX","SEEKX","SFLNX","SMPIX","SVAAX","SWPPX","TEPSX","TMFGX","ULPIX","UOPIX","WPSGX"),from = "2019-10-01",initial = 1000)
dates <- row.names(EMF)
MF <- colnames(EMF)
rend <- c(967.3915,1016.9254,556.5145,1409.9162,965.1136,1330.5321,1412.6756,1117.2352,1107.793,1250.8098,998.1264,917.0972,2041.1832,1079.9738,1036.389,455.6952,1095.1524,1003.7246,1744.1344,1479.0805,3889.4611,1396.0966,914.1531,1092.8635,561.1896,1128.9828,735.9648,1221.8468,2551.0527,872.1171,667.2516,1006.708,1042.0436,1412.8391,638.1872,1110.5696,1592.8249,1117.4977,1195.9206,1021.937)
end <- as.numeric(last(EMF))
scale <- rend / end
trip <- EMF %*% diag(scale)
EMF <- data.frame(trip)
rownames(EMF) <- dates
colnames(EMF) <- MF
EMF$total <- rowSums( EMF[,1:dim(EMF)[2]] )
EMF$inc <- (EMF$total-EMF$total[1])/EMF$total[1] 

dates <- as.Date(dates)
EMFplot <- as.numeric(EMF[,42])
pp <- data.frame(cbind(dates,EMFplot))
ggplot(pp,aes(dates,EMFplot))+geom_point()+geom_smooth(method = "auto")
```

![](Etrade-MF_files/figure-html/fun-1.png)<!-- -->
