logall <- data.frame(n=numeric(),r=numeric(),x=numeric())
for (r in seq(0,4,by = .005)){
  s <- .25
  logx <- data.frame(n=numeric(),r=numeric(),x=numeric())
  logx[1,1] <- 1
  logx[1,2] <- r
  logx[1,3] <- r*s*(1-s)
  for (n in 2:250){
    logx[n,1] <- n
    logx[n,2] <- r
    logx[n,3] <- r*logx[n-1,3]*(1-logx[n-1,3])
  } # End of n loop
  logall <- rbind(logall,logx)
} #End of r loop

lgmap <- logall %>% filter(n >= 240) 
lm <- lgmap %>% ggplot(aes(r,x))+geom_point(size=.005)
