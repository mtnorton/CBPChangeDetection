# CCDC Summary Statistics
# 8.9.2017 by Mike

#Number of breaks by year for all CD methods

setwd("X:/GIS/CBP_Change_Detection/_Data/CCDC_CBP")

g <- matrix(NA, 32,8)
g[,1] <- 1985:2016

for (i in 1985:2014)
{
  filename <- paste0("year_",i,".tif")
  r <- raster(filename)
  r <- crop(r, PG_boundaries)
  r <- mask(r, PG_boundaries)
  g[i-1984,2] <- length(which(r[]>0))/length(which(r[]==0))
}

plot(g[,1],g[,2],col="white",cex.lab=2, cex.main=2,cex.axis=1.5,cex.sub=2,xlab="Year",ylab="",main="CCDC breaks detected by year")
title(ylab="Percentage",line=2.5,cex.lab=2)
lines(g[,1],g[,2])
lines(1985:2014,rep(mean(g[,1]),30), lty=2)

r <- raster("f:/ls_temp/bfastNDVI.grd")
r <- mask(r,PG_boundaries_LS)
for (i in 2009:2016)
{
  g[i-2008,3] <- length(which(round(r[],0)==i))/length(which(!is.na(r[])))
}


















#BFAST Timing - old plot/disregard

r <- raster("f:/ls_temp/bfastNDVI_notrend.grd")
r <- mask(r,PG_boundaries_LS)
h = hist(r)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE,main="Timing of BFAST breaks - without trend", xlab="Date",cex.lab=2, cex.main=2, cex.sub=2,col="gray",ylab="",ylim=c(0,25))
title(ylab="Percentage",line=2.5,cex.lab=2)

r <- raster("f:/ls_temp/bfastNDVI.grd")
r <- mask(r,PG_boundaries_LS)
h = hist(r)
h$density = h$counts/sum(h$counts)*100
plot(h, freq=FALSE, main="Timing of BFAST breaks - with trend", xlab="Date",cex.lab=2, cex.main=2, cex.sub=2,col="gray",ylab="",ylim=c(0,25))
title(ylab="Percentage",line=2.5,cex.lab=2)
