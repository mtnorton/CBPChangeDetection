# CCDC Summary Statistics
# 8.9.2017 by Mike

#Number of breaks by year for all CD methods

library(raster)

setwd("X:/GIS/CBP_Change_Detection/_Data/CCDC_CBP")

g <- matrix(NA, 32,8)
colnames(g) <- c("Year", "BFAST (2000)", "BFAST (2009)", "CCDC", "EWMA", "LandTrendr", "VCT",  "VeRDET")
g[,1] <- 1985:2016

for (i in 1985:2014)
{
  filename <- paste0("year_",i,".tif")
  r <- raster(filename)
  r <- crop(r, PG_boundaries)
  r <- mask(r, PG_boundaries)
  g[i-1984,4] <- length(which(r[]>0))/length(which(r[]==0))
}

#plot(g[,1],g[,2],col="white",cex.lab=2, cex.main=2,cex.axis=1.5,cex.sub=2,xlab="Year",ylab="",main="CCDC breaks detected by year")
#title(ylab="Percentage",line=2.5,cex.lab=2)
#lines(g[,1],g[,2])
#lines(1985:2014,rep(mean(g[,1]),30), lty=2)

r <- raster("f:/ls_temp/bfastNDVI.grd")
r <- mask(r,PG_boundaries_LS)
for (i in 2009:2016)
{
  g[i-2008+24,2] <- length(which(floor(r[])==i))/length(which(!is.na(r[])))
}

r <- raster("f:/ls_temp/bfastNDVI_2000.grd")
r <- mask(r,PG_boundaries_LS)
for (i in 2000:2016)
{
  g[i-1999+15,3] <- length(which(floor(r[])==i))/length(which(!is.na(r[])))
}


#LandTrendr

g[,6] <- 0

for (j in 1:7)
{
  r <- raster("X:/GIS/CBP_Change_Detection/_Data/LandTrendr/lt_medoid_015033NDVI.tif",band=j)
  r <- crop(r, PG_boundaries)
  r <- mask(r, PG_boundaries)
  
  for (i in 1985:2016)
  {
    g[i-1984,6] <- g[i-1984,6] + length(which(r[]==i))/length(which(!is.na(r[])))
    writeLines(paste(i,j,Sys.time()))
  }
}


#VCT
r <- raster("X:/GIS/CBP_Change_Detection/_Data/VCT/vct_15033 (1).tif",band=1)
PG_temp <- spTransform(PG_boundaries,crs(r))

g[,7] <- 0

for (j in 1:3)
{
  r <- raster("X:/GIS/CBP_Change_Detection/_Data/VCT/vct_15033 (1).tif",band=j)
  r <- crop(r, PG_temp)
  r <- mask(r, PG_temp)
  
  for (i in 1985:2016)
  {
    g[i-1984,7] <- g[i-1984,7] + length(which(r[]==i))/length(which(!is.na(r[])))
    writeLines(paste(i,j,Sys.time()))
  }
}

g[,5] <- 0

#EWMACD
r <- raster("W:/CBP_ChangeDetection/_Data/NDVIStack/EWMACD_results.tif")
PG_temp <- spTransform(PG_boundaries,crs(r))
for (j in 2:33)
{
  r <- raster("W:/CBP_ChangeDetection/_Data/NDVIStack/EWMACD_results.tif",band=j)
  r <- crop(r, PG_temp)
  r <- mask(r, PG_temp)
  r[which(r[] %in% 1:3)] <- 0
  g[j-1,5] <- g[j-1,5] + length(which(r[]!=0))/length(which(!is.na(r[])))
  writeLines(paste(j,Sys.time()))
}


g[32,6] <- NA


# VeRDET
dir <- "X:/GIS/CBP_Change_Detection/_Data/VeRDET/Vbucket/"
for (i in 1985:2016)
{
  fn <- paste0(dir,"PGbin",i,".tif")
  r <- raster(fn)
  g[i-1984,8] <- length(which(r[]>0))/length(which(!is.na(r[])))
  writeLines(paste(i,Sys.time()))
}

cols <- c("gray20", "gray20", "red", "darkorchid3", "mediumseagreen", "dodgerblue3", "chocolate2")

plot(g[,1],g[,2],ylim=c(0,0.25),col="white",main="Breaks detected by year (%)",ylab="",xlab="Year",cex.lab=2, cex.main=2,cex.axis=1.5,cex.sub=2)
title(ylab="Percentage",line=2.5, cex.lab=2)
lines(g[,1],g[,2],col="gray20",lwd=3,lty=2)
for (i in 3:8)
{
  lines(g[,1],g[,i],col=cols[i-1],lwd=3)
}
legend(x="top", 1, legend=colnames(g)[2:8], col=cols, lty=c(1,2,rep(1,5)), cex=1,lwd=3)
              


for (i in 2:6)
{
  for (j in 2:6)
  {
    writeLines(paste(i,j,cor(g[,i],g[,j],use="complete.obs")))
  }
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
