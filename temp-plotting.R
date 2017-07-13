# temp - plot changes on landsat scale

landsat2 <- landsat


#landsat3 <- landsat
spts2 <- rasterToPoints(landsat3)
#temp2 <- as.vector(lc13[1:5900,1])
temp <- rasterize(spts2[,1:2],landsat3,field=lc13[,7])
plot(temp)
writeRaster(temp, "c:\\HR2LS\\final\\cat7_13", format="GTiff", overwrite=TRUE)

lc09[which(is.na(lc09[,1])),1]<- -1
lc09[which(is.na(lc09[,2])),2]<- -1
lc09[which(is.na(lc09[,3])),3]<- -1
lc09[which(is.na(lc09[,4])),4]<- -1
lc09[which(is.na(lc09[,5])),5]<- -1
lc09[which(is.na(lc09[,6])),6]<- -1
lc09[which(is.na(lc09[,7])),7]<- -1

min(lc09[(which(lc09[,1]==-1)),9],na.rm=TRUE)
max(lc09[(which(lc09[,1]==-1)),9],na.rm=TRUE)
min(lc09[(which(lc09[,1]==-1)),10],na.rm=TRUE)
max(lc09[(which(lc09[,1]==-1)),10],na.rm=TRUE)


min(lc13[(which(lc13[,1]==-1)),9],na.rm=TRUE)
max(lc13[(which(lc13[,1]==-1)),9],na.rm=TRUE)
min(lc13[(which(lc13[,1]==-1)),10],na.rm=TRUE)
max(lc13[(which(lc13[,1]==-1)),10],na.rm=TRUE)
