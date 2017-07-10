# temp - plot changes on landsat scale

landsat2 <- landsat
landsat2[which(landsat2[]>200)]<-0
curpixel2 <- 1

for (a in 1:nrow(landsat2))
{
  for (b in 1:ncol(landsat2))
  {
    if(!is.na(landsat2[a,b]))
    {
      landsat2[a,b] <- lc09[curpixel2, 1] - lc13_fixed[curpixel2,1]
      writeLines(paste("Added",a,b,curpixel2))
      curpixel2 <- curpixel2 + 1
    }
  }
  if (a%%5==0)
  {plot(landsat2)}
}

for (a in 1:nrow(lc09))
{
 if (is.na(lc09[a,1]))
   {writeLines(paste(lc09[a,]))}
}




#landsat3 <- landsat
spts2 <- rasterToPoints(landsat3)
#temp2 <- as.vector(lc13[1:5900,1])q
temp <- rasterize(spts2[,1:2],landsat3,field=f)
plot(temp)
writeRaster(temp, "c:\\HR2LS\\tree_canopy_changeF", format="GTiff", overwrite=TRUE)

lc09[which(is.na(lc09[,1])),1]==-1
lc09[which(is.na(lc09[,2])),2]==-1
lc09[which(is.na(lc09[,3])),3]==-1
lc09[which(is.na(lc09[,4])),4]==-1
lc09[which(is.na(lc09[,5])),5]==-1
lc09[which(is.na(lc09[,6])),6]==-1
lc09[which(is.na(lc09[,7])),7]==-1
