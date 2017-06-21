# Average high-res data into Landsat Pixels
# 6.20.2017 by Michael Norton
# Last Updated 6.20.2017

library(sp)
library(rgdal)
library(raster)

# Set up directories

dir_2009 <- "V:\\GIS\\BayPlanimetrics\\TreeCanopyVT\\MD_UTC\\LC_2009\\Prince_Georges_County"
dir_2013 <- "Y:\\LandCover\\MD\\PRIN_24033"
landsat_dir <- "X:\\GIS\\CBP_Change_Detection\\_Data\\Landsat Scenes"
PG_boundaries <- shapefile("X:\\GIS\\PG_County_MD\\_Data\\PG_Boundary.shp")

# Load LC Datasets

lc2009 <- raster(paste0(dir_2009,"\\landcover_2009_princegeorges_3ft.img"))
lc2013 <- raster(paste0(dir_2013,"\\PRIN_24033.img"))

# Grab one Landsat scene for CRS

landsat <- raster(paste0(landsat_dir,"\\LC80150332013111LGN01_B1.tif"))
PG_boundaries_LS <- spTransform(PG_boundaries, crs(landsat))
landsat <- crop(landsat, PG_boundaries_LS)
landsat <- mask(landsat, PG_boundaries_LS)

landsat09 <- projectRaster(landsat, crs=crs(lc2009))
landsat13 <- projectRaster(landsat, crs=crs(lc2013))


#getValuesBlock(landsat, row=500, col=500, nrows=10, ncols=10)

spts <- rasterToPoints(landsat)

lc09 <- matrix(NA,nrow(landsat) * ncol(landsat),8)
lc13 <- matrix(NA,nrow(landsat) * ncol(landsat),13)

goplot=FALSE
par(mfrow=c(2,3),cex.main=1.5,mar=c(4,5,3,0), cex.axis=1.5)

for (i in 1:nrow(landsat))
{
  for (j in 1:ncol(landsat))
  {
    if (!is.na(landsat[i,j]))
    {
    # get coordinates for each pixel with rasterToPoints
    # draw rectangles, grab # of different land types within each tile
      curpixel <- (i-1)*ncol(landsat)+j
      coords <- coordinates(spts)[curpixel,1:2]
      
      x1 <- coords[1]-15
      x2 <- coords[1]+15
      y1 <- coords[2]-15
      y2 <- coords[2]+15
      box = Polygon(matrix(c(x1,y1,x2,y1,x2,y2,x1,y2,x1,y1),ncol = 2, byrow = TRUE))
      box <- Polygons(list(box), ID="a")
      box <- SpatialPolygons(list(box),proj4string = crs(landsat))
      box09 <- spTransform(box, crs(lc2009))
      box13 <- spTransform(box, crs(lc2013))
      
      boxlc09 <- extract(lc2009,box09)
      boxlc13 <- extract(lc2013,box13)
      
      lc09[curpixel,8] <- length(boxlc09[[1]])
      for (k in 1:7)
      {
        lc09[curpixel,k] <- length(which(boxlc09[[1]]==k))/lc09[curpixel,8]
      }
      
      lc13[curpixel,13] <- length(boxlc13[[1]])
      for (k in 1:12)
      {
        lc13[curpixel,k] <- length(which(boxlc13[[1]]==k))/lc13[curpixel,13]
      }
      writeLines (paste(i,j,Sys.time()))
      writeLines (paste("2009:",paste(lc09[curpixel,],collapse=" ")))  
      writeLines (paste("2013:",paste(lc13[curpixel,],collapse=" "))) 
      goplot=TRUE  
    }
    
  }
  
  if (goplot) 
  {
    plot(lc09[,1],rowSums(lc13[,c(3,10:12)]),col="#008000", pch=19,main="Forest",xlim=c(0,1),ylim=c(0,1),ylab="2013",xlab="")
    abline(a=0,b=1, lty=2)
    plot(lc09[,2],rowSums(lc13[,4:5]),col="#00FF00", pch=19,main="Low Veg",xlim=c(0,1),ylim=c(0,1),xlab="2009")
    abline(a=0,b=1, lty=2)
    plot(lc09[,5],lc13[,7],col="#FF0000", pch=19,main="Structures",xlim=c(0,1),ylim=c(0,1),xlab="")
    abline(a=0,b=1, lty=2)
    plot(lc09[,6],lc13[,9],col="#000000", pch=19,main="Roads",xlim=c(0,1),ylim=c(0,1),ylab="2013",xlab="")
    abline(a=0,b=1, lty=2)
    plot(lc09[,4],lc13[,1],col="#0000FF", pch=19,main="Water",xlim=c(0,1),ylim=c(0,1),xlab="2009")
    abline(a=0,b=1, lty=2)
    plot(lc09[,3],lc13[,6],col="#805200", pch=19,main="Barren",xlim=c(0,1),ylim=c(0,1),xlab="")
    abline(a=0,b=1, lty=2)
  }
}



plot(lc09[,2],lc13[,5])
plot(lc09[,5],lc13[,7])
  

