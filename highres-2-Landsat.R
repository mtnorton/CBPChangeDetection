# Average high-res data into Landsat Pixels
# 6.20.2017 by Michael Norton
# Last Updated 6.27.2017

library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(grid)

rasterOptions(tmpdir="F:\\HR2LS\\temp")

######################
# PLOTTING FUNCTION
######################

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

################## Preprocessing - so you don't have to test ~500 pixels at the start of every Landsat row
################## RUN only once- VERY SLOW


firstPixelByRow <- vector("numeric",nrow(landsat))

for (i in 1:nrow(landsat))
{
  for (j in 1:ncol(landsat))
  {
    if (!is.na(landsat[i,j]))
    {
      firstPixelByRow[i] <- j
      writeLines(paste(i,j))
      break
    }
  }
}

##########################


# Set up directories (network drives)

#dir_2009 <- "V:\\GIS\\BayPlanimetrics\\TreeCanopyVT\\MD_UTC\\LC_2009\\Prince_Georges_County"
#dir_2013 <- "Y:\\LandCover\\MD\\PRIN_24033"
#landsat_dir <- "X:\\GIS\\CBP_Change_Detection\\_Data\\Landsat Scenes"
#PG_boundaries <- shapefile("X:\\GIS\\PG_County_MD\\_Data\\PG_Boundary.shp")

# Set up directories (local)

setwd("C:\\HR2LS")

# Load LC Datasets
#lc2009 <- raster(paste0(dir_2009,"\\landcover_2009_princegeorges_3ft.img"))
#lc2013 <- raster(paste0(dir_2013,"\\PRIN_24033.img"))

lc2009 <- raster("landcover_2009_princegeorges_3ft.img")
lc2013 <- raster("PRIN_24033.img")

# Grab one Landsat scene for CRS

landsat <- raster(paste0(landsat_dir,"\\LC80150332013111LGN01_B1.tif"))
PG_boundaries_LS <- spTransform(PG_boundaries, crs(landsat))
landsat <- crop(landsat, PG_boundaries_LS)
landsat <- mask(landsat, PG_boundaries_LS)

landsat[which(is.na(landsat[]))] <- -1

# (Not needed - just reproject box later)
#landsat09 <- projectRaster(landsat, crs=crs(lc2009))
#landsat13 <- projectRaster(landsat, crs=crs(lc2013))


#getValuesBlock(landsat, row=500, col=500, nrows=10, ncols=10)

spts <- rasterToPoints(landsat)
spts_x <- matrix(spts[,1], ncol=ncol(landsat),byrow=TRUE)
spts_y <- matrix(spts[,2], ncol=ncol(landsat),byrow=TRUE)
spts_do <- matrix(spts[,3], ncol=ncol(landsat),byrow=TRUE)
spts_do[which(spts_do<0)] <- 0
spts_do[which(spts_do>0)] <- 1


lc09 <- matrix(NA,nrow(spts),10)
lc13 <- matrix(NA,nrow(spts),10)

#lc09 <- lc09[1:nrow(spts),]
#lc13 <- lc13[1:nrow(spts),]

curpixel <- 1

for (i in 1:nrow(landsat))
{
  nowstart09=TRUE
  nowstart13=TRUE
  for (j in 1:ncol(landsat))
  {
      if (spts_do[i,j]==1)
      {
            # draw rectangles, grab # of different land types within each tile
            #(i-1)*ncol(landsat)+j
            #curpixel <- (i-1)*ncol(landsat)+j
          
            coords <- cbind(spts_x[i,j],spts_y[i,j])
            x1 <- coords[1]-15
            x2 <- coords[1]+15
            y1 <- coords[2]-15
            y2 <- coords[2]+15
            box = Polygon(matrix(c(x1,y1,x2,y1,x2,y2,x1,y2,x1,y1),ncol = 2, byrow = TRUE))
            box <- Polygons(list(box), ID="a")
            box <- SpatialPolygons(list(box),proj4string = crs(landsat))
            box09 <- spTransform(box, crs(lc2009))
            box13 <- spTransform(box, crs(lc2013))
            
            if (0) 
            { #this code block for debugging only
              a <- crop(lc2009,box09)
              plot(a)
              b <- crop(lc2013,box13)
              plot(b)
            }
            
            ###############LC 2009
            
            if (j==firstPixelByRow[i]+700)
            {
              #crop raster to process only in memory
              e <- extent(c(extent(lc2009)[1],extent(lc2009)[2],extent(box09)[3]-500,extent(box09)[4]+5000))
              crop09 <- crop(lc2009,e)
              nowstart09 = FALSE
            }
            
            if (nowstart09==FALSE)
            {
              
              boxlc09 <- extract(crop09,box09)
              
              lc09[curpixel,8] <- length(boxlc09[[1]])
              lc09[curpixel,9] <- i
              lc09[curpixel,10] <- j
              
              for (k in 1:7)
              {
                lc09[curpixel,k] <- length(which(boxlc09[[1]]==k))/lc09[curpixel,8]
              }
              writeLines (paste("2009:",paste(lc09[curpixel,],collapse=" ")))  
            }
            
            
            ###############LC 2013
            
            if (j==firstPixelByRow[i]+220)
            {
              e <- extent(c(extent(lc2013)[1],extent(lc2013)[2],extent(box13)[3]-500,extent(box13)[4]+5000))
              crop13 <- crop(lc2013,e)
              nowstart13=FALSE
            }
            
            if (nowstart13==FALSE)
            {
              
              boxlc13 <- extract(crop13,box13)
              
              lc13[curpixel,8] <- length(boxlc13[[1]])
              lc13[curpixel,9] <- i
              lc13[curpixel,10] <- j
              
              lc13[curpixel,1] <- length(which(boxlc13[[1]]==3)) + length(which(boxlc13[[1]]==10)) + length(which(boxlc13[[1]]==11)) + length(which(boxlc13[[1]]==12)) # rowSums(lc13[,c(3,10:12)])
              lc13[curpixel,1] <- lc13[curpixel,1]/lc13[curpixel,8]
              lc13[curpixel,2] <- (length(which(boxlc13[[1]]==4)) + length(which(boxlc13[[1]]==5)))/lc13[curpixel,8]
              lc13[curpixel,3] <- length(which(boxlc13[[1]]==6))/lc13[curpixel,8]
              lc13[curpixel,4] <- length(which(boxlc13[[1]]==1))/lc13[curpixel,8]
              lc13[curpixel,5] <- length(which(boxlc13[[1]]==7))/lc13[curpixel,8]
              lc13[curpixel,6] <- length(which(boxlc13[[1]]==9))/lc13[curpixel,8]
              lc13[curpixel,7] <- length(which(boxlc13[[1]]==8))/lc13[curpixel,8]
              writeLines (paste("2013:",paste(lc13[curpixel,],collapse=" "))) 
            }
            
        writeLines (paste(i,j,Sys.time()))
        
        curpixel <- curpixel + 1
      }
  }
  writeLines ("*********************")
  writeLines (paste0("***************** Finished row ",i,", current pixel is ",curpixel))
  writeLines ("*********************")
  
  if (i%%200==0) # save progress
  {
    write.csv(lc09,paste0("C:\\HR2LS\\lc09_",i,"_",curpixel,".csv"),col.names=FALSE,row.names=FALSE)
    write.csv(lc13,paste0("C:\\HR2LS\\lc13_",i,"_",curpixel,".csv"),col.names=FALSE,row.names=FALSE)
    
  }
}  
  
  if (0) # ((goplot) & (i%%10==0))
  {
    par(mfrow=c(2,3),cex.main=1.5,mar=c(4,5,3,0.5), cex.axis=1.5)
    plot(lc09[,1],rowSums(lc13[,c(3,10:12)]),col="#008000", pch=19,main="Forest",xlim=c(0,1),ylim=c(0,1),ylab="2013",xlab="")
    abline(a=0,b=1, lty=2)
    plot(lc09[,2],rowSums(lc13[,4:5]),col="#00FF00", pch=19,main="Low Veg",xlim=c(0,1),ylim=c(0,1),xlab="2009")
    abline(a=0,b=1, lty=2)
    plot(lc09[,5],lc13[,7],col="#FF0000", pch=19,main="Structures",xlim=c(0,1),ylim=c(0,1),xlab="")
    abline(a=0,b=1, lty=2)
    plot(lc09[,6],lc13[,9],col="#000000", pch=19,main="Roads",xlim=c(0,1),ylim=c(0,1),ylab="2013",xlab="")
    abline(a=0,b=1, lty=2)
    plot(lc09[,7],lc13[,8],col="#AAAAAA", pch=19,main="Impervious (other)",xlim=c(0,1),ylim=c(0,1),xlab="2009")
    abline(a=0,b=1, lty=2)
    plot(lc09[,4],lc13[,1],col="#0000FF", pch=19,main="Water",xlim=c(0,1),ylim=c(0,1),xlab="2009")
    abline(a=0,b=1, lty=2)
    #plot(lc09[,3],lc13[,6],col="#805200", pch=19,main="Barren",xlim=c(0,1),ylim=c(0,1),xlab="")
    #abline(a=0,b=1, lty=2)
  }


############################
# REORDER COLUMNS
############################

lc13_fixed <- matrix(NA, nrow(lc13), 7)

lc13_fixed[,1] <- rowSums(lc13[,c(3,10:12)])
lc13_fixed[,2] <- rowSums(lc13[,4:5])
lc13_fixed[,3] <- lc13[,6]
lc13_fixed[,4] <- lc13[,1]
lc13_fixed[,5] <- lc13[,7]
lc13_fixed[,6] <- lc13[,9]
lc13_fixed[,7] <- lc13[,8]

# No longer necessary... 


############################
# SAVE PROGRESS
############################

lc09_tmp <- round(lc09[which(!is.na(lc09[,1])),],2)
lc13_tmp <- round(lc13[which(!is.na(lc13[,1])),],2)

write.csv(lc09,"C:\\HR2LS\\lc09_tmp62817.csv")
write.csv(lc13,"C:\\HR2LS\\lc13_tmp62817.csv")



lc09<-read.csv("C:\\HR2LS\\lc09_576.csv")
lc13<-read.csv("C:\\HR2LS\\lc13_576.csv")

############################
# Regressions for Tolerances
############################



t1 <- lc09[which(!is.na(lc09[,2])),2]
t2 <- lc13[which(!is.na(lc09[,2])),5]
t1 <- t1[-which(is.na(t2))]
t2 <- t2[-which(is.na(t2))]
x2 <- t1^2

reg <- lm(t2 ~ t1 + x2)

plotting_x <- 1:100/100
plotting_y <- reg$coefficients[1] + plotting_x*reg$coefficients[2]  + (plotting_x^2)*reg$coefficients[3]

par(mfrow=c(1,2))

plot(t1, t2, xlab="Percentage of Pixel 2009", ylab="Percentage of Pixel 2013", main="Low Vegetation")
points(plotting_x, plotting_y, col="yellow", pch=19, cex=3)

RSS <- c(crossprod(reg$residuals))
MSE <- RSS/length(reg$residuals)
RMSE <- sqrt(MSE)

#points(plotting_x[which(plotting_y+RMSE<=1)], plotting_y[which(plotting_y+RMSE<=1)]+RMSE, col="#008000", pch=19, cex=2.5)
points(plotting_x[which(plotting_y+RMSE*2<=1)], plotting_y[which(plotting_y+RMSE*2<=1)]+RMSE*2, col="#008000", pch=19, cex=2.5)

#points(plotting_x[which(plotting_y-RMSE>=0)], plotting_y[which(plotting_y-RMSE>=0)]-RMSE, col="#008000", pch=19, cex=2.5)
points(plotting_x[which(plotting_y-RMSE*2>=0)], plotting_y[which(plotting_y-RMSE*2>=0)]-RMSE*2, col="#008000", pch=19, cex=2.5)

# Now plot for inside/outside RMSE +/- 2

blackpts <- cbind(t1,t2,reg$fitted.values, round(reg$residuals,4))
blackpts <- rbind(blackpts[which(reg$residuals > RMSE*2),], blackpts[which(reg$residuals < RMSE*-2),])
graypts <- cbind(t1,t2,reg$fitted.values, round(reg$residuals,4))
graypts <- graypts[which((reg$residuals <= RMSE*2) & (reg$residuals >= RMSE*-2)),]

plot(blackpts, xlab="Percentage of Pixel 2009", ylab="Percentage of Pixel 2013", main="Low Vegetation - Filtered")
points(graypts, col="#CCCCCC")
points(plotting_x[which(plotting_y+RMSE*2<=1)], plotting_y[which(plotting_y+RMSE*2<=1)]+RMSE*2, col="#008000", pch=19, cex=2.5)
points(plotting_x[which(plotting_y-RMSE*2>=0)], plotting_y[which(plotting_y-RMSE*2>=0)]-RMSE*2, col="#008000", pch=19, cex=2.5)


############################
# RAINBOW SCATTERPLOTS WITH GGPLOT
############################


temp1 <- lc09[,1]
temp2 <- rowSums(lc13[,c(3,10:12)])

df <- data.frame(x = temp1, y = temp2,
                 d = densCols(temp1, temp2, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))
p1 <- ggplot(df) +
  geom_point(aes(x, y, col = d), size = 1) +
  scale_color_identity() +
  theme_bw() +
  ggtitle("Tree Canopy") +
  geom_abline(slope=1,intercept=0,linetype="dotted", size=2) +
  labs(x="2009", y="2013")
#print(p)

temp1 <- lc09[,2]
temp2 <- rowSums(lc13[,4:5])


df <- data.frame(x = temp1, y = temp2,
                 d = densCols(temp1, temp2, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))
p2 <- ggplot(df) +
  geom_point(aes(x, y, col = d), size = 1) +
  scale_color_identity() +
  theme_bw() +
  ggtitle("Low Vegetation") +
  geom_abline(slope=1,intercept=0,linetype="dotted", size=2) +
  labs(x="2009", y="2013")
#print(p + ggtitle("Low Vegetation"))

temp1 <- lc09[,5]
temp2 <- lc13[,7]

df <- data.frame(x = temp1, y = temp2,
                 d = densCols(temp1, temp2, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))
p3 <- ggplot(df) +
  geom_point(aes(x, y, col = d), size = 1) +
  scale_color_identity() +
  theme_bw() +
  ggtitle("Impervious (structures)") +
  geom_abline(slope=1,intercept=0,linetype="dotted", size=2) +
  labs(x="2009", y="2013")
#print(p + ggtitle("Structures"))

temp1 <- rowSums(lc09[,6:7])
temp2 <- rowSums(lc13[,8:9])

df <- data.frame(x = temp1, y = temp2,
                 d = densCols(temp1, temp2, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))
p5 <- ggplot(df) +
  geom_point(aes(x, y, col = d), size = 1) +
  scale_color_identity() +
  theme_bw() +
  ggtitle("Impervious (roads + other)") +
  geom_abline(slope=1,intercept=0,linetype="dotted", size=2) +
  labs(x="2009", y="2013")
#print(p)


temp1 <- lc09[,3]
temp2 <- lc13[,6]

df <- data.frame(x = temp1, y = temp2,
                 d = densCols(temp1, temp2, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))
p4 <- ggplot(df) +
  geom_point(aes(x, y, col = d), size = 1) +
  scale_color_identity() +
  theme_bw() +
  ggtitle("Barren") +
  geom_abline(slope=1,intercept=0,linetype="dotted", size=2) +
  labs(x="2009", y="2013")
#print(p + ggtitle("Impervious (roads + other)"))


temp1 <- lc09[,4]
temp2 <-  rowSums(lc13[,1:2])

df <- data.frame(x = temp1, y = temp2,
                 d = densCols(temp1, temp2, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))
p6 <- ggplot(df) +
  geom_point(aes(x, y, col = d), size = 1) +
  scale_color_identity() +
  theme_bw() +
  ggtitle("Water and Wetlands") +
  geom_abline(slope=1,intercept=0,linetype="dotted", size=2) +
  labs(x="2009", y="2013")
#print(p + ggtitle("Water and Wetlands"))

multiplot(p1,p2,p3,p4,p5,p6,cols=3)

#Lab.palette <- colorRampPalette(c("white","blue","green", "yellow", "red"), space = "Lab")
#smoothScatter(temp1,temp2,colramp = Lab.palette,bandwidth=0.1)
  

