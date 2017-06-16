library(raster)
library(rgdal)
library(rts)


dir <- "C:/Users/Mike/Dropbox/Mike_Morocco"

nat_bound <- shapefile(paste0(dir,"/Data/SHP/MAR_adm0.shp"))
loc_bound <- shapefile(paste0(dir,"/Data/SHP/MAR_adm1.shp"))
wat_lin <- shapefile(paste0(dir,"/Data/SHP/MAR_water_lines_dcw.shp"))
dem <- raster(paste0(dir,"/Data/SHP/MAR_alt.grd"))
#loc_bound2 <- shapefile(paste0(dir,"/SHP/MAR_adm2.shp"))
#loc_bound3 <- shapefile(paste0(dir,"/SHP/MAR_adm3.shp"))
#ORMVA <- shapefile("C:/Users/Mike/Dropbox/Morocco/Data/ORMVA/ORMVA1.shp")

#
#
#
#par(mfrow=c(2,2))
#plot(nat_bound)
#plot(loc_bound)
#plot(loc_bound2)
#plot(loc_bound3)

#cx <- as.matrix(coordinates(wat_lin))

### PLOT ADMIN BOUNDARIES

plot(loc_bound)

for (i in 1:length(wat_lin))
{
  text(cx[i,1],cx[i,2],labels=i,col="red")
}

d <- read.table(paste0(dir,"/raw_LS7_names.txt"))

# specify region
reg <- 4

# or make polygon for boundaries
# Region in Doukkala
pts <- rbind(c(-8.9,32.75),c(-8.0,32.75),c(-8.0,32.25),c(-8.9,32.25),c(-8.9,32.75))
# or Gharb
pts <- rbind(c(-6.7,35),c(-5.15,35),c(-5.15,34.1),c(-6.7,34.1),c(-6.7,35))
# or Marrakesh
pts <- rbind(c(-9.85,32.8),c(-7,32.8),c(-7,30.8),c(-9.85,30.8),c(-9.85,32.8))


sp <- SpatialPolygons(list(Polygons(list(Polygon(pts)),1)))

crop_area <- sp

brick_1 <- crop(brick(paste0(dir,"/Data/TIFF/19990626-0000000000-0000000000.tif"), nl=1), crop_area)
brick_1 <- mask(brick_1,crop_area)

yrs <- vector(mode="numeric",668)
mos <- vector(mode="numeric",668)

yrs[1] <- 1999
mos[1] <- 6

for (i in 2:668)
{
  j2 <- raster(paste0(dir,"/Data/TIFF/",substr(d[i,2],18,27),"-0000000000-0000000000.tif"), nl=1)
  j2_mask<-crop(j2, crop_area)
  j2_mask<-mask(j2_mask, crop_area) 
  
  
  print (paste(i,length(j2_mask[which(j2_mask[,] < -0.5)])))
  
  j2_mask[which(j2_mask[,] < -0.5)] <- NA
  
  brick_1 <- addLayer(brick_1, j2_mask)
  
  yrs[i] <- as.numeric(substr(d[i,2],18,21))
  mos[i] <- as.numeric(substr(d[i,2],22,23))
}

#####################
### SAMPLE PLOT START
#####################

s <- spsample(sp, n=10, type='random')
b <- subset(brick_1,1)
c <- cellFromXY(b,s)

e <- extract(brick_1,c)[1,]

# Set up wave plots

plot(e[280:420],col='white',add=TRUE,ylim=c(0,1),bg='white',pch=21,xlab='Date (8-day intervals starting June 1999)', ylab='NDVI')
#title(main='ORMVA')

prev <- c(0,-1)
for (j in 280:420)
{
  if (!is.na(e[j])) 
  {
    if (prev[2]>-1)
    {
      lines(c(prev[1],j),c(prev[2],e[j]), col="white")
    }
    prev <- c(j, e[j])
  }
}

for (i in c(2,4)) {
  col <- rainbow(i)
  e <- extract(brick_1,c)[i,]
  #points(e,add=T,col=i,pch=21,bg=i)
  prev <- c(0,-1)
  for (j in 1:668)
  {
    if (!is.na(e[j])) 
    {
      if (prev[2]>-1)
      {
        lines(c(prev[1],j),c(prev[2],e[j]), col=i)
      }
      prev <- c(j, e[j])
    }
  }
}

#####################
### SHOW WAVE FIT PLOT START
#####################

# plot(subset(brick_1,9), ylim=c(26,37), xlim=c(-15,0))

plot(brick_1[i,j][1:668],col='black',add=TRUE,ylim=c(0,1),bg='black',pch=21,xlab='Date (8-day intervals starting June 1999)', ylab='NDVI')

x <- 1:668
co <- cos(x*2*pi/46)
si <- sin(x*2*pi/46)
fit <- m$coefficients[1] + m$coefficients[2]*co + si*m$coefficients[3] + x*m$coefficients[4] + b2[xcoord,ycoord][(yrs-1999)*12+mos]*m$coefficients[4]

for (k in 2:length(x))
{    
  lines(c(k-1, k),c(fit[k-1],fit[k]))
}
summary(m)

######################################
### MAKE R2 plot (takes a long time)
######################################

mrow <- nrow(brick_1)
mcol <- ncol(brick_1)
ext <- extent(brick_1)

r2 <- matrix(NA, nrow(brick_1), ncol(brick_1))
intc <- matrix(NA, nrow(brick_1), ncol(brick_1))
c2 <- matrix(NA, nrow(brick_1), ncol(brick_1))
c3 <- matrix(NA, nrow(brick_1), ncol(brick_1))
c4 <- matrix(NA, nrow(brick_1), ncol(brick_1))
c5 <- matrix(NA, nrow(brick_1), ncol(brick_1))
c6 <- matrix(NA, nrow(brick_1), ncol(brick_1))
t1 <- matrix(NA, nrow(brick_1), ncol(brick_1))
t2 <- matrix(NA, nrow(brick_1), ncol(brick_1))
t3 <- matrix(NA, nrow(brick_1), ncol(brick_1))
t4 <- matrix(NA, nrow(brick_1), ncol(brick_1))
t5 <- matrix(NA, nrow(brick_1), ncol(brick_1))
t51 <- matrix(NA, nrow(brick_1), ncol(brick_1))
t52 <- matrix(NA, nrow(brick_1), ncol(brick_1))
maxv <- matrix(NA, nrow(brick_1), ncol(brick_1))
minv <- matrix(NA, nrow(brick_1), ncol(brick_1))
AIC_with <- vector(mode="numeric",mrow*mcol)
AIC_wo <-  vector(mode="numeric",mrow*mcol)

br <- as.matrix(brick_1)
br_coor <- as.matrix(coordinates(brick_1))

spts <- SpatialPoints(cbind(coordinates(brick_1)))
projection(spts)<-projection(brick_1)

cutout <- over(spts,loc_bound[reg,])$VALIDTO_1=="Present"

for (i in 1:mrow)
{
  for (j in 1:mcol)
  {
    vec <- as.vector(br[mcol*(i-1)+j,]) 
    if (!all(sapply(vec,is.na)))
      #if (!is.na(cutout[(i-1)*mcol+j]))
    {
      
      no <- !sapply(vec, is.na)
      #if (length(no==TRUE))
      cord <- br_coor[mcol*(i-1)+j,1:2]
      xcoord <- colFromX(trmm,cord[1])
      ycoord <- rowFromY(trmm,cord[2])
      
      index <- (yrs[no]-1999)*12+mos[no]-1
      
      mx <- lm(vec[no] ~ co[no] + si[no] + x[no])
      AIC_wo[mcol*(i-1)+j] <- AIC(mx)
      
      m <- lm(vec[no] ~ co[no] + si[no] + x[no] + trmm[ycoord,xcoord][index])
      AIC_with[mcol*(i-1)+j] <- AIC(m)
      
      #fitres[i,j,] <- m$coefficients[1] + m$coefficients[2]*co + si*m$coefficients[3]
      r2[i,j] <- summary(m)$r.squared
      intc[i,j] <- m$coefficients[1]
      c2[i,j] <- m$coefficients[2]
      c3[i,j] <- m$coefficients[3]
      c4[i,j] <- m$coefficients[4]
      c5[i,j] <- m$coefficients[5]
      #c6[i,j] <- m$coefficients[6]
      t1[i,j] <- summary(m)$coefficients[1,3]
      t2[i,j] <- summary(m)$coefficients[2,3]
      t3[i,j] <- summary(m)$coefficients[3,3]
      t4[i,j] <- summary(m)$coefficients[4,3]
      t5[i,j] <- summary(m)$coefficients[5,3]
      #t6[i,j] <- summary(m)$coefficients[6,3]
      # plot(fit, vec, add=TRUE, col='black')
      
      f <- m$coefficients[1] + co*m$coefficients[2] + si *m$coefficients[3] + x * m$coefficients[4]  + trmm[xcoord,ycoord][index] * m$coefficients[5]
      fx <- mx$coefficients[1] + co*mx$coefficients[2] + si *mx$coefficients[3] + x * mx$coefficients[4]
      
      maxv[i,j] <- max(as.vector(f),na.rm=TRUE)
      minv[i,j] <- min(as.vector(f),na.rm=TRUE)
      
      print(paste(i,j,Sys.time()))
      print(summary(m))
      
      # Split into two periods
      #m  <- lm(vec[which(no[1:334])] ~ co[which(no[1:334])] + si[which(no[1:334])] + x[which(no[1:334])] + trmm[ycoord,xcoord][index[1:length(which(no[1:334]))]])
      #t51[i,j] <- summary(m)$coefficients[5,3]
      #m  <- lm(vec[which(no[335:668])] ~ co[which(no[335:668])] + si[which(no[335:668])] + x[which(no[335:668])] + trmm[ycoord,xcoord][index[1:length(which(no[335:668]))]])
      #t52[i,j] <- summary(m)$coefficients[5,3]
    }
    else {print(paste(i,j,Sys.time(),"XXXX"))}
  }
  r<-raster(t5)
  extent(r)<-ext
  plot(r,breaks=c(minValue(r),-1.96,-1.69,1.69,1.96,maxValue(r)),col=c("#FF0000","#FF8888","#FFFFFF","#8888FF","#0000FF"),xlab="Longitude",ylab="Latitude")
  plot(ORMVA,add=TRUE,col=color)
  scalebar(10,type="bar",divs=3,below='kilometers')#  ,xy=click())
}

#######################
###AIC TESTS
#######################

rc1 <- raster(intc)
extent(rc1) <- ext
rc2 <- raster(c2)
extent(rc2) <- ext
rc3 <- raster(c3)
extent(rc3) <- ext
rc4 <- raster(c4)
extent(rc4) <- ext
rc5 <- raster(c5)
extent(rc5) <- ext
rmax <- raster(maxv)
extent(rmax) <- ext
rmin <- raster(minv)
extent(rmin) <- ext

breg <- brick(rc1,rc2,rc3,rc4,rc5,rmax,rmin)
classes <- 10
breg.kmeans <- kmeans(breg[], classes, iter.max = 100, nstart = 3)
kmeansraster <- raster(breg)
kmeansraster[] <-breg.kmeans$cluster
plot(kmeansraster,breaks=c(1:classes),col=rainbow(classes))

#######################
###AIC TESTS
#######################

length(which(abs(AIC_with[which(is_ORMVA)])<abs(AIC_wo[which(is_ORMVA)])))
length(which(abs(AIC_with[which(is_ORMVA)])>abs(AIC_wo[which(is_ORMVA)])))
length(which(abs(AIC_with[which(!is_ORMVA)])<abs(AIC_wo[which(!is_ORMVA)])))
length(which(abs(AIC_with[which(!is_ORMVA)])>abs(AIC_wo[which(!is_ORMVA)])))

#######################
###GRAPHS AND BRICK OUTPUT
#######################

extent(r)<-extent(brick_1)
plot(r,breaks=c(minValue(r),-2.33,-1.96,-1.645,1.645,1.96,2.33,maxValue(r)),col=c("#FF0000","#FF6666","#FFAAAA","#FFFFFF","#AAAAFF","#6666FF","#0000FF"))
color <- rgb(0.8,0.8,0.8,alpha=0.4)
plot(ORMVA,add=TRUE,col=color)

extent(r)<-extent(brick_1)
plot(r)
color <- rgb(1,0,0,alpha=0.1)
plot(ORMVA[22:23,],add=TRUE)

r <- raster()
extent(r) <- extent(brick_1)
writeRaster(r,paste0(dir,"/08152014/reg4/2/num_breaks.grd"))

##################################
### Calculate max and min of waves
##################################

fitres <- matrix(nrow(brick_1), ncol(brick_1), x)

co1 <- raster(paste0(dir,"/08152014/reg8/c1.grd"))
co2 <- raster(paste0(dir,"/08152014/reg8/c2.grd"))
co3 <- raster(paste0(dir,"/08152014/reg8/c3.grd"))
co4 <- raster(paste0(dir,"/08152014/reg8/c4.grd"))
co5 <- raster(paste0(dir,"/08152014/reg8/c5.grd"))

for (i in 1:nrow(brick_1))
{
  for (j in 1:ncol(brick_1))
  { 
    #plot(x,brick_1[i,j,])
    cord <- round(coordinates(brick_1)[nrow(brick_1)*(i-1)+j,1:2],1)
    xcoord <- (15+cord[1])*10+1
    ycoord <- (37-cord[2])*10+1
    index <- (yrs-1999)*12+mos
    f <- co1[i,j] + co2[i,j]*co + si*co3[i,j] + x*co4[i,j] + co5[i,j]*trmm[xcoord,ycoord][index]
  }
}

