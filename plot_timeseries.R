# Make simple time series chart using ndviStack object



vec <- as.numeric(ndviStack[[1:nlayers(ndviStack)]][400,400])
vec2 <- cbind(vec, getZ(ndviStack))

vec3 <- vec2[order(vec2[,2]),]

vec3 <- vec3[which(!is.na(vec3[,1])),]
vec3[,1] <- vec3[,1]/10000
vec3[,2] <- vec3[,2]-min(vec3[,2])+1


plot(vec3[,2],vec3[,1],xlab="",ylab="",pch=18)
title(cex.lab=2, cex.main=2,cex.axis=1.5,cex.sub=2,line=2.5,ylab="NDVI")
title(cex.lab=2, cex.main=2,cex.axis=1.5,cex.sub=2,xlab="Time (days)")
lines(vec3[,2],vec3[,1])

#write.csv(vec3,"C:/Users/MNorton/Documents/CBPChangeDetection/timeseries.csv")
#write.csv(getZ(ndviStack),"C:/Users/MNorton/Documents/CBPChangeDetection/timeseries_dates.csv")

CCDC <- raster("X:/GIS/CBP_Change_Detection/_Data/CCDC_CBP/CCDCYrlyBin/CCDC_09_13bin.tif")
Verd <- raster("X:/GIS/CBP_Change_Detection/_Data/VeRDET/Vbucket/VeRDET_09_13.tif")
VCT <- raster("X:/GIS/CBP_Change_Detection/_Data/VCT/PGvct_0913.tif")
BFAST <- raster("X:/GIS/CBP_Change_Detection/_Data/BFAST/bfast0913PG.tif")
EWMA <- raster("W:/CBP_ChangeDetection/_Data/NDVIStack/EWMACDBucket/EW0913reclass.tif")
LT <- raster("X:/GIS/CBP_Change_Detection/_Bucket/ldtrdr0913pg/w001001.adf")

EWMA[which(EWMA[]==2)] <- 0
EWMA[which(EWMA[]==4)] <- 1

# -----------------  BFAST

r <- CCDC+BFAST
length(which(r[]==0))/length(which(r[] %in% 0:2))
length(which(r[]==1))/length(which(r[] %in% 0:2))
length(which(r[]==2))/length(which(r[] %in% 0:2))
r <- r - 2*CCDC
length(which(r[]==1))/length(which(r[] %in% -1:2))
length(which(r[]==-1))/length(which(r[] %in% -1:2))

r <- EWMA+BFAST
length(which(r[]==0))/length(which(r[] %in% 0:2))
length(which(r[]==1))/length(which(r[] %in% 0:2))
length(which(r[]==2))/length(which(r[] %in% 0:2))
r <- r - 2*BFAST
length(which(r[]==1))/length(which(r[] %in% -1:2))
length(which(r[]==-1))/length(which(r[] %in% -1:2))

r <- LT+BFAST
length(which(r[]==0))/length(which(r[] %in% 0:2))
length(which(r[]==1))/length(which(r[] %in% 0:2))
length(which(r[]==2))/length(which(r[] %in% 0:2))
r <- r - 2*BFAST
length(which(r[]==1))/length(which(r[] %in% -1:2))
length(which(r[]==-1))/length(which(r[] %in% -1:2))

r <- VCT+BFAST
length(which(r[]==0))/length(which(r[] %in% 0:2))
length(which(r[]==1))/length(which(r[] %in% 0:2))
length(which(r[]==2))/length(which(r[] %in% 0:2))
r <- r - 2*BFAST
length(which(r[]==1))/length(which(r[] %in% -1:2))
length(which(r[]==-1))/length(which(r[] %in% -1:2))

r <- Verd+BFAST
length(which(r[]==0))/length(which(r[] %in% 0:2))
length(which(r[]==1))/length(which(r[] %in% 0:2))
length(which(r[]==2))/length(which(r[] %in% 0:2))
r <- r - 2*BFAST
length(which(r[]==1))/length(which(r[] %in% -1:2))
length(which(r[]==-1))/length(which(r[] %in% -1:2))


# -----------------  CCDC

r <- CCDC+Verd
length(which(r[]==0))/length(which(r[] %in% 0:2))
length(which(r[]==1))/length(which(r[] %in% 0:2))
length(which(r[]==2))/length(which(r[] %in% 0:2))
r <- r - 2*CCDC
length(which(r[]==1))/length(which(r[] %in% -1:2))
length(which(r[]==-1))/length(which(r[] %in% -1:2))

r <- CCDC+VCT
length(which(r[]==0))/length(which(r[] %in% 0:2))
length(which(r[]==1))/length(which(r[] %in% 0:2))
length(which(r[]==2))/length(which(r[] %in% 0:2))
r <- r - 2*CCDC
length(which(r[]==1))/length(which(r[] %in% -1:2))
length(which(r[]==-1))/length(which(r[] %in% -1:2))

r <- CCDC+EWMA
length(which(r[]==0))/length(which(r[] %in% 0:2))
length(which(r[]==1))/length(which(r[] %in% 0:2))
length(which(r[]==2))/length(which(r[] %in% 0:2))
r <- r - 2*CCDC
length(which(r[]==1))/length(which(r[] %in% -1:2))
length(which(r[]==-1))/length(which(r[] %in% -1:2))

r <- CCDC+LT
length(which(r[]==0))/length(which(r[] %in% 0:2))
length(which(r[]==1))/length(which(r[] %in% 0:2))
length(which(r[]==2))/length(which(r[] %in% 0:2))
r <- r - 2*CCDC
length(which(r[]==1))/length(which(r[] %in% -1:2))
length(which(r[]==-1))/length(which(r[] %in% -1:2))


# -----------------  EWMA

r <- EWMA+LT
length(which(r[]==0))/length(which(r[] %in% 0:2))
length(which(r[]==1))/length(which(r[] %in% 0:2))
length(which(r[]==2))/length(which(r[] %in% 0:2))
r <- r - 2*EWMA
length(which(r[]==1))/length(which(r[] %in% -1:2))
length(which(r[]==-1))/length(which(r[] %in% -1:2))

r <- EWMA+Verd
length(which(r[]==0))/length(which(r[] %in% 0:2))
length(which(r[]==1))/length(which(r[] %in% 0:2))
length(which(r[]==2))/length(which(r[] %in% 0:2))
r <- r - 2*EWMA
length(which(r[]==1))/length(which(r[] %in% -1:2))
length(which(r[]==-1))/length(which(r[] %in% -1:2))

r <- EWMA+VCT
length(which(r[]==0))/length(which(r[] %in% 0:2))
length(which(r[]==1))/length(which(r[] %in% 0:2))
length(which(r[]==2))/length(which(r[] %in% 0:2))
r <- r - 2*EWMA
length(which(r[]==1))/length(which(r[] %in% -1:2))
length(which(r[]==-1))/length(which(r[] %in% -1:2))

# -----------------  LT

r <- LT+VCT
length(which(r[]==0))/length(which(r[] %in% 0:2))
length(which(r[]==1))/length(which(r[] %in% 0:2))
length(which(r[]==2))/length(which(r[] %in% 0:2))
r <- r - 2*LT
length(which(r[]==1))/length(which(r[] %in% -1:2))
length(which(r[]==-1))/length(which(r[] %in% -1:2))

r <- LT+Verd
length(which(r[]==0))/length(which(r[] %in% 0:2))
length(which(r[]==1))/length(which(r[] %in% 0:2))
length(which(r[]==2))/length(which(r[] %in% 0:2))
r <- r - 2*LT
length(which(r[]==1))/length(which(r[] %in% -1:2))
length(which(r[]==-1))/length(which(r[] %in% -1:2))

r <- VCT+Verd
length(which(r[]==0))/length(which(r[] %in% 0:2))
length(which(r[]==1))/length(which(r[] %in% 0:2))
length(which(r[]==2))/length(which(r[] %in% 0:2))
r <- r - 2*VCT
length(which(r[]==1))/length(which(r[] %in% -1:2))
length(which(r[]==-1))/length(which(r[] %in% -1:2))







