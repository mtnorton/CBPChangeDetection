library(raster)

r <- raster("/Users/mtnorton/NLCD/nlcd_2006_to_2011_landcover_fromto_change_index_2011_edition_2014_10_10.img")
e <- extent(c(1618900,1657230,1891920,1953760))

r <- crop(r, e)

TCplus <- c(26,43,60,77,94,111,128,196,213,230,247,264,281,
            27,44,61,78,95,112,129,197,214,231,248,265,282,
            28,45,62,79,96,113,130,198,215,232,249,266,283)
TCminus <- c(138,155,172,
             139,156,173,
             140,157,174,
             141,158,175,
             142,159,176,
             143,160,177,
             144,161,178,
             148,165,182,
             149,166,183,
             150,167,184,
             151,168,185,
             152,169,186,
             153,170,187)

TC <- matrix(NA,nrow=nrow(r),ncol=ncol(r))
TC <- raster(TC)
extent(TC) <- extent(r)
crs(TC) <- crs(r)
TC[which(r[] %in% TCplus)] <- 1
TC[which(r[] %in% TCminus)] <- -1

LVplus  <- c(29,46,63,80,97,114,131,148,165,182,233,250,267,284,
             30,47,64,81,98,115,132,149,166,183,234,251,268,285)
LVminus <- c(189:198,201:204,206:215,218:221)

LV <- matrix(NA,nrow=nrow(r),ncol=ncol(r))
LV <- raster(LV)
extent(LV) <- extent(r)
crs(LV) <- crs(r)
LV[which(r[] %in% LVplus)] <- 1
LV[which(r[] %in% LVminus)] <- -1

BNplus  <- c(21,38,72,89,106,140,157,174,191,208,225,242,259,276, 
             25,42,76,93,110,144,161,178,195,212,229,246,263,280)
BNminus <- c(56:58,121:126,128:136)

BN <- matrix(NA,nrow=nrow(r),ncol=ncol(r))
BN <- raster(BN)
crs(BN) <- crs(r)
extent(BN) <- extent(r)
BN[which(r[] %in% BNplus)] <- 1
BN[which(r[] %in% BNminus)] <- -1

WTplus <- c(36,53,70,87,104,121,138,155,172,189,206,223,240,
            50,67,84,101,118,135,152,169,186,203,220,237,254,
            34,51,68,85,102,119,136,153,170,187,204,221,238,255)
WTminus <- c(20:32,258:270,275:287)

WT <- matrix(NA,nrow=nrow(r),ncol=ncol(r))
WT <- raster(WT)
extent(WT) <- extent(r)
crs(WT) <- crs(r)
WT[which(r[] %in% WTplus)] <- 1
WT[which(r[] %in% WTminus)] <- -1

IMplus <- c(22:24, 39:41, 124:126, 141:143, 158:160, 175:177, 192:194, 209:211, 226:228, 243:245, 260:262, 277:279)
IMminus <- c(76:85, 87,93:102, 110:119)

IM <- matrix(NA,nrow=nrow(r),ncol=ncol(r))
IM <- raster(IM)
extent(IM) <- extent(r)
crs(IM) <- crs(r)
IM[which(r[] %in% IMplus)] <- 1
IM[which(r[] %in% IMminus)] <- -1

AGplus <- c(32,49,66,83,100,117,134,151,168,185,202,219,236,270,287,
            31,48,65,82,99,116,133,150,167,184,201,252,269,286)
AGminus <- c(240:252, 254:255, 223:234,236:238)


AG <- matrix(NA,nrow=nrow(r),ncol=ncol(r))
AG <- raster(AG)
extent(AG) <- extent(r)
crs(AG) <- crs(r)
AG[which(r[] %in% AGplus)] <- 1
AG[which(r[] %in% AGminus)] <- -1

writeRaster(TC, "/Users/mtnorton/NLCD/TC.tif",format="GTiff",overwrite=TRUE)
writeRaster(LV, "/Users/mtnorton/NLCD/LV.tif",format="GTiff",overwrite=TRUE)
writeRaster(BN, "/Users/mtnorton/NLCD/BN.tif",format="GTiff",overwrite=TRUE)
writeRaster(WT, "/Users/mtnorton/NLCD/WT.tif",format="GTiff",overwrite=TRUE)
writeRaster(IM, "/Users/mtnorton/NLCD/IM.tif",format="GTiff",overwrite=TRUE)
writeRaster(AG, "/Users/mtnorton/NLCD/AG.tif",format="GTiff",overwrite=TRUE)
