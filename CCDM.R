# CCDM processing for Change Detection
# Started 6.20.2017 by Michael Norton
# Last update 6.20.2017

# rm(list=ls(all=TRUE))

library (sp)
library (raster)

# Read in landsat images, stack them

setwd("X:\\GIS\\CBP_Change_Detection\\_Data\\Landsat Scenes")

# 21 Apr 2013

b1_a <- raster("LC80150332013111LGN01_B1.tif")
b2_a <- raster("LC80150332013111LGN01_B2.tif")
b3_a <- raster("LC80150332013111LGN01_B3.tif")
b4_a <- raster("LC80150332013111LGN01_B4.tif")
b5_a <- raster("LC80150332013111LGN01_B5.tif")
b6_a <- raster("LC80150332013111LGN01_B6.tif")
b7_a <- raster("LC80150332013111LGN01_B7.tif")
b8_a <- raster("LC80150332013111LGN01_B8.tif")
b9_a <- raster("LC80150332013111LGN01_B9.tif")
b10_a <- raster("LC80150332013111LGN01_B10.tif")
b11_a <- raster("LC80150332013111LGN01_B11.tif")


# 01 Dec 2013

b1_b <- raster("LC80150332013335LGN00_B1.tif")
b2_b <- raster("LC80150332013335LGN00_B2.tif")
b3_b <- raster("LC80150332013335LGN00_B3.tif")
b4_b <- raster("LC80150332013335LGN00_B4.tif")
b5_b <- raster("LC80150332013335LGN00_B5.tif")
b6_b <- raster("LC80150332013335LGN00_B6.tif")
b7_b <- raster("LC80150332013335LGN00_B7.tif")
b8_b <- raster("LC80150332013335LGN00_B8.tif")
b9_b <- raster("LC80150332013335LGN00_B9.tif")
b10_b <- raster("LC80150332013335LGN00_B10.tif")
b11_b <- raster("LC80150332013335LGN00_B11.tif")


#Band Math
dNBR <- (b4_a-b7_a)/(b4_a+b7_a) - (b4_c-b7_c)/(b4_c+b7_c)
dNDVI <- (b4_a-b3_a)/(b4_a+b3_a) - (b4_c-b3_c)/(b4_c+b3_c)

brk <- c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
cols <- c("#000000", "#222222", "#444444", "#666666", "#888888", "#aaaaaa", "#cccccc", "#eeeeee")
plot(dNBR, breaks=brk, col=cols)
plot(dNDVI, breaks=brk, col=cols)


CV= (b1_a-b1_c)+(b2_a-b2_c)+(b3_a-b3_c)+(b4_a-b4_c)+(b5_a-b5_c)+(b6_a-b6_c)+(b7_a-b7_c)+(b8_a-b8_c)+(b9_a-b9_c)+(b10_a-b10_c)+(b11_a-b11_c)
cv <- cv*cv



