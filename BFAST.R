#BFAST package updating
# Started 7.13.2017 by Mike Norton
# Last updated 7.24

library(utils)
library(stringr)
library(raster)
library(rgdal)
library(caTools)
library(devtools)
devtools::install_github('loicdtx/bfastSpatial', ref = 'develop')
library(bfastSpatial)
library(maptools)
library(sp)
rasterOptions(tmpdir="F:\\HR2LS\\temp")
setwd("F:\\Landsat7")


newExtent <- extent(PG_boundaries_LS)

# List the Landsat scenes
inputList <- list.files("F:/Landsat7/", pattern="*.tar.gz", full.names=TRUE)
dirout = "F:/ls_out/"

# Process new landsat scenes for LS7
processLandsatBatch(x = inputList, outdir = dirout, delete = TRUE, vi = 'ndvi', mask = 'pixel_qa', keep = c(66, 130), e=newExtent, overwrite = TRUE)

# Process new landsat scenes for LS5 and 8 ... from server

inputList <- list.files("W:/CBP_ChangeDetection/_Data/Landsat5_7_8_HLDP_Collection1",pattern=glob2rx("LT05*2008*.tar.gz"), full.names=TRUE)
processLandsatBatch(x = inputList, outdir = dirout, delete = TRUE, vi = 'ndvi', mask = 'pixel_qa', keep = c(66, 130), e=newExtent, overwrite = TRUE)

inputList <- list.files("W:/CBP_ChangeDetection/_Data/Landsat5_7_8_HLDP_Collection1", pattern=glob2rx("LC08*.tar.gz"), full.names=TRUE)
processLandsatBatch(x = inputList, outdir = dirout, delete = TRUE, vi = 'ndvi', mask = 'pixel_qa', keep = c(322, 386), e=newExtent, overwrite = TRUE)

#inputList <- list.files("W:/CBP_ChangeDetection/_Data/Landsat5_7_8_HLDP_Collection1",pattern=glob2rx("LT05*2008*.tar.gz"), full.names=TRUE)
#inputList <- c(inputList,list.files("W:/CBP_ChangeDetection/_Data/Landsat5_7_8_HLDP_Collection1",pattern=glob2rx("LT05*2009*.tar.gz"), full.names=TRUE))
#inputList <- c(inputList,list.files("W:/CBP_ChangeDetection/_Data/Landsat5_7_8_HLDP_Collection1",pattern=glob2rx("LT05*2010*.tar.gz"), full.names=TRUE))
#inputList <- c(inputList,list.files("W:/CBP_ChangeDetection/_Data/Landsat5_7_8_HLDP_Collection1",pattern=glob2rx("LT05*2011*.tar.gz"), full.names=TRUE))
#processLandsatBatch(x = inputList, outdir = dirout, delete = TRUE, vi = 'ndvi', mask = 'pixel_qa', keep = c(66, 130), e=newExtent, overwrite = TRUE)


# List the processed NDVI scenes for stacking
ndviList <- list.files("F:/ls_out/ndvi", pattern='*ndvi.grd', full.names = TRUE)
dirout2 <- file.path(dirname(rasterTmpFile()), 'stack')
dir.create(dirout2, showWarnings=FALSE)

# Remove pixel saturation. (values of 20,000 for NDVI)
for (i in 1:length(ndviList))
{
  r <- raster(ndviList[i])
  g <- length(which(r[]==20000))
  r[which(r[]==20000)] <- NA
  writeRaster(r, ndviList[i], overwrite=TRUE)
  writeLines(paste(g,ndviList[i]))
}

# Generate a file name for the output stack
stackName <- file.path(dirout2, 'stackNDVI_path15_row33.grd')

# Stack the layers
ndviStack <- timeStack(x=ndviList, filename=stackName, datatype='INT2S', overwrite=TRUE)
notNA <- sum(!is.na(ndviStack))

#bfm <- bfmPixel(ndviStack, start=c(1999,7), interactive=TRUE)

#Define output path
out <- file.path(dirout2, "bfastNDVI.grd")


# remove blank layers and divide into smaller tiles

#Run bfmSpatial
bfmSpatial(ndviStack, formula = response~trend+harmon, order = 1, history = c(1985, 1), filename = out)

# Or by pixel
bfm <- bfmPixel(ndviStack, start=c(2009,1), formula = response~harmon, interactive=TRUE) #start=c(2009,1), interactive=TRUE)
plot(bfm$bfm)

#############################
# UNNECESSARY????
#############################




#############################
# FUNCTIONALITY
# 1. Untar NDVI and qa_band from tar files
# 2. Run qa tool for dropped pixel (at command line)
# 3. crop NDVI to PG Boundaries and mask to both PG boundaries and QA mask
# 4. Create timeStack using rasterStack z options
# 5. Run modified bfm function that shows progress
#############################


filelist <- list.files("F:/Landsat7/")
targetdir <- "F:/Landsat7/untar_new"

#############################
# UNTAR FROM TARBALL
#############################

untar_tiff <- function(filename) { 
  #dir.create(folder, showWarnings = F)
  list <- untar(filename, list = T)
  untar(filename, files = str_c(list[str_detect(list, "ndvi.tif")]), exdir = "F:\\Landsat7\\untar_new")
  writeLines(filename)
} 

lapply(filelist, untar_tiff)

#############################
# CROP AND MASK TO PG COUNTY
#############################

PG_boundaries <- shapefile("X:\\GIS\\PG_County_MD\\_Data\\PG_Boundary.shp")
PG_boundaries_LS <- spTransform(PG_boundaries, crs(ndvi1))

filelist <- list.files(targetdir, pattern="*.tif")
filelist_full <- list.files(targetdir, pattern="*.tif", full.names=TRUE)

crop_and_mask <- function(filename) {
  f <- raster(filename)
  f <- crop(f, PG_boundaries_LS)
  #f <- mask(f, PG_boundaries_LS)
  outname <- paste0(targetdir,"/ndvi",substr(filename,40,47),".grd")
  writeRaster(f, outname, overwrite=TRUE)
  writeLines(outname)
}

lapply(filelist_full, crop_and_mask)

#############################
# MAKE TIMESTACK
#############################

filelist_ndvi <- list.files(targetdir, pattern="*.grd", full.names=TRUE)

dates <- substr(filelist_ndvi,27,34)
dates <- as.Date(dates, format="%Y%j")

#get dates, set Z

s <- stack(filelist_ndvi)
s <- setZ(x=s, z=dates)

#############################
# RUN BFAST
#############################

bfm <- bfmPixel(s, start=c(2015,1), interactive=TRUE)