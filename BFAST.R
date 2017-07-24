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