#BFAST package updating
# Started 7.13.2017 by Mike Norton

library(utils)
library(stringr)

setwd("F:\\Landsat7\\")


filelist <- list.files("F:\\Landsat7\\")



for (i in 1:length(filelist))
{
  tarlist <- untar(paste0("F:\\Landsat7\\",filelist[i]), list=TRUE)
  ndvi_img <- grep("ndvi.tif",tarlist)
  
  if (!is.na(tarlist[ndvi_img]))
  {
    date <- substr(tarlist[ndvi_img],18,25)
  }
  
  writeLines(date)
}
