# Push out update maps from high-res landcover change
# 
# Started 7.13.2017 by Mike Norton


for (i in 1:7)
{
  thisplot <- i # Classification type to graph
  RMSE_mult <- 3 # number of RMSE away from model to filter
  
  # have to combine roads + impervious
  #t1 <- rowSums(lc09[,6:7])
  #t2 <- rowSums(lc13[,6:7])
  
  t1 <- lc09[,thisplot]
  x2 <- t1^2
  
  titles <- c("Tree Canopy", "Low Vegetation", "Barren", "Water and Wetlands", "Impervious (Structures)", "Impervious (Roads + Other)")
  
  reg <- lm(t2 ~ t1 + x2)
  
  RSS <- c(crossprod(reg$residuals))
  MSE <- RSS/length(reg$residuals)
  RMSE <- sqrt(MSE)
  
  filtered <- vector("numeric",length(reg$residuals))
  filtered[] <- 0
  filtered[which(reg$residuals > RMSE*RMSE_mult)] <- 1
  filtered[which(reg$residuals < RMSE*RMSE_mult*-1)] <- -1
  
  plotter <- rasterize(spts2[,1:2],landsat3,field=filtered)
  plot(plotter)
  writeRaster(plotter, paste0("c:\\HR2LS\\final\\filtered_class_6+7"), format="GTiff", overwrite=TRUE)
  
  
}