# temp - plot changes on landsat scale

landsat2 <- landsat


#landsat3 <- landsat
spts2 <- rasterToPoints(landsat3)
#temp2 <- as.vector(lc13[1:5900,1])
temp <- rasterize(spts2[,1:2],landsat3,field=lc13[,7])
plot(temp)
writeRaster(temp, "c:\\HR2LS\\final\\cat7_13", format="GTiff", overwrite=TRUE)

############################
# Graph regressions for tolerance
############################

thisplot <- 1 # Classification type to graph
RMSE_mult <- 3 # number of RMSE away from model to filter

t1 <- lc09[,thisplot]
t2 <- lc13[,thisplot]
x2 <- t1^2

titles <- c("Tree Canopy", "Low Vegetation", "Barren", "Water and Wetlands", "Impervious (Structures)", "Impervious (Roads + Other)")

reg <- lm(t2 ~ t1 + x2)

plotting_x <- 1:100/100
plotting_y <- reg$coefficients[1] + plotting_x*reg$coefficients[2]  + (plotting_x^2)*reg$coefficients[3]

par(mfrow=c(1,2))

plot(t1, t2, xlab="Percentage of Pixel 2009", ylab="Percentage of Pixel 2013", main=titles[thisplot],xlim=c(0,1),ylim=c(0,1))
points(plotting_x, plotting_y, col="yellow", pch=19, cex=2)

RSS <- c(crossprod(reg$residuals))
MSE <- RSS/length(reg$residuals)
RMSE <- sqrt(MSE)

#points(plotting_x[which(plotting_y+RMSE<=1)], plotting_y[which(plotting_y+RMSE<=1)]+RMSE, col="#008000", pch=19, cex=2.5)
points(plotting_x[which(plotting_y+RMSE*RMSE_mult<=1)], plotting_y[which(plotting_y+RMSE*RMSE_mult<=1)]+RMSE*RMSE_mult, col="#008000", pch=19, cex=2)

#points(plotting_x[which(plotting_y-RMSE>=0)], plotting_y[which(plotting_y-RMSE>=0)]-RMSE, col="#008000", pch=19, cex=2.5)
points(plotting_x[which(plotting_y-RMSE*RMSE_mult>=0)], plotting_y[which(plotting_y-RMSE*RMSE_mult>=0)]-RMSE*RMSE_mult, col="#008000", pch=19, cex=2)

# Now plot for inside/outside RMSE +/- 2

blackpts <- cbind(t1,t2,reg$fitted.values, round(reg$residuals,4))
blackpts <- rbind(blackpts[which(reg$residuals > RMSE*RMSE_mult),], blackpts[which(reg$residuals < RMSE*RMSE_mult*-1),])
graypts <- cbind(t1,t2,reg$fitted.values, round(reg$residuals,4))
graypts <- graypts[which((reg$residuals <= RMSE*RMSE_mult) & (reg$residuals >= RMSE*RMSE_mult*-1)),]

plot(blackpts, xlab="Percentage of Pixel 2009", ylab="Percentage of Pixel 2013", main=paste(titles[thisplot],"- Filtered"),xlim=c(0,1),ylim=c(0,1))
points(graypts, col="#CCCCCC")
points(plotting_x[which(plotting_y+RMSE*RMSE_mult<=1)], plotting_y[which(plotting_y+RMSE*RMSE_mult<=1)]+RMSE*RMSE_mult, col="#008000", pch=19, cex=2)
points(plotting_x[which(plotting_y-RMSE*RMSE_mult>=0)], plotting_y[which(plotting_y-RMSE*RMSE_mult>=0)]-RMSE*RMSE_mult, col="#008000", pch=19, cex=2)

############################
# Summarize hi-res changes
############################

r <- raster("X:/GIS/CBP_Change_Detection/_Data/HR2LS/filtered_class_6+7.tif")

length(which(r[]==1))
length(which(r[]==-1))
length(which(r[]==0))

############################
# RAINBOW SCATTERPLOTS WITH GGPLOT
############################

temp1 <- lc09[,1]
temp2 <- lc13[,1]

df <- data.frame(x = temp1, y = temp2,
                 d = densCols(temp1, temp2, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))
p1 <- ggplot(df) +
  geom_point(aes(x, y, col = d), size = 1) +
  scale_color_identity() +
  theme_bw() +
  xlim(0,1) +
  ylim(0,1) +
  ggtitle("Tree Canopy") +
  geom_abline(slope=1,intercept=0,linetype="dotted", size=2) +
  labs(x="Percentage LC 2009", y="Percentage LC 2013")
#print(p)

temp1 <- lc09[,2]
temp2 <- lc13[,2]


df <- data.frame(x = temp1, y = temp2,
                 d = densCols(temp1, temp2, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))
p2 <- ggplot(df) +
  geom_point(aes(x, y, col = d), size = 1) +
  scale_color_identity() +
  theme_bw() +
  xlim(0,1) +
  ylim(0,1) +
  ggtitle("Low Vegetation") +
  geom_abline(slope=1,intercept=0,linetype="dotted", size=2) +
  labs(x="Percentage LC 2009", y="Percentage LC 2013")
#print(p + ggtitle("Low Vegetation"))

temp1 <- lc09[,3]
temp2 <- lc13[,3]

df <- data.frame(x = temp1, y = temp2,
                 d = densCols(temp1, temp2, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))
p3 <- ggplot(df) +
  geom_point(aes(x, y, col = d), size = 1) +
  scale_color_identity() +
  theme_bw() +
  xlim(0,1) +
  ylim(0,1) +
  ggtitle("Barren") +
  geom_abline(slope=1,intercept=0,linetype="dotted", size=2) +
  labs(x="Percentage LC 2009", y="Percentage LC 2013")
#print(p + ggtitle("Structures"))

temp1 <- lc09[,4]
temp2 <- lc13[,4]

df <- data.frame(x = temp1, y = temp2,
                 d = densCols(temp1, temp2, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))
p5 <- ggplot(df) +
  geom_point(aes(x, y, col = d), size = 1) +
  scale_color_identity() +
  theme_bw() +
  xlim(0,1) +
  ylim(0,1) +
  ggtitle("Water and Wetlands") +
  geom_abline(slope=1,intercept=0,linetype="dotted", size=2) +
  labs(x="Percentage LC 2009", y="Percentage LC 2013")
#print(p)


temp1 <- lc09[,5]
temp2 <- lc13[,5]

df <- data.frame(x = temp1, y = temp2,
                 d = densCols(temp1, temp2, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))
p4 <- ggplot(df) +
  geom_point(aes(x, y, col = d), size = 1) +
  scale_color_identity() +
  theme_bw() +
  xlim(0,1) +
  ylim(0,1) +
  ggtitle("Impervious (structures)") +
  geom_abline(slope=1,intercept=0,linetype="dotted", size=2) +
  labs(x="Percentage LC 2009", y="Percentage LC 2013")
#print(p + ggtitle("Impervious (roads + other)"))


temp1 <- rowsums(lc09[,6:7])
temp2 <- rowsums(lc13[,6:7])

df <- data.frame(x = temp1, y = temp2,
                 d = densCols(temp1, temp2, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))
p6 <- ggplot(df) +
  geom_point(aes(x, y, col = d), size = 1) +
  scale_color_identity() +
  theme_bw() +
  xlim(0,1) +
  ylim(0,1) +
  ggtitle("Impervious (roads + other)") +
  geom_abline(slope=1,intercept=0,linetype="dotted", size=2) +
  labs(x="Percentage LC 2009", y="Percentage LC 2013")
#print(p + ggtitle("Water and Wetlands"))

multiplot(p1,p2,p3,p4,p5,p6,cols=3)