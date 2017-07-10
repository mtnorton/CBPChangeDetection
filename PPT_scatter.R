# sample scatterplot for PPT
# Michael Norton 6.27.17


data <- matrix(c(100,31.3,0,4.4,0,35.7,0,11.7,0,16.9),ncol = 2, byrow = TRUE)

plot(data[,1],data[,2],col=c("#008000", "#00FF00", "#FFAA00", "#FF0000", "#000000"), cex=2, pch=19,main="Land Cover Changes",xlim=c(0,100),ylim=c(0,100),ylab="Percentage of land cover 2013",xlab="Percentage of land cover 2009")
abline(a=0,b=1, lty=2)