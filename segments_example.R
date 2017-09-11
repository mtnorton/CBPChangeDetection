

library(segmented)
library(ggplot2)

seg_matrix <- matrix(NA, 300, 2)

# Set three obvious segments

seg_matrix[,1] <- 1:300
seg_matrix[1:100,2] <- seg_matrix[1:100,1]*0.1
seg_matrix[101:200,2] <- seg_matrix[101:200,1]*1-90
seg_matrix[201:300,2] <- seg_matrix[201:300,1]*0.1+90

# Add random noise

seg_matrix[,2] <- seg_matrix[,2] + rnorm(300,0,3)

plot(seg_matrix)

# Convert to data frame

seg_matrix <- as.data.frame(seg_matrix)

# Do normal regression, colnames as default (V1, V2)

my.lm <- lm (V2 ~ V1, data=seg_matrix)

summary(my.lm)

my.seg <- segmented(my.lm, 
                    seg.Z = ~ V1,
                    psi = list(V1 = c(100, 200)))

summary(my.seg)

#get breakpoints and slope

my.seg$psi
slope(my.seg)

# get the fitted data
my.fitted <- fitted(my.seg)
my.model <- data.frame(V1 = seg_matrix$V1, V2 = my.fitted)

# plot the fitted model
p <- ggplot(seg_matrix, aes(x = V1, y = V2)) + geom_line()
p + geom_line(data = my.model, aes(x = V1, y = V2), colour = "tomato")


u <- seg_matrix$V2
mygroup <- c(rep("group1",300))
ecdfs = lapply(split(u,mygroup), ecdf)
ys = sapply(ecdfs, function(e) e(u))
