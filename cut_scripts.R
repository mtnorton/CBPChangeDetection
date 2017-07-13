
############################
# REORDER COLUMNS ---- No longer necessary... 
############################

lc13_fixed <- matrix(NA, nrow(lc13), 7)

lc13_fixed[,1] <- rowSums(lc13[,c(3,10:12)])
lc13_fixed[,2] <- rowSums(lc13[,4:5])
lc13_fixed[,3] <- lc13[,6]
lc13_fixed[,4] <- lc13[,1]
lc13_fixed[,5] <- lc13[,7]
lc13_fixed[,6] <- lc13[,9]
lc13_fixed[,7] <- lc13[,8]

############################
# SAVE PROGRESS
############################

lc09_tmp <- round(lc09[which(!is.na(lc09[,1])),],2)
lc13_tmp <- round(lc13[which(!is.na(lc13[,1])),],2)

write.csv(lc09,"C:\\HR2LS\\lc09_tmp62817.csv")
write.csv(lc13,"C:\\HR2LS\\lc13_tmp62817.csv")



lc09<-read.csv("C:\\HR2LS\\lc09_576.csv")
lc13<-read.csv("C:\\HR2LS\\lc13_576.csv")
