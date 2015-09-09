data <- iris[,c(1,2,3,5)]

data[,4] <- as.numeric(data[,4])
data[,4][data[,4]!=1] <- 0

plot3d(data[,1:3],col=data[,4]+1)


