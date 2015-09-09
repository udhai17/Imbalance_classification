############data1################
pdata <- matrix(0, nrow = 10, ncol = 3)
pdata[,3] <- 1
pdata[,1] <- rnorm(10, mean = 3, sd = 0.1)
pdata[,2] <- rnorm(10, mean = 2, sd = 0.1)

ndata <- matrix(0, nrow = 100, ncol = 3)
ndata[,3] <- 0
ndata[,1] <- rnorm(100, mean = 1, sd = 1)
ndata[,2] <- rnorm(100, mean = 2, sd = 1)
data1 <- rbind(pdata,ndata)
plot(data1[,1:2], pch = data1[,3]+1, col = data1[,3]+1, main = "Dataset1", xlab = "", ylab = "")
write.csv(data1, "dataset1.csv", row.names = F)

############data2################
pdata <- matrix(0, nrow = 10, ncol = 3)
pdata[,3] <- 1
pdata[,1] <- rnorm(10, mean = 3, sd = 1)
pdata[,2] <- rnorm(10, mean = 2, sd = 1)

ndata <- matrix(0, nrow = 100, ncol = 3)
ndata[,3] <- 0
ndata[,1] <- rnorm(100, mean = 1, sd = 1)
ndata[,2] <- rnorm(100, mean = 2, sd = 1)
data2 <- rbind(pdata,ndata)
plot(data2[,1:2], pch = data2[,3]+1, col = data2[,3]+1, main = "Dataset2", xlab = "", ylab = "")
write.csv(data2, "dataset2.csv", row.names = F)

############data3################
pdata <- matrix(0, nrow = 10, ncol = 3)
pdata[,3] <- 1
pdata[,1] <- rnorm(10, mean = 1, sd = 0.1)
pdata[,2] <- rnorm(10, mean = 2, sd = 0.1)

ndata <- matrix(0, nrow = 100, ncol = 3)
ndata[,3] <- 0
ndata[,1] <- rnorm(100, mean = 1, sd = 1)
ndata[,2] <- rnorm(100, mean = 2, sd = 1)
data3 <- rbind(pdata,ndata)
plot(data3[,1:2], pch = data3[,3]+1, col = data3[,3]+1, main = "Dataset3", xlab = "", ylab = "")
write.csv(data3, "dataset3.csv", row.names = F)

############data4################
pdata <- matrix(0, nrow = 10, ncol = 3)
pdata[,3] <- 1
pdata[,1] <- rnorm(10, mean = 1, sd = 1)
pdata[,2] <- rnorm(10, mean = 2, sd = 1)

ndata <- matrix(0, nrow = 100, ncol = 3)
ndata[,3] <- 0
ndata[,1] <- rnorm(100, mean = 1, sd = 1)
ndata[,2] <- rnorm(100, mean = 2, sd = 1)
data4 <- rbind(pdata,ndata)
plot(data4[,1:2], pch = data4[,3]+1, col = data4[,3]+1, main = "Dataset4", xlab = "", ylab = "")
write.csv(data4, "dataset4.csv", row.names = F)

############data5################
pdata1 <- matrix(0, nrow = 10, ncol = 3)
pdata1[,3] <- 1
pdata1[,1] <- rnorm(10, mean = 4, sd = 0.1)
pdata1[,2] <- rnorm(10, mean = 1, sd = 0.1)

pdata2 <- matrix(0, nrow = 10, ncol = 3)
pdata2[,3] <- 1
pdata2[,1] <- rnorm(10, mean = 1, sd = 0.1)
pdata2[,2] <- rnorm(10, mean = 4, sd = 0.1)

pdata3 <- matrix(0, nrow = 10, ncol = 3)
pdata3[,3] <- 1
pdata3[,1] <- rnorm(10, mean = 2, sd = 0.1)
pdata3[,2] <- rnorm(10, mean = 1, sd = 0.1)

pdata4 <- matrix(0, nrow = 10, ncol = 3)
pdata4[,3] <- 1
pdata4[,1] <- rnorm(10, mean = 5, sd = 0.1)
pdata4[,2] <- rnorm(10, mean = 4, sd = 0.1)

pdata5 <- matrix(0, nrow = 10, ncol = 3)
pdata5[,3] <- 1
pdata5[,1] <- rnorm(10, mean = 1, sd = 0.1)
pdata5[,2] <- rnorm(10, mean = 2, sd = 0.1)

ndata <- matrix(0, nrow = 500, ncol = 3)
ndata[,3] <- 0
ndata[,1] <- rnorm(500, mean = 3, sd = 1)
ndata[,2] <- rnorm(500, mean = 3, sd = 1)
data5 <- rbind(pdata1, pdata2, pdata3, pdata4, pdata5 ,ndata)
plot(data5[,1:2], pch = data5[,3]+1, col = data5[,3]+1, main = "Dataset5", xlab = "", ylab = "")
write.csv(data5, "dataset5.csv", row.names = F)