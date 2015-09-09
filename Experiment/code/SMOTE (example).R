###################SMOTE DATASET1###########################
temp <- data1
over <- length(temp[,3])/sum(temp[,3])*100

plot(temp[,1:2], col = temp[,3]+1, pch = temp[,3]+1, main = "Dataset1", xlab = "", ylab = "")
syn <- ubSMOTE(as.data.frame(temp[,-3]), as.factor(temp[,3]), perc.over = over, perc.under = 0)
points(syn$X, col = 3, pch = 2)
points(temp[,1:2], col = temp[,3]+1, pch = temp[,3]+1)

###################SMOTE DATASET2###########################
temp <- data2
over <- length(temp[,3])/sum(temp[,3])*100

plot(temp[,1:2], col = temp[,3]+1, pch = temp[,3]+1, main = "Dataset2", xlab = "", ylab = "")
syn <- ubSMOTE(as.data.frame(temp[,-3]), as.factor(temp[,3]), perc.over = over, perc.under = 0)
points(syn$X, col = 3, pch = 2)
points(temp[,1:2], col = temp[,3]+1, pch = temp[,3]+1)


###################SMOTE DATASET3###########################
temp <- data3
over <- length(temp[,3])/sum(temp[,3])*100

plot(temp[,1:2], col = temp[,3]+1, pch = temp[,3]+1, main = "Dataset3", xlab = "", ylab = "")
syn <- ubSMOTE(as.data.frame(temp[,-3]), as.factor(temp[,3]), perc.over = over, perc.under = 0)
points(syn$X, col = 3, pch = 2)
points(temp[,1:2], col = temp[,3]+1, pch = temp[,3]+1)

###################SMOTE DATASET4###########################
temp <- data4
over <- length(temp[,3])/sum(temp[,3])*100

plot(temp[,1:2], col = temp[,3]+1, pch = temp[,3]+1, main = "Dataset4", xlab = "", ylab = "")
syn <- ubSMOTE(as.data.frame(temp[,-3]), as.factor(temp[,3]), perc.over = over, perc.under = 0)
points(syn$X, col = 3, pch = 2)
points(temp[,1:2], col = temp[,3]+1, pch = temp[,3]+1)

###################SMOTE DATASET5###########################
temp <- data5
over <- length(temp[,3])/sum(temp[,3])*100

plot(temp[,1:2], col = temp[,3]+1, pch = temp[,3]+1, main = "Dataset5", xlab = "", ylab = "")
syn <- ubSMOTE(as.data.frame(temp[,-3]), as.factor(temp[,3]), perc.over = over, perc.under = 0)
points(syn$X, col = 3, pch = 2)
points(temp[,1:2], col = temp[,3]+1, pch = temp[,3]+1)
