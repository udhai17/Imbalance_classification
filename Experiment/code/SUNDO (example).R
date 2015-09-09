data <-iris
data <- data[data[,5]=="setosa"|data[,5]=="versicolor",]
data <- data[-(57:100),]
data[,5] <- as.factor(as.character(data[,5]))
pc <- prcomp(data[,-5])
data <-cbind(pc$x[,c(2,3)],data[,5])


##########################################################

aburl = 'http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data'
abnames = c('sex','length','diameter','height','weight.w','weight.s','weight.v','weight.sh','rings')
abalone = read.table(aburl, header = F , sep = ',', col.names = abnames)
data <- abalone[abalone[,9]==18|abalone[,9]==9,]
pc <- prcomp(data[,-c(1,9)])
data <- cbind(pc$x[,1:2],data[,9])
data[,3] <- as.factor(data[,3])

#########################################################






library("ggplot2")

n.data <- SUNDO(data = data,l = 3,k1 = 0.5)

data <- as.data.frame(data)
ggplot()+
  geom_point(mapping = aes(x = n.data$OversampledSet[,1],n.data$OversampledSet[,2]),col=3,size=4)+
  geom_point(mapping = aes(data[as.numeric(data[,3])==2,1],data[as.numeric(data[,3])==2,2]),col=2 ,pch=16,size=4)+
  geom_point(mapping = aes(x = n.data$UndersampledSet[,1],n.data$UndersampledSet[,2]),pch=16,size=4)+
  geom_point(mapping = aes(x = n.data$elim[,1],n.data$elim[,2]),pch=13,size=4)

ggplot()+
  geom_point(mapping = aes(x = n.data$OversampledSet[,1],n.data$OversampledSet[,2]),col=3,size=4)+
  geom_point(mapping = aes(x = n.data$UndersampledSet[,1],n.data$UndersampledSet[,2]),col=2 ,pch=16,size=4)