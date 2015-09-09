library("FNN")
library("e1071")


BOS <- function(training,index.col,N,m){# N must less then minority class number*100 
  print(sum(training[,index.col]==1)*100)
  if(N >= sum(training[,index.col]==1)*100)
    stop("N must less then minority class number*100 ")
  
  model <- svm(V3~.,data = training)
  SV <- intersect(model$index,(1:nrow(training))[training[,index.col]==1])
  
  Tn <- round(N/100*sum(training[,index.col]==1))
  k <- ceiling(N/100)
  
  knn <- knnx.index(training[,-index.col],training[SV,-index.col],m+1)[,-1]
  knn_min <- knn.index(training[training[,index.col]==1,-index.col],k)
  
  x_new <- matrix(NA,0,ncol(data)-1)
  for(i in 1:length(SV)){
    if(sum(knn[i,]%in% (1:nrow(training))[-SV]) < m/2){
      for(j in 1:(Tn/length(SV))){
        x_new <- rbind(x_new,training[SV[i],-index.col]+runif(1)*(training[SV[i],-index.col]-training[training[,index.col]==1,-index.col][knn_min[i,j],]))
      }
    }
    else{
      for(j in 1:(Tn/length(SV))){
        x_new <- rbind(x_new,training[SV[i],-index.col]+runif(1)*(training[training[,index.col]==1,-index.col][knn_min[i,j],]-training[SV[i],-index.col]))
      }
    }
  }
  new <- data.frame(x_new,1)
  colnames(new) <- colnames(training)
  ret <- list()
  ret$origin <- training
  ret$new <- new
  return(ret)
}


data <- read.csv("dataset5.csv",h=T)

part <- STRPart(data,3,0.75)
training <- part[[1]]
test <- part[[2]]

newdata <- BOS(training = training,index.col = 3,N = 300,m = 20)$new

plot(data[,-3])
points(data[data[,3]==1,-3],col=2)
points(newdata,col=3)
