#SBC
#Undersampling
#m: minor를 1로 놓았을때
#   major의 비율(m:1)
#k: 클러스터 수

SBC <- function(data,target.column,m,k){
  km <- kmeans(data,k)

  ratio <- vector('numeric',k)
  for(i in 1:k){
    ratio[i] <- nrow(data[km$cluster==i&data[,target.column]==0,])/nrow(data[km$cluster==i&data[,target.column]==1,])
    if(nrow(data[km$cluster==i&data[,target.column]==1,])==0)
      ratio[i] <- nrow(data[km$cluster==i&data[,target.column]==0,])/1
  }
  samNum <- m*nrow(data[data[,target.column]==1,])*ratio/sum(ratio)
  
  
  UnderSampledSet <- matrix(data = NA,nrow = 0,ncol = ncol(data))
  for(i in 1:k){
    nR <- nrow(data[km$cluster==i&data[,target.column]==0,])
    sam <-sample(1:nR,size = round(samNum[i]))
    UnderSampledSet <- rbind(UnderSampledSet,data[km$cluster==i&data[,target.column]==0,][sam,])
  }
  return(rbind(data[data[,target.column]==1,],UnderSampledSet))
}

d <- read.csv("dataset3.csv",h=T)
plot(d[,1:2],col=d[,3]+1)
res <- SBC(data = d,target.column = 3,m = 3,k = 4)
plot(res[,1:2],col=res[,3]+1)
