SUNDO <- function(data,l,k1,beta = 0.05,alpha = 10){
  library("MASS")
  STRPart <- function(dt, r, p){  
    dt <- as.data.frame(dt)
    dt[,r] <- as.factor(dt[,r]) 
    n.class <- length(levels(dt[,r])) 
    dt.lst <- vector("list",n.class) 
    ndt.lst <- vector("list",n.class) 
    dt.trn.lst <-vector("list",n.class)  
    dt.tst.lst <- vector("list",n.class)  
    
    i <- 1     
    while(i <= n.class){   
      dt.lst[[i]] <- dt[dt[,r]==levels(dt[,r])[i],] 
      ndt.lst[[i]] <- nrow(dt.lst[[i]])             
      dt.lst[[i]] <- dt.lst[[i]][sample(ndt.lst[[i]]),]  
      dt.trn.lst[[i]] <- dt.lst[[i]][1:round(ndt.lst[[i]]*p),] 
      dt.tst.lst[[i]] <- dt.lst[[i]][(round(ndt.lst[[i]]*p)+1):ndt.lst[[i]],]    
      i <- i+1 
    }    
    j <- 2 
    d.train <- dt.trn.lst[[1]] 
    d.test <- dt.tst.lst[[1]]     
    while(j <= n.class){ 
      b <- dt.trn.lst[[j]]    
      d.train <- rbind(d.train,b)  
      c <- dt.tst.lst[[j]]     
      d.test <- rbind(d.test,c)
      j <- j+1                 
    }  
    return(list(d.train,d.test)) 
  }
  part <-STRPart(data,l,0.75)
  
  training <-part[[1]]
  training[,3] <- as.numeric(training[,3])
  
  n0 <- max(table(training[,l]))  #majority
  n1 <- min(table(training[,l]))  #minority
  
  k1 <- k1 #minority target
  k0 <- 1-k1 #majority target
  
  N <- round(k1*n0-k0*n1)
  
  mat.train.maj <- as.matrix(training[training[,l]==which.max(table(data[,l])),-l])
  mat.train.min <- as.matrix(training[training[,l]==which.min(table(data[,l])),-l])
  
  Kr <- round(alpha*N/n1)
  
  syn <- matrix(NA,0,ncol(data)-1)
  for(i in 1:n1){
    syn <- rbind(syn,mvrnorm(Kr,mat.train.min[i,],beta*diag(apply(mat.train.min,2,sd))))
  }
  
  
  mat.train.min <- rbind(mat.train.min,
                         syn[order(apply(t(apply(syn,1,function(x){sqrt(rowSums((x-mat.train.maj)^2))})),1,min),decreasing = T)[1:N],])
  
  ##############################################################
  pi <- apply(X = mat.train.maj,MARGIN = 2,function(x) {x/max(x)})
  d <- as.matrix(dist(pi))
  d[lower.tri(d,diag = T)] <-NA
  d <- arrayInd(order(d)[1:N],.dim = dim(d))
  
  elim <- c()
  for(i in 1:N){
    s <- sample((ncol(data)-1),1)
    if(sum(d[i,])==0)
      next
    elim <- c(elim,d[i,s])
    d[i,] <-0
    d[d%in%elim]<-0
  }
  elim <- unique(elim)[unique(elim)!=0]
  ##############################################################
  returns <- list()
  returns$OversampledSet  <- mat.train.min
  returns$UndersampledSet <- mat.train.maj[-elim,]
  returns$testset <- part[[2]]
  returns$elim <- mat.train.maj[elim,]
  
  print(paste(as.character(N),"object Undersampled and Oversampled"))
  return(returns)
}
