SLSMOTE <- function(data, target.column, k) {
  
  
  library(FNN)
  # target.column : column of the input data containing class labels
  # k : number of nearest neighbors
  
  
  y <- data[ ,target.column]
  if (is.character(y) == TRUE) {
    y <- as.factor(y)
    y <- as.numeric(y)
  } else if (is.factor(y) == TRUE) {
    y <- as.numeric(y)
  }
  tmp <- table(y)
  n <- sum(tmp)
  if (length(tmp) != 2) {
    stop("Error : not for the binary classification")
  }
  
  
  y.mn <- which.min(tmp)
  n.mn <- tmp[y.mn]
  n.mj <- n - n.mn
  y.mn <- as.numeric(names(y.mn))
  ind.mn <- which(y == y.mn)
  ind.mj <- 1:n
  ind.mj <- ind.mj[-ind.mn]
  if (length(ind.mn) < k) {
    stop("Error : k must be smaller than the number of minority examples")
  }
  
  
  data <- data[,-target.column]
  data <- as.matrix(data)
  if (is.character(data) == TRUE) {
    stop("Error : non-numeric values in feature vectors")
  }
  if (sum(is.na(data)) != 0) {
    stop("Error : NAs in feature vector")
  }
  
  
  myNN <- get.knnx(data = data,
                   query = data[ind.mn,],
                   k = k,
                   algorithm = "kd_tree")$nn.index
  mySL <- apply(X = myNN,
                MARGIN = 1,
                FUN = function(x) sum(x %in% ind.mn))
  you <- apply(X = myNN,
               MARGIN = 1,
               FUN = function(x) sample(x = x, size = 1))
  youNN <- get.knnx(data = data,
                    query = data[you,],
                    k = k,
                    algorithm = "kd_tree")$nn.index
  youSL <- apply(X = youNN,
                 MARGIN = 1,
                 FUN = function(x) sum(x %in% ind.mn))
  
  
  d.synIndvd <- list()
  itr <- length(ind.mn)
  caseFlag <- rep(x = NA, itr)
  
  
  for (i in 1:itr) {
    
    denom <- youSL[i]
    num <- mySL[i]
    
    if (denom == 0) {
      
      if (num == 0) {
        caseFlag[i] <- 1
      } else {
        caseFlag[i] <- 2
        alpha <- 0
        syn <- data[ind.mn[i],] + alpha * (data[you[i],] - data[ind.mn[i],])
      }
      
    } else {
      
      if (num == denom) {
        caseFlag[i] <- 3
        alpha <- runif(n = 1, min = 0, max = 1)
        syn <- data[ind.mn[i],] + alpha * (data[you[i],] - data[ind.mn[i],])
      } else if (num > denom) {
        caseFlag[i] <- 4
        alpha <- runif(n = 1, min = 0, max = (denom/num))
        syn <- data[ind.mn[i],] + alpha * (data[you[i],] - data[ind.mn[i],])
      } else {
        caseFlag[i] <- 5
        alpha <- runif(n = 1, min = (1-(num/denom)), max = 1)
        syn <- data[ind.mn[i],] + alpha * (data[you[i],] - data[ind.mn[i],])
      }
      
    }
    
    d.synIndvd[[i]] <- syn
    
  }
  
  
  names(d.synIndvd) <- ind.mn
  d.synTotal <- matrix(data = unlist(d.synIndvd), ncol = ncol(data), byrow = T)
  result <- list(d.synTotal, d.synIndvd, caseFlag)
  names(result) <- c("d.synTotal", "d.synIndvd", "case")
  return(result)
  
  
}