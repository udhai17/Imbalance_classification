DBSMOTE <- function(data, eps, minpts) {
  
  
  ########## input ##########
  
  
  # data : A cluster of minor objects without class labels
  # eps : From DBSCAN setting
  # minpts : From DBSCAN setting
  
  
  ########## sub-function ##########
  
  
  EpsNeighbor <- function(data, eps) {
    
    dist <- as.matrix(dist(data))
    epsNN <- dist < eps
    diag(epsNN) <- F
    
    tmp <- list()
    itr <- nrow(data)
    for (i in 1:itr) {
      tmp[[i]] <- as.numeric(which(epsNN[i,] == T))
    }
    epsNN <- tmp
    
    result <- list(dist, epsNN)
    names(result) <- c("dist", "epsNN")
    return(result)
    
  }
  
  DijkstraTree <- function(cntr, adjMat) {
    
    cost <- rep(x = Inf, length = n)
    cost[cntr] <- 0
    
    prev <- rep(x = NA, length = n)
    prev[cntr] <- 0
    
    vertexSet <- 1:nrow(adjMat)
    
    loopTest <- length(vertexSet) != 0
    loopFlag <- loopTest
    
    while (loopFlag == TRUE) {
      
      u <- which.min(cost[vertexSet])
      u <- vertexSet[u]
      ind <- which(vertexSet == u)
      vertexSet <- vertexSet[-ind]
      
      nn <- which(adjMat[u,] != 0)
      
      for (i in nn) {
        alt <- cost[u] + adjMat[u,i]
        if (alt < cost[i]) {
          cost[i] <- alt
          prev[i] <- u
        }
      }
      
      loopTest <- length(vertexSet) != 0
      loopFlag <- loopTest
      
    }
    
    return(prev)
    
  }
  
  
  ########## main ##########
  
  
  epsNN <- EpsNeighbor(data = data, eps = eps)
  dist <- epsNN$dist
  epsNN <- epsNN$epsNN
  
  n <- nrow(data)
  adjMat <- matrix(data = 0, nrow = n, ncol = n)
  for (i in 1:n) {
    nn <- epsNN[[i]]
    test <- length(nn)
    if (test >= minpts) {
      adjMat[i,nn] <- dist[i,nn]
      adjMat[nn,i] <- dist[i,nn]
    }
  }
  
  cntr <- matrix(data = colMeans(data), nrow = 1)
  cntr <- get.knnx(data = data, query = cntr,
                   k = 1, algorithm = "kd_tree")$nn.index
  
  prev <- DijkstraTree(cntr = cntr, adjMat = adjMat)
  
  d.synt <- matrix(nrow = n, ncol = ncol(data))
  
  for (i in 1:n) {
    
    if (i != cntr) {
      
      path <- c()
      path[1] <- i
      j <- prev[i]
      loopTest <- j != cntr
      while (loopTest == TRUE) {
        path <- c(path, j)
        j <- prev[j]
        loopTest <- j != cntr
      }
      path <- c(path, cntr)
      
      edge <- length(path) - 1
      edge <- sample(x = 1:edge, size = 1)
      
      rnd <- runif(1)
      x <- data[path[edge],]
      y <- data[path[edge + 1],]
      diff <- y - x
      new <- x + (rnd * diff)
      
      d.synt[i,] <- as.numeric(new)
      
    }
    
  }
  
  d.synt <- d.synt[-cntr,]
  
  return(d.synt)
  
}