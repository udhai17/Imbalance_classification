CBSO <- function(data, target.column, beta, k, c) {
  
  
  library(FNN)
  # target.column : column of the input data containing class labels
  # beta : the desired balance level (1 means fully balance)
  # k : number of nearest neighbors
  # c : a constant for clustering
  
  
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
  
  
  data <- data[,-target.column]
  data <- as.matrix(data)
  if (is.character(data) == TRUE) {
    stop("Error : non-numeric values in feature vectors")
  }
  if (sum(is.na(data)) != 0) {
    stop("Error : NAs in feature vector")
  }
  
  
  g <- (n.mj - n.mn)*beta
  nn <- get.knnx(data = data,
                 query = data[ind.mn,],
                 k = k,
                 algorithm = "kd_tree")$nn.index
  r <- apply(X = nn,
             MARGIN = 1,
             FUN = function(x) sum(x %in% ind.mj))
  r <- r/k
  r <- r/sum(r)
  g <- round(x = r*g, digits = 0)
  print(paste(sum(g),"synthetic data will be generated"))
  
  
  model.cluster <- hclust(d = dist(data[ind.mn,]),
                          method = "average")
  d <- mean(get.knn(data = data[ind.mn,],
                    k = 1,
                    algorithm = "kd_tree")$nn.dist)
  cluster <- cutree(tree = model.cluster,
                    h = d*c)
  print(paste("The number of cluster is",max(cluster)))
  
  
  d.synIndvd <- list()
  itr <- length(ind.mn)
  for (i in 1:itr) {
    
    me <- data[ind.mn[i],]
    myCluster <- cluster[i]
    
    myRef <- ind.mn[which(cluster == myCluster)]
    myRef <- data[myRef,]
    
    if (is.vector(myRef) == TRUE) {
      syn <- matrix(data = me, nrow = g[i], ncol = length(me), byrow = TRUE)
    } else {
      syn <- matrix(data = NA, nrow = 0, ncol = length(me), byrow = TRUE)
      for (j in 1:g[i]) {
        ind.ref <- sample(x = 1:nrow(myRef), size = 1)
        syn <- rbind(syn, me + ((myRef[ind.ref,] - me)*runif(1)))
      }
    }
    d.synIndvd[[i]] <- syn
    
  }
  
  
  names(d.synIndvd) <- ind.mn
  d.synTotal <- matrix(data = unlist(d.synIndvd), ncol = ncol(data), byrow = T)
  result <- list(d.synTotal, d.synIndvd)
  names(result) <- c("d.synTotal", "d.synIndvd")
  return(result)
  
  
}