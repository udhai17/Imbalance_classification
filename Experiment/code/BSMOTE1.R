BSMOTE1 <- function(data, target.column, m, k, s) {
  
  
  library(FNN)
  # target.column : column of the input data containing class labels
  # m : number of nearest neighbors for obtaining DANGER objects
  # k : number of nearest neighbors for reference objects
  # s : number of newly generated objects for each object in DANGER

  
  if (k < s) {
    stop("Error : s must be smaller than k")
  }
  
  
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
  
  
  nn <- get.knnx(data = data,
                 query = data[ind.mn,],
                 k = m,
                 algorithm = "kd_tree")$nn.index
  r <- apply(X = nn,
             MARGIN = 1,
             FUN = function(x) sum(x %in% ind.mj))
  r <- r/m
  
  
  ind.safe <- which(r < 0.5)
  ind.noisy <- which(r == 1)
  ind.danger <- ind.mn[-c(ind.safe, ind.noisy)]
  ind.safe <- ind.mn[ind.safe]
  ind.noisy <- ind.mn[ind.noisy]
  
  
  temp <- length(ind.danger)
  print(paste(temp, " (", (temp/n.mn)*100, "%) ",
              "samples are in DANGER", sep=""))
  temp <- temp * s
  print(paste(temp,"synthetic samples are generated"))
  
  
  d.synIndvd <- list()
  itr <- length(ind.danger)
  dangerNN <- get.knnx(data = data[ind.mn,],
                        query = data[ind.danger,],
                        k = k,
                        algorithm = "kd_tree")$nn.index
  for (i in 1:itr) {
    
    x <- ind.danger[i]
    x <- data[x,]
    
    myNN <- dangerNN[i,]
    myNN <- ind.mn[myNN]
    myNN <- sample(x = myNN, size = s)
    myNN <- data[myNN,]
    
    syn <- matrix(data = NA, nrow = 0, ncol = ncol(data))
    for (j in 1:s) {
      syn <- rbind(syn, x + (runif(1) * (myNN[j,] - x)))
    }
    d.synIndvd[[i]] <- syn
    
  }
  
  
  names(d.synIndvd) <- ind.danger
  d.synTotal <- matrix(data = unlist(d.synIndvd), ncol = ncol(data), byrow = T)
  status <- list()
  status[[1]] <- ind.danger
  status[[2]] <- ind.safe
  status[[3]] <- ind.noisy
  names(status) <- c("DANGER","SAFE","NOISY")
  result <- list(d.synTotal, d.synIndvd, status)
  names(result) <- c("d.synTotal", "d.synIndvd", "status")
  return(result)
  
  
}