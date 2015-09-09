AhcOver <- function(data) {
  
  rownames(data) <- 1:nrow(data)
  
  # data : a set of minority objects without their labels
  # Subfunction : SingleLink, CompleteLink
  
  SingleLink <- function(oldDist) {
    
    merge <- which(oldDist == min(oldDist), arr.ind = TRUE)
    merge <- merge[1,]
    
    newLabel <- paste(rownames(oldDist)[merge[1]],
                      ",",
                      rownames(oldDist)[merge[2]],
                      sep = "")
    tmp <- rownames(oldDist)
    tmp[merge[1]] <- newLabel
    newDist <- oldDist
    rownames(newDist) <- tmp
    colnames(newDist) <- tmp
    
    ind <- 1:nrow(oldDist)
    ind <- ind[-merge]
    for (i in ind) {
      col <- merge[1]
      row <- i
      newValue <- max(oldDist[i,merge])
      newDist[row,col] <- newValue
      newDist[col,row] <- newValue
    }
    
    newDist <- newDist[-merge[2],-merge[2]]
    
    result <- list(newLabel, newDist)
    names(result) <- c("newLabel", "newDist")
    
    return(result)
    
  }
  
  CompleteLink <- function(oldDist) {
    
    merge <- which(oldDist == min(oldDist), arr.ind = TRUE)
    merge <- merge[1,]
    
    newLabel <- paste(rownames(oldDist)[merge[1]],
                      ",",
                      rownames(oldDist)[merge[2]],
                      sep = "")
    tmp <- rownames(oldDist)
    tmp[merge[1]] <- newLabel
    newDist <- oldDist
    rownames(newDist) <- tmp
    colnames(newDist) <- tmp
    
    ind <- 1:nrow(oldDist)
    ind <- ind[-merge]
    for (i in ind) {
      col <- merge[1]
      row <- i
      newValue <- min(oldDist[i,merge])
      newDist[row,col] <- newValue
      newDist[col,row] <- newValue
    }
    
    newDist <- newDist[-merge[2],-merge[2]]
    
    result <- list(newLabel, newDist)
    names(result) <- c("newLabel", "newDist")
    
    return(result)
    
  }
  
  clusterResult <- list()
  
  oldDist <- dist(data)
  oldDist <- as.matrix(oldDist)
  diag(oldDist) <- Inf
  
  loopFlag <- TRUE
  i <- 1
  
  while(loopFlag == TRUE) {
    
    test1 <- i%%2
    if (test1 != 0) {
      result <- SingleLink(oldDist = oldDist)
      clusterResult[[i]] <- result$newLabel
      oldDist <- result$newDist
    } else {
      result <- CompleteLink(oldDist = oldDist)
      clusterResult[[i]] <- result$newLabel
      oldDist <- result$newDist
    }
    
    test2 <- is.vector(oldDist)
    if (test2 == TRUE) {
      loopFlag <- FALSE
    } else {
      i <- i + 1
    }
    
  }
  
  StringSplit <- function(x) {
    y <- strsplit(x = x, split = ",")
    y <- unlist(y)
    y <- as.numeric(y)
    return(y)
  }
  
  clusterResult <- lapply(X = clusterResult, FUN = StringSplit)
  
  centroid <- lapply(X = clusterResult,
                     FUN = function(x) {colMeans(data[x,])})
  centroid <- matrix(data = unlist(centroid), ncol = ncol(data), byrow = TRUE)
  
  return(centroid)
  
}