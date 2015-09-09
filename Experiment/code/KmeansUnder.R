KmeansUnder <- function(data, k) {
  centroid <- kmeans(x = data, centers = k)$centers
  return(centroid)
}