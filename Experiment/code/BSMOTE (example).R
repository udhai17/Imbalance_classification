########## Example dataset ##########

d.mj <- cbind(rnorm(n = 450, mean = 0, sd = 1), rnorm(n = 450, mean = 0, sd = 1))
d.mn1 <- cbind(rnorm(n = 25, mean = 2, sd = 1), rnorm(n = 25, mean = 0, sd = 1))
d.mn2 <- cbind(rnorm(n = 25, mean = 0, sd = 1), rnorm(n = 25, mean = 2, sd = 1))
class <- c(rep(0, nrow(d.mj)), rep(1, nrow(rbind(d.mn1,d.mn2))))
data <- rbind(d.mj, d.mn1, d.mn2)
data <- data.frame(x1 = data[,1], x2 = data[,2], class = class)
rm(d.mj,d.mn1,d.mn2)
plot(data[,-3], col = class+1, pch = class+1, cex = class+1)
legend("topright", c("Majority","Minority"), pch = c(1,2), col = c(1,2), bty = "n")

########## BSMOTE1 ##########

result <- BSMOTE1(data = data, target.column = 3, m = 4, k = 10, s = 5)

d.synTotal <- result$d.synTotal
head(d.synTotal)

d.synIndvd <- result$d.synIndvd
d.synIndvd$`451`

status <- result$status
status
d.danger <- data[status$DANGER,]
d.safe <- data[status$SAFE,]
d.noisy <- data[status$NOISY,]

points(x = d.danger[,1], y = d.danger[,2], pch = "D", cex = 2, col = 2)
points(x = d.safe[,1], y = d.safe[,2], pch = "S", cex = 2, col = 4)
points(x = d.noisy[,1], y = d.noisy[,2], pch = "N", cex = 2, col = 3)

draw <- function(i) {
  ind <- status$DANGER[i]
  points(x = d.synIndvd[[i]][,1], y = d.synIndvd[[i]][,2], pch = 15, col = 3, cex = 2)
  points(x = data[ind,1], y = data[ind,2], pch = 17, col = 4, cex = 2)
}

plot(data[,-3], col = class+1, pch = class+1, cex = class+1)
legend("topright", c("Majority","Minority"), pch = c(1,2), col = c(1,2), bty = "n")
draw(1)
draw(2)
draw(3)

plot(data[,-3], col = class+1, pch = class+1, cex = class+1)
legend("topright", c("Majority","Minority"), pch = c(1,2), col = c(1,2), bty = "n")
for (i in 1:nrow(d.danger)) draw(i)

########## BSMOTE2 ##########

result <- BSMOTE2(data = data, target.column = 3, m = 4, k = 10, s = 5)

d.synTotal <- result$d.synTotal
head(d.synTotal)

d.synIndvdMnr <- result$d.synIndvdMnr
d.synIndvdMnr$`451`

d.synIndvdMjr <- result$d.synIndvdMjr
d.synIndvdMnr$`451`

status <- result$status
status
d.danger <- data[status$DANGER,]
d.safe <- data[status$SAFE,]
d.noisy <- data[status$NOISY,]

plot(data[,-3], col = class+1, pch = class+1, cex = class+1)
legend("topright", c("Majority","Minority"), pch = c(1,2), col = c(1,2), bty = "n")
points(x = d.danger[,1], y = d.danger[,2], pch = "D", cex = 2, col = 2)
points(x = d.safe[,1], y = d.safe[,2], pch = "S", cex = 2, col = 4)
points(x = d.noisy[,1], y = d.noisy[,2], pch = "N", cex = 2, col = 3)

drawMinor <- function(i) {
  ind <- status$DANGER[i]
  points(x = d.synIndvdMnr[[i]][,1], y = d.synIndvdMnr[[i]][,2], pch = 15, col = 3, cex = 2)
  points(x = data[ind,1], y = data[ind,2], pch = 17, col = 4, cex = 2)
}

drawMajor <- function(i) {
  ind <- status$DANGER[i]
  points(x = d.synIndvdMjr[[i]][,1], y = d.synIndvdMjr[[i]][,2], pch = 16, col = 5, cex = 2)
  points(x = data[ind,1], y = data[ind,2], pch = 17, col = 4, cex = 2)
}

plot(data[,-3], col = class+1, pch = class+1, cex = class+1)
legend("topright", c("Majority","Minority"), pch = c(1,2), col = c(1,2), bty = "n")
drawMajor(1)
drawMinor(1)
drawMajor(2)
drawMinor(2)

plot(data[,-3], col = class+1, pch = class+1, cex = class+1)
legend("topright", c("Majority","Minority"), pch = c(1,2), col = c(1,2), bty = "n")
for (i in 1:nrow(d.danger)) {
  drawMajor(i)
  drawMinor(i)
}