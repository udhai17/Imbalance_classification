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

########## CBSO ##########

result <- CBSO(data = data, target.column = 3, beta = 0.5, k = 15, c = 5)

d.synTotal <- result$d.synTotal
head(d.synTotal)

d.synIndvd <- result$d.synIndvd
d.synIndvd$`451`

draw <- function(i) {
  ind <- which(class == 1)
  ind <- ind[i]
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
for (i in 1:50) draw(i)
