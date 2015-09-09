data <- read.csv(file = "Complex_border.csv", header = T)
plot(data[,-3], col = data[,3]+1)

minorClass <- table(data[,3])
tmp <- which.min(minorClass)
minorClass <- as.numeric(rownames(minorClass)[tmp])
majorClass <- which(data[,3] != minorClass)
minorClass <- which(data[,3] == minorClass)
rm(tmp)

d.SyntMinor <- AhcOver(data = data[minorClass,-3])
points(x = d.SyntMinor[,1], y = d.SyntMinor[,2], pch = "*", col = 3)

d.SyntMajor <- KmeansUnder(data = data[majorClass,-3], k = length(minorClass))
points(x = d.SyntMajor[,1], y = d.SyntMajor[,2], pch = "*", col = 4)