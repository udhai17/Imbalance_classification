## 결과물: 행렬 6개가 들어있는 array가 만들어지며, 
## 각 매트릭스의 행은 500개(class1은 400개, class2는 100개)
## 각 매트릭스의 열은 3개(1열:x좌표, 2열:y좌표, 3열:class)
## 행렬1는 0%중첩, 행렬2는 20%중첩, 행렬3은 40%, 행렬4는 60%, 행렬5는 80%, 행렬6은 100%중첩

over <- array(0, dim=c(500,3,6))

for (i in 1:6) {
  p <- 10*i-10
  q <- 10*i+40
  over[1:400,1,i] <- matrix(runif(400, min = p, max = q))
  over[1:400,2,i] <- matrix(runif(400, min = 0, max = 100))
  over[401:500,1,i] <- matrix(runif(100, min = 50, max = 100))
  over[401:500,2,i] <- matrix(runif(100, min = 0, max = 100))
  over[1:400,3,i] <- 1
  over[401:500,3,i] <- 2
  r <- 20*i-20
  write.csv (over[,,i], file=paste("overlap",as.character(r),"%",".csv",sep=""))
}


##  밑에는 참고삼아 그려본 것 ##
plot(over[100:400,1:2,2], main = "Overlap 20%", pch=16, xlim = c(0,100), ylim=c(0,100))
points(over[401:500,1,2],over[401:500,2,2],pch=17, col="red")
