# https://www.youtube.com/watch?v=ZLTyX4zL2Fc

answers <- c()
minRiver <- 1
maxRiver <- 10000
rivers <- minRiver:maxRiver
# steps <- read.csv("frog_problem.csv")

for (riverWidth in rivers) {
  numTests <- 1000000/riverWidth
  maxJumps <- riverWidth
  minJumps <- 1
  jumps <- 0
  # jumps <- c()
  for (test in 1:numTests) {
    totalDist <- 0
    for (jump in 1:maxJumps) {
      if (riverWidth - totalDist == 1) {
        jumpDist <- 1
      } else {
        jumpDist <- round(runif(1, minJumps, riverWidth - totalDist))
      }
      totalDist <- totalDist + jumpDist
      if (totalDist == riverWidth ) {
        # jumps <- c(jumps, jump)
        jumps <- jumps + jump
        break
      }
    }
  }
  print(jumps/numTests)
  # answers <- c(answers, jumps/numTests)
  # print(mean(jumps))
  # qplot(jumps, bins=max(jumps), xlim=c(0,10)) + scale_x_continuous(breaks=1:max(jumps)) + theme_bw()
}

steps <- data.frame(rivers, answers)
colnames(steps) <- c("river", "answer")

ggplot(steps, aes(river, answer)) + geom_point() + theme_bw()#+ geom_smooth(method='loess')
answers <- steps['answer'][[1]]
rivers <- steps['river'][[1]]
m<-nls(answers ~ a*rivers/(b+rivers), start = c(a=1, b=1))
cor(answers,predict(m))
plot(rivers,answers)
lines(rivers,predict(m),lty=2,col="red",lwd=3)

# write.csv(steps, file="frog_problem.csv", row.names=FALSE)
