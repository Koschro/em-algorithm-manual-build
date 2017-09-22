library(ggplot2)

set.seed(1)
m1 <-matrix(rbinom(10,1,0.75),5,10)
m2 <-matrix(rbinom(10,1,0.5),5,10)
m<-rbind(m1,m2)
#m

y<-apply(m, 1, function(x) sum(x[x==1]))
#y

headsProbA <- sum(y[1:5])/50
headsProbB <- sum(y[6:10])/50
headsProb <- c(headsProbA, headsProbB)

coinLikelihood	<- function(v,headsProb){
  likelihood <- 1
  x <- dbinom(v,1,headsProb)
  for (i in 1:length(x)){
    likelihood <- likelihood*x[i]
  }
  return(likelihood)
}

#coinLikelihood(v,headsProb)

prob <- seq(0,1,0.01)
#i
v1	<- c(0,1,0,1,0,0,1,1)
p <- qplot(prob,sapply(prob, function (prob) {
  coinLikelihood(v1, prob)
}))
p + labs(title = "Likelihood Plot", y = "Likelihood function")
#ii
v2	<- c(1,1,1,1,0,0,1,1,1,0)
p <- qplot(prob, sapply(prob, function (prob) {
  coinLikelihood(v2, prob)
}))
p + labs(title = "Likelihood Plot2", y = "Likelihood function")
#iii
v3  <- c(0,0,0,0,0,0,0)
p <- qplot(prob, sapply(prob, function (prob) {
  coinLikelihood(v3, prob)
}))
p + labs(title = "Likelihood Plot3", y = "Likelihood function")

coinEStep	<- function	(m,	headsProbA,	headsProbB)	{
  estep_probA <- c()
  for (i in 1:nrow(m)){
    estep_probA <- c(estep_probA,(coinLikelihood(m[i,],headsProbA)/ (coinLikelihood(m[i,],headsProbA) + coinLikelihood(m[i,],headsProbB))))
  }
  return(estep_probA)
}
#coinEStep(m,headsProbA,headsProbB)

estep_probA <- c()
estep_probB <- 1- estep_probA

coinMStep	<- function(m,	relativeProbsForCoinA) {
  
  probA <- sum((apply(m,1,sum)) * relativeProbsForCoinA)/sum((apply(m,1,length)) * relativeProbsForCoinA)  
  probB <- sum((apply(m,1,sum)) * (1 - relativeProbsForCoinA) )/sum((apply(m,1,length)) * (1 - relativeProbsForCoinA))
  totalProb <- c(probA,probB)
  
  return(totalProb)  
}
#relativeProbsForCoinA<-coinEStep(m,headsProbA,headsProbB)
#coinMStep(m,relativeProbsForCoinA)

#Some numerical testing to check the functions
#x<-1
#y<-1.0001
#e<-0.001
#all.equal(x,y,tolerance = e)

compared <- function(x,y,e){abs(x-y)<=e}

coinTossEM <- function(m,coinProbsA,coinProbsB,e){
  coinProbA <- c()
  coinProbB <- c()
  coinProbA[1] <- 1
  coinProbB[1] <- 1
  coinProbA[2] <- coinProbsA
  coinProbB[2] <- coinProbsB
  i <- 2
  while(!((compared(coinProbA[i],coinProbA[i-1],e))&(compared(coinProbB[i],coinProbB[i-1],e)))){
    relativeProbsForCoinA <- coinEStep (m,coinProbA[i],coinProbB[i])
    final <- coinMStep(m, relativeProbsForCoinA)
    coinProbA[i+1] <- final[1]
    coinProbB[i+1] <- final[2]
    i <- i+1
  }  
  return(final)  
}


coinTossEM(m,0.5,0.5,0.1)
coinTossEM(m,0.75,0.5,0.01)

#The starting propabilities that would create that,
#when provided to the function	we wrote, will cause problems	with	the	algorithm	so that it will not	work
#as intended are the probalities that are equal for the two coins.The algorithm's output would be equal propabilities. 

#coinTossEM(m,0.5,0.5,0.1)

