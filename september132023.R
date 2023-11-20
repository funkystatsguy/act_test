library(dfcrm)

#Bayesion, X0 is starting dose level, regardless of skeleton
#MLE = seqience of tried dose levels until you hit DLT)
PI <- c(0.02, 0.04, 0.10, 0.25, 0.5)
prior <- c(0.05, 0.12, 0.25, 0.40, 0.55)
target <- 0.25
N <- 20
x0 <- 3
nsim <- 1000
#Problem 2
crmsim(PI, prior, target, N, x0, nsim, scale = sqrt(1.5))

#Problem 3
crmsim(PI, prior, target, N, x0, nsim, model = "logistic")
