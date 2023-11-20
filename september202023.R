#In class September 20
library(dfcrm)
p0 <- c(0.05, 0.12, 0.25, 0.40, 0.55)
tau <- 126
theta <- 0.26
y <- c(0, 0, 0, 1)
x <- c(3, 3, 3, 3)
difftime("2023-03-19", "2023-03-12")
ui <- c(72, 65, 35, 28)
wt <- ui/tau
titecrm(p0, theta, y, x, weight = wt)

#Simulation
pi <- c(0.02, 0.04, 0.10, 0.25, 0.40)
prior <- c(0.05, 0.12, 0.25, 0.40, 0.55)
target <- 0.25
x0 <- 3
n <- 30
obswin <- 6
rate <- 4

titesim(pi, prior, target, n, x0 = x0, nsim = 200, restrict = T, obswin = obswin, 
        rate = rate, accrual = "poisson", surv = 'uniform', scheme = "linear", count = T,
        method = "bayes", model = "empiric", scale = sqrt(1.34))
