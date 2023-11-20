#This is the in class work from advanced clinical trials 08/28/2023

#1 Generate 100 dose-toxicity curves below
#Choose the MTD
#Study dose levels
stdolv <- c(0.10, 0.2, 0.25, 0.3, 0.4)
j <- c(1, 2, 3, 4, 5)
J <- 5


#Randomly sample M~Beta(max(J - j, 0.50, 1)) and set upper bound B = theta + (1 - theta) x M
j <- round(runif(100, 0.5, 5.49))

for(i in 1:100){
  m <- rbeta(100, max(J - j[i], 0.5), 1)
}

df <- as.data.frame(m)
df$j <- j

df$b <- 0.25 + (1 - 0.25) * df$m

probs <- data.frame(f1 = rep(0, 100), f2 = rep(0, 100), f3 = rep(0, 100), f4 = rep(0, 100), f5 = rep(0, 100))


for(i in 1:100){
  probs[i, ] <- sort(runif(J, 0, df$b[i]))
}

matplot(y = t(probs), type = "l")


probs[2, ] <- sort(runif(J, 0, df$b[2]))
