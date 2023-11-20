#In Class work for September 18th
library(dfcrm)

#Set prior
prior <- c(0.05, 0.12, 0.25, 0.40, 0.55)
prior <- getprior(0.05, 0.25, 3, 5)
#Set target rate
theta <- 0.25
#True Values
pis <- c(0.02, 0.04, 0.10, 0.25, 0.5)
#DLTs
tox <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
#Dose Levels;
lev <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4)
#Look at CRM
crm(prior, theta, tox, lev, method = "mle", model = "empiric")

prior ** exp(0.439)

library(Iso)
pava(c(2/9, 0/3, 1/6), w = c(9, 3, 6))        
