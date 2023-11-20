#Assignment 2 R Code;
library(dfcrm)
#Question 1
#Set seed;
set.seed(643)
#Get Set of latent tolerances;
lattol1 <- round(as.vector(runif(20)), 3)
#Set good prior
prior1 <- round(getprior(0.05, 0.25, 3, 4), 3)
#Set target
theta1 <- 0.25
#Set true probabilities
pis1 <- c(0.05, 0.12, 0.25, 0.40)
#Tox vector
tox1 <- c(0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0)
#Dose Levels
lvls1 <- c(1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 2, 2, 3, 3, 2, 2)
#CRM
crm(prior1, theta1, tox1, lvls1, method = "bayes", model = "empiric")

#Question 2: see "funk_CRM_simulation_edit.R"

#Question 3
#part a
#Get titeCRM next assignment;
#Skeleton
skel3 <- c(0.05, 0.12, 0.25, 0.40, 0.55)
#Target
theta3 <- 0.25
#DLTs
dlt3 <- c(0, 0, 0, 1, rep(0, 11))
#Set Levels;
lvls3 <- c(1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2)
#Set length of time in study;
length3 <- c(90, 90, 90, 30, rep(90, 7), 75, 60, 45, 30)
#Total length of interval;
totleng3 <- 90
#Set Weight
weight3 <- length3 / totleng3 
#titeCRM function
titecrm(prior = skel3, target = theta3, tox = dlt3, level = lvls3, 
        weight = weight3, scale = sqrt(1.34))

#part b
toxicity_risk3 <- ((1 - weight3[12:15]) * 0.099) / (1 - weight3[12:15] * 0.099)

#Question 4
set.seed(62)
#Get set of latent tolerences;
lattol4 <- round(runif(36), 3)
#If runif is greater than 0.5, escalate A,
#else if runif is less than 0.5, escalate B
random1 <- runif(1)
ifelse(random1 > 0.5, "A", "B")

