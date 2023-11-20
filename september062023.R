library(BOIN)

#Question 1
get.boundary(target = 0.3, ncohort = 10, cohortsize = 3)



#Question 4
get.boundary(target = 0.3, ncohort = 10, cohortsize = 3, p.saf = 0.4*0.3, p.tox = 1.6*0.3)
get.oc(0.25, p.true = c(0.02, 0.04, 0.10, 0.25, 0.40), ncohort = 10, cohortsize = 3, ntrial = 5000)


chisq.test()