#In class work from 8/30/2023
library(UBCRM)

cdat <- CreData(4, c("D1", "D2", "D3", "D4"))
cdat

udat <- updata(cdat, lastdose = 1, npt = 3, ndlt = 0)

nextdose <- troisPtrois(udat, 1)
nextdose

pis <- c(0.02, 0.04, 0.10, 0.25)

sim1 <- sim3p3(pis, seed = 84302)
sim1

sim2 <- ssim3p3(pis, n = 1000, seed = 84302)
sim2


#Biased Coin Design;
pis <- c(0.02, 0.04, 0.10, 0.25, 0.50)
lvls <- c(1, 2, 3, 4, 5)


j <- 1
theta <- 0.25
ph <- theta/ (1 - theta)
vec <- as.vector(0)
dltvec <- as.vector(0)



bcd <- function(j = 3, n = 20, theta = 0.25, pis = c(0.02, 0.04, 0.10, 0.25, 0.50)){
  lvls <- 1:length(pis)
  maxlvl <- max(lvls)
  ph <- theta / (1 - theta)
  vec <- as.vector(0)
  dltvec <- as.vector(0)
  
  for(i in 1:n){
  dltval <- runif(1)
  toxval <- dltval < pis[j]
  dltvec[i] <- toxval
  vec[i] <- j
  j <- ifelse(toxval == F, ifelse(runif(1) < ph, min(j + 1, maxlvl), j) , max(j - 1, 1))
  }
  return(data.frame(vec, dltvec))
}


bcd()

hist(replicate(1000, bcd()))
