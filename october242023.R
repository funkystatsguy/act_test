library(ph2bayes)
library(VGAM)
N <- 40
n <- 23
x <- 16
ae <- 0.6
be <- 0.4
ps <- 0.6
thetat <- 0.9

i <- 0:17

predprob(x, n, N, ae, be, ps, thetat)

indicator <- c(rep(0, 12), rep(1, 6))

pi <- dbetabinom.ab(i, 17, 16.6, 7.4)
round(pi, 4)
bi <- 1 - pbeta(0.6, 16.6 + i, 24.4 - i)

pps <- (indicator * pi)
ppsd <- (pi * bi)



sum(pps)/sum(ppsd)

