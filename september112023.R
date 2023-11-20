#In CLass Work for MTPI and MTPI2 (Keyboard);
#MTPI
theta <- 0.3
ep <- 0.05
a <- 0.5
b <- 0.5
succ <- 1
fail <- 3

pbeta(theta - ep, a + succ, b + fail) / 0.25
(pbeta(theta + ep, a + succ, b + fail) - pbeta(theta - ep, a + succ, b + fail)) / 0.1
(1 - pbeta(theta + ep, a + succ, b + fail)) / 0.65


#MTPI2
a <- 1
b <- 1
succ <- 3
fail <- 7

keyleng <- 2 * ep

100 %% keyleng

pbeta(theta - ep, a + succ, b + fail) - pbeta(theta - 2 * ep, a + succ, b + fail)
pbeta(theta + ep, a + succ, b + fail) - pbeta(theta - ep, a + succ, b + fail)
pbeta(theta + 2 * ep, a + succ, b + fail) - pbeta(theta + ep, a + succ, b + fail)


