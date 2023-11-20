function (prior, target, tox, level, n = length(level), dosename = NULL, 
          include = 1:n, pid = 1:n, conf.level = 0.9, method = "bayes", 
          model = "empiric", intcpt = 3, scale = sqrt(1.34), model.detail = TRUE, 
          patient.detail = TRUE, var.est = TRUE) 
{
  y1p <- tox[include]
  w1p <- rep(1, length(include))
  if (model == "empiric") {
    dosescaled <- prior
    x1p <- prior[level[include]]
    if (method == "mle") {
      if (sum(y1p) == 0 | sum(y1p) == length(y1p)) 
        stop(" mle does not exist!")
      est <- optimize(lcrm, c(-10, 10), x1p, y1p, w1p, 
                      tol = 1e-04, maximum = TRUE)$max
      if (var.est) {
        e2 <- integrate(crmht2, -100, 100, x1p, y1p, 
                        w1p, 500, abs.tol = 0)[[1]]/integrate(crmh, 
                                                              -10, 10, x1p, y1p, w1p, 500, abs.tol = 0)[[1]]
      }
    }
    else if (method == "bayes") {
      den <- integrate(crmh, -Inf, Inf, x1p, y1p, w1p, 
                       scale, abs.tol = 0)[[1]]
      est <- integrate(crmht, -10, 10, x1p, y1p, w1p, scale, 
                       abs.tol = 0)[[1]]/den
      if (var.est) {
        e2 <- integrate(crmht2, -10, 10, x1p, y1p, w1p, 
                        scale, abs.tol = 0)[[1]]/den
      }
    }
    else {
      stop(" unknown estimation method")
    }
    ptox <- prior^exp(est)
    if (var.est) {
      post.var <- e2 - est^2
      crit <- qnorm(0.5 + conf.level/2)
      lb <- est - crit * sqrt(post.var)
      ub <- est + crit * sqrt(post.var)
      ptoxL <- prior^exp(ub)
      ptoxU <- prior^exp(lb)
    }
  }
  else if (model == "logistic") {
    dosescaled <- log(prior/(1 - prior)) - intcpt
    if (!all(dosescaled < 0)) {
      stop("Intercept parameter in logit model is too small: scaled doses > 0!")
    }
    x1p <- dosescaled[level[include]]
    if (method == "mle") {
      if (sum(y1p) == 0 | sum(y1p) == length(y1p)) 
        stop(" mle does not exist!")
      est <- optimize(lcrmlgt, c(-10, 10), x1p, y1p, w1p, 
                      intcpt, tol = 1e-04, maximum = TRUE)$max
      if (var.est) {
        e2 <- integrate(crmht2lgt, -100, 100, x1p, y1p, 
                        w1p, 500, intcpt, abs.tol = 0)[[1]]/integrate(crmhlgt, 
                                                                      -10, 10, x1p, y1p, w1p, 500, intcpt, abs.tol = 0)[[1]]
      }
    }
    else if (method == "bayes") {
      den <- integrate(crmhlgt, -Inf, Inf, x1p, y1p, w1p, 
                       scale, intcpt, abs.tol = 0)[[1]]
      est <- integrate(crmhtlgt, -10, 10, x1p, y1p, w1p, 
                       scale, intcpt, abs.tol = 0)[[1]]/den
      if (var.est) {
        e2 <- integrate(crmht2lgt, -10, 10, x1p, y1p, 
                        w1p, scale, intcpt, abs.tol = 0)[[1]]/den
      }
    }
    else {
      stop(" unknown estimation method")
    }
    ptox <- (1 + exp(-intcpt - exp(est) * dosescaled))^{
      -1
    }
    if (var.est) {
      post.var <- e2 - est^2
      crit <- qnorm(0.5 + conf.level/2)
      lb <- est - crit * sqrt(post.var)
      ub <- est + crit * sqrt(post.var)
      ptoxL <- (1 + exp(-intcpt - exp(ub) * dosescaled))^{
        -1
      }
      ptoxU <- (1 + exp(-intcpt - exp(lb) * dosescaled))^{
        -1
      }
    }
  }
  else {
    stop(" model specified not available.")
  }
  if (all(ptox <= target)) {
    rec <- length(prior)
  }
  else if (all(ptox >= target)) {
    rec <- 1
  }
  else {
    rec <- order(abs(ptox - target))[1]
  }
  if (!var.est) {
    post.var <- ptoxU <- ptoxL <- NA
  }
  foo <- list(prior = prior, target = target, tox = tox, level = level, 
              dosename = dosename, subset = pid[include], estimate = est, 
              model = model, prior.var = scale^2, post.var = post.var, 
              method = method, mtd = rec, include = include, pid = pid, 
              model.detail = model.detail, intcpt = intcpt, ptox = ptox, 
              ptoxL = ptoxL, ptoxU = ptoxU, conf.level = conf.level, 
              patient.detail = patient.detail, tite = FALSE, dosescaled = dosescaled, 
              var.est = var.est)
  class(foo) <- "mtd"
  foo
}