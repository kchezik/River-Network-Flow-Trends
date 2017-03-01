library(TMB)
compile("varianceFunction.cpp")
dyn.load(dynlib("varianceFunction"))

set.seed(1234)
b0 <- 0.5
b1 <- 0.3
b2 <- 0.1
sigma0 <- 0.1
sigma1 <- -0.2
sigma2 <- 0.3
N <- 2000
s1 <- sample(seq(1, 10), N, replace = TRUE)
s2 <- sample(seq(1, 10), N, replace = TRUE)

# This is the way that nlme::varExp() has the function set up:
# var * exp(2 * t * v)
# This one is set up a little bit differently but has the same general shape:
u <- rnorm(N, b0 + b1 * s1 + b2 * s2, sqrt(exp(sigma0 + sigma1 * s1 + sigma2 * s2)))
# matching it exactly:
# u <- rnorm(N, b0 + b1 * s, sqrt(sigma0^2 * exp(2 * sigma1 * s)))
# Not using this one right now because I'd have to tweak the model matrix below 
# and I'm out of time. 
plot(s1, u)
plot(s2, u)

mm <- model.matrix(~ s1 + s2)

obj <- MakeADFun(data = list(x_ij = mm, y_i = u),
  parameters = list(b_j = c(0, 0, 0), sigma_j = c(0, 0, 0)),
  DLL = "varianceFunction")

opt <- nlminb(start=obj$env$last.par.best, objective=obj$fn, gradient=obj$gr)
opt$par
# If you need standard errors and therefore confidence intervals... exp(sigma1 + 1.96 * SE)... :
rep <- sdreport(obj)
rep
