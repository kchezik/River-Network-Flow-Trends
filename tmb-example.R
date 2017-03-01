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



# Replicate with Fraser Data.
library(tidyverse)
load("out_doy2.RData")

#Function for scaling values between 0 and 1.
zero_one = function(x) ((x-min(x)))/(diff(range(x)))
#Function for standardizing values.
std = function(x) (x/sd(x))

u = out_doy2$Area$real_slopes$slope
s1 = out_doy2$Area$real_slopes %>% mutate(STDsqrtArea = std(sqrt(Area))) %>% .$STDsqrtArea
s2 = out_doy2$Area$real_slopes %>% select(8:ncol(.)) %>% apply(.,2,function(x) zero_one(x)) %>% 
	apply(.,1,function(x) sum(x)) %>% std(.)
mm = model.matrix(~ s1 + s2)

library(TMB)
compile("varianceFunction.cpp")
dyn.load(dynlib("varianceFunction"))

obj <- MakeADFun(data = list(x_ij = mm, y_i = u),
								 parameters = list(b_j = c(0, 0, 0), sigma_j = c(0, 0, 0)),
								 DLL = "varianceFunction")

opt <- nlminb(start=obj$env$last.par.best, objective=obj$fn, gradient=obj$gr)
opt$par

#			b_j           b_j           b_j       sigma_j       sigma_j       sigma_j 
#-2.197504e-03  2.525132e-04  1.253969e-04 -1.308270e+01 -1.327247e+00  5.401138e-01

rep <- sdreport(obj)
rep

# sdreport(.) result
# 						Estimate   Std. Error
# b_j     -2.197504e-03 0.0004539609
# b_j      2.525132e-04 0.0001232716
# b_j      1.253969e-04 0.0002550198
# sigma_j -1.308270e+01 0.4312540804
# sigma_j -1.327247e+00 0.1735828737
# sigma_j  5.401138e-01 0.2108288820
# Maximum gradient component: 0.02134764