# test gls model and arima simulation
# prove that they are interchangeable
# answer: yes.
#
# gls() with arima residuals is interchangeable with fitted arima model
# with same order and drift

set.seed(1)
library(forecast) # for Arima()
library(dplyr)

out <- plyr::rdply(200, function() {

  y <- as.numeric(arima.sim(n = 60L, model = list(model = c(1, 0, 0),
        ar = 0.3), sd = 0.2))

  m1_arima <- Arima(y, order = c(1L, 0L, 0L), include.drift = TRUE)
  phi_arima <- coef(m1_arima)[[1]]
  intercept_arima <- coef(m1_arima)[[2]]
  drift_arima <- coef(m1_arima)[[3]]
  sigma_arima <- sqrt(m1_arima$sigma2)

  m1 <- gls(y~seq_along(y), correlation = corAR1())
  phi <- coef(m1$model[[1]], unconstrained = FALSE)[[1]]
  sigma <- m1$sigma
  intercept <- coef(m1)[[1]]
  slope <- coef(m1)[[2]]

  ysim <- arima.sim(n = length(y), model = list(order = c(1, 0, 0),
      ar = phi), sd = sigma) %>% as.numeric()

  m2 <- gls(ysim~seq_along(ysim), correlation = corAR1())
  phi_sim <- coef(m2$model[[1]], unconstrained = FALSE)[[1]]
  sigma_sim <- m2$sigma
  intercept_sim <- coef(m2)[[1]]

  data.frame(phi, sigma, phi_sim, sigma_sim, phi_arima, intercept_arima,
    sigma_arima, drift_arima, slope, intercept)
})

out <- out %>% mutate(phi_re = (phi_sim - phi) / phi,
  sigma_re = (sigma_sim - sigma) / sigma)

par(mfrow = c(3, 2))
hist(out$phi)
abline(v = 0.3, col = "red")
hist(out$sigma)
abline(v = 0.2, col = "red")
hist(out$phi_sim)
abline(v = 0.3, col = "red")
hist(out$sigma_sim)
abline(v = 0.2, col = "red")
hist(out$phi_re)
abline(v = 0, col = "red")
hist(out$sigma_re)
abline(v = 0, col = "red")

par(mfrow = c(2, 2))
plot(out$drift_arima, out$slope)
abline(a = 0, b = 1, col = "red")
plot(out$intercept_arima, out$intercept)
abline(a = 0, b = 1, col = "red")
plot(out$phi_arima, out$phi)
abline(a = 0, b = 1, col = "red")
plot(out$sigma_arima, out$sigma_arima)
abline(a = 0, b = 1, col = "red")
