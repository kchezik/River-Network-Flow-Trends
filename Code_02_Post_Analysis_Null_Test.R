load("03_Data_Annual.RData")
load("03_Data_Monthly.RData")
logit = function(p){log(p/(1-p))}
Y.Data = plyr::ddply(Y.Data,"Station.ID",plyr::mutate, med.log.sd = scale(Median.F, center = F), max.log.sd = scale(Max.F, center = F), min.log.sd = scale(Min.F, center = F), DOY2.logit = logit(DOY2/365), Year.Center = Year-1988)
M.Data = plyr::ddply(M.Data, "Station.ID", plyr::mutate, med.log.sd = scale(median.M.flow, center = F),max.log.sd = scale(max.M.flow, center = F), min.log.sd = scale(min.M.flow, center = F), Year.Center = Year-1988)
#Scaling the data by site has no impact on the results, rather it only stands to reduce the likelyhood of computational issues during model bootstraping.

# this function fits slopes to real data
fit_slopes <- function(flow.dat, response) {
	area_dat <- unique(flow.dat[,c("Station.ID", "Area")])
	equation <- as.formula(paste(response,"~Year.Center"))
	models <- plyr::ddply(flow.dat,"Station.ID", function(x){
		library(nlme)
		mod <- gls(equation, correlation = corAR1(), data = x)
		slope <- coef(mod)[[2]]
		intercept <- coef(mod)[[1]]
		se <- summary(mod)$tTable[2,2]
		sigma <- mod$sigma
		phi <- coef(mod$model[[1]], unconstrained = F)[[1]]
		data.frame(intercept,slope,se,sigma,phi)
	})
	models <- plyr::join(models,area_dat, by = "Station.ID")
	models
}

# this function takes a data frame from fit_slopes() and generates a simulated
# time series (WITHOUT SLOPE) plus a gls() fit to that simulated data
sim_slopes <- function(slope.dat, yrs, return_ts = FALSE){
	out <- plyr::adply(slope.dat, 1, function(x) {
		y <- as.numeric(arima.sim(n = length(yrs),
															model = list(order = c(1, 0, 0), ar = x$phi),
															mean = 0, sd = x$sigma)) + x$intercept
		mod_sim <- gls(y~yrs, correlation = corAR1(), control = list(maxIter = 1000, msMaxIter = 1000))
		slope_sim <- coef(mod_sim)[[2]]
		se_sim <- summary(mod_sim)$tTable[2,2]
		if (!return_ts) {
			data.frame(slope_sim, se_sim)
		} else {
			data.frame(y, yrs)
		}
	})
	if (return_ts) out <- out[,c("Station.ID", "yrs", "y")]
	out
}

# this function fits a gls model with a variance structure for the residual error
# slopes and area are both vectors
fit_var <- function(slopes, area) {
	scale_factor <- var(slopes)
	scaled_slope <- slopes / scale_factor
	x <- sqrt(area)
	x <- x - mean(x)
	tryCatch({
		m <- gls(scaled_slope~x, weights = varExp(form= ~sqrt(area)/1e3), 
			control = glsControl(maxIter = 1000L, msMaxIter = 1000L))
		varexp <- m$model[[1]][[1]]
		sigma <- m$sigma * scale_factor
		intercept <- coef(m)[[1]] * scale_factor
		slope = m$coefficients[[2]] * scale_factor
		slopePval = summary(m)[[18]][8]
		data.frame(varexp, sigma, intercept, slope, slopePval)
	}, .error = function(e) {
		data.frame(varexp = NA, sigma = NA, intercept = NA, slope = NA, slopePval = NA)
	})
}

# wrapper function
null_sim <- function(flow.dat, response, iter, .parallel = FALSE){
  yrs <- unique(flow.dat$Year.Center)
  real_slopes <- fit_slopes(flow.dat, response)
  example_ts <- sim_slopes(slope.dat = real_slopes, yrs = yrs, return_ts = TRUE)
  sim_varexp <- plyr::ldply(seq_len(iter), function(i){
    simulated_slopes <- sim_slopes(slope.dat = real_slopes, yrs = yrs)
    out <- fit_var(simulated_slopes$slope_sim, area = simulated_slopes$Area)
    out$.n <- i
    out
  },.progress = "text", .parallel = .parallel)
  real_varexp <- fit_var(real_slopes$slope, area = real_slopes$Area)
  list(real_varexp = real_varexp, real_slopes = real_slopes,
    sim_varexp = sim_varexp, example_ts = example_ts)
}

doParallel::registerDoParallel(cores = parallel::detectCores()-1)

set.seed(123)
out_doy2 <- null_sim(Y.Data, "DOY2.logit", iter = 1000L, .parallel = TRUE)
out_min <- null_sim(Y.Data, "log(min.log.sd)", iter = 1000L, .parallel = TRUE)
out_max <- null_sim(Y.Data, "log(max.log.sd)", iter = 1000L, .parallel = TRUE)
out_med <- null_sim(Y.Data, "log(med.log.sd)", iter = 1000L, .parallel = TRUE)

stopifnot(identical(sum(is.na(out_doy2$sim_varexp$varexp)), 0L))
stopifnot(identical(sum(is.na(out_min$sim_varexp$varexp)), 0L))
stopifnot(identical(sum(is.na(out_max$sim_varexp$varexp)), 0L))
stopifnot(identical(sum(is.na(out_med$sim_varexp$varexp)), 0L))

save(out_doy2, file = "out_doy2.RData")
save(out_min, file = "out_min.RData")
save(out_max, file = "out_max.RData")
save(out_med, file = "out_med.RData")

out_min_month <- plyr::dlply(M.Data, "nMonth", function(x)
  null_sim(x, "log(min.log.sd)", iter = 1000L),
  .parallel = TRUE, .paropts = list(.packages = "nlme"))
save(out_min_month, file = "min_month.RData")

out_max_month <- plyr::dlply(M.Data, "nMonth", function(x)
  null_sim(x, "log(max.log.sd)", iter = 1000L),
  .parallel = TRUE, .paropts = list(.packages = "nlme"))
save(out_max_month, file = "max_month.RData")

out_med_month <- plyr::dlply(M.Data, "nMonth", function(x)
  null_sim(x, "log(med.log.sd)", iter = 1000L),
  .parallel = TRUE, .paropts = list(.packages = "nlme"))
save(out_med_month, file = "med_month.RData")


## look at some sample time series:
p1 <- ggplot(out_med$example_ts, aes(yrs, exp(y))) + geom_line() + facet_wrap(~Station.ID, scales = "free_y")
ggsave("sim-doy2.pdf", width = 13, height = 10)

p2 <- ggplot(out_med$sim_varexp)
 
p <- ggplot(Y.Data, aes(Year.Center, Min.F)) + geom_line() + facet_wrap(~Station.ID, scales = "free_y")
ggsave("real-doy2.pdf", width = 13, height = 10)
 
p <- ggplot(out_min_month[[6]]$example_ts, aes(yrs, exp(y))) + geom_line() + facet_wrap(~Station.ID, scales = "free_y")
ggsave("sim-min-month.pdf", width = 13, height = 10)
 
p <- ggplot(subset(M.Data, nMonth == 6), aes(Year.Center, min.M.flow)) + geom_line() + facet_wrap(~Station.ID, scales = "free_y")
ggsave("real-min-month.pdf", width = 13, height = 10)
 
# check order of arima:
aic_arima <- function(flow.dat, response) {
  library(nlme)
  area_dat <- unique(flow.dat[,c("Station.ID", "Area")])
  equation <- as.formula(paste(response,"~Year.Center"))
  arma <- expand.grid(ar = 0:2, ma = 0:1)
  plyr::ddply(flow.dat,c("Station.ID"), function(x){
    plyr::adply(arma, 1, function(y) {
      ar_ <- y$ar
      ma_ <- y$ma
      if (ar_ > 0 & ma_ > 0) {
        m <- gls(equation, correlation = corARMA(p = ar_, q = ma_), data = x)
      }
      if (ar_ > 0 & ma_ == 0) {
        m <- gls(equation, correlation = corARMA(p = ar_), data = x)
      }
      if (ar_ == 0 & ma_ > 0) {
        m <- gls(equation, correlation = corARMA(q = ma_), data = x)
      }
      if (ar_ == 0 & ma_ == 0) {
        m <- gls(equation, data = x)
      }
      aic_out <- AICcmodavg::AICc(m)
    data.frame(aic_out)
    })
  })
}
aic_out <- aic_arima(Y.Data, "DOY2.logit")
aic_out %>% group_by(Station.ID) %>%
  mutate(delta_aic = aic_out - min(aic_out)) %>%
  ggplot(aes(paste(ar, ma), delta_aic, group = Station.ID)) + geom_line(alpha = 0.4)

aic_out <- aic_arima(subset(M.Data, nMonth == 6), "log(med.log.sd)")
aic_out %>% group_by(Station.ID) %>%
  mutate(delta_aic = aic_out - min(aic_out)) %>%
  ggplot(aes(paste(ar, ma), delta_aic, group = Station.ID)) + geom_line(alpha = 0.4)
