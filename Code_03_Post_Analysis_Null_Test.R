library(tidyverse); library(stringr);library(attenPlot)
#Load/Organize Annual Data
load("03_Data_Annual.RData")
logit = function(p){log(p/(1-p))}
Y.Data = Y.Data %>% group_by(Station.ID) %>% mutate(med.log.sd = scale(Median.F, center = F), max.log.sd = scale(Max.F, center = F), min.log.sd = scale(Min.F, center = F), DOY2.logit = logit(DOY2/365), Year.Center = Year-1988)
#Load/Orgnize Montly Data
load("03_Data_Monthly.RData")
M.Data = M.Data %>% group_by(Station.ID) %>% mutate(med.log.sd = scale(median.M.flow, center = F), max.log.sd = scale(max.M.flow, center = F), min.log.sd = scale(min.M.flow, center = F), Year.Center = Year-1988)
#Scaling the data by site has no impact on the results, rather it only stands to reduce the likelyhood of computational issues during model bootstraping.

#Incorporate the Climate Data
setwd("~/sfuvault/Simon_Fraser_University/PhD_Research/Projects/Data/Original_Data/GIS_Data/BC_Climate_Data/ClimateTrends/zonal_stats")
vars = unlist(lapply(strsplit(dir(), "-"), function(x) x[1]))
AnnualVars = vars[which(nchar(vars) == 3)]
MonthlyVars = vars[which(nchar(vars) > 3 & nchar(vars)<7)]
#Annual
for(i in AnnualVars){
	labels = c("Station.ID","count","min","mean","max",paste(i,".sd",sep = ""),"P25","P75")
	cDat = read_csv(paste(i,"-stats.csv", sep = ""), col_names =  labels) %>% select(1,6)
	Y.Data = dplyr::left_join(Y.Data, cDat, by = "Station.ID")
}
Y.Data = Y.Data %>% ungroup(); rm(cDat, AnnualVars, i, logit, vars, labels)

#Monthly
vars = unique(str_extract(string = MonthlyVars, pattern = "([^0-9]){3,}"))
M.Data = lapply(vars,function(x){
	clim = MonthlyVars[grep(pattern = x, MonthlyVars)] %>% paste(.,"-stats.csv", sep = "") %>%
		lapply(., function(y){
			labels = c("Station.ID","count","min","mean","max",paste(x,".sd", sep = ""),"P25","P75")
			read_csv(y, col_names = labels) %>% select(1,6) %>% mutate(nMonth = as.numeric(str_extract(string = y, pattern = "([0-9]{2})")))
		}) %>% bind_rows()
		M.Data <<- left_join(M.Data, clim, by = c("Station.ID","nMonth"))
})[[length(vars)]] %>% ungroup()
rm(vars, MonthlyVars)

#remove = c("08MH006","08MH076")
#M.Data = filter(M.Data, Station.ID%in%remove == F)
#Y.Data = filter(Y.Data, Station.ID%in%remove == F)

setwd("~/sfuvault/Simon_Fraser_University/PhD_Research/Projects/River-Network-Flow-Trends")
# this function fits slopes to real data
fit_slopes <- function(flow.dat, response, pred.var, vars) {
	area_dat <- unique(flow.dat[,c("Station.ID", vars)])
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
fit_var <- function(slopes, ind, clim, var2 = F) {
	scale_factor <- var(slopes)
	scaled_slope <- slopes / scale_factor
	x <- sqrt(ind)
	x <- x - mean(x)
	v = sqrt(ind)/sd(sqrt(ind))
	w = clim/sd(clim)
	if(var2 == T){
		tryCatch({
			m <- gls(scaled_slope~x, weights = varComb(varExp(form = ~v), varExp(form = ~w)),
							 control = glsControl(maxIter = 1000L, msMaxIter = 1000L))
			varexpA <- m$model[[1]][[1]][[1]]
			varexpC <- m$model[[1]][[2]][[1]]
			sigma <- m$sigma * scale_factor
			intercept <- coef(m)[[1]] * scale_factor
			slope = m$coefficients[[2]] * scale_factor
			slopePval = summary(m)[[18]][8]
			retrn = data.frame(varexpA, varexpC, sigma, intercept, slope, slopePval)
		}, .error = function(e) {
			retrn = data.frame(varexpA = NA, varexpC = NA, sigma = NA, intercept = NA, slope = NA, slopePval = NA)
		})
	}
	if(var2 == F){
		tryCatch({
			m <- gls(scaled_slope~x, weights = varExp(form= ~sqrt(ind)/1e3),
							 control = glsControl(maxIter = 1000L, msMaxIter = 1000L))
			varexp <- m$model[[1]][[1]]
			sigma <- m$sigma * scale_factor
			intercept <- coef(m)[[1]] * scale_factor
			slope = m$coefficients[[2]] * scale_factor
			slopePval = summary(m)[[18]][8]
			retrn = data.frame(varexp, sigma, intercept, slope, slopePval)
		}, .error = function(e) {
			retrn = data.frame(varexp = NA, sigma = NA, intercept = NA, slope = NA, slopePval = NA)
		})
	}
	retrn
}

# wrapper function for analysis of individual predictor variables.
null_sim <- function(flow.dat, response, pred.var, var2, vars, iter, .parallel){
  yrs <- unique(flow.dat$Year.Center)
  real_slopes <- fit_slopes(flow.dat, response, pred.var, vars)
  real_slopes = real_slopes %>% select(8:ncol(.)) %>% apply(., 2, function(x) zero_one(x)) %>% 
  	apply(., 1, function(x) sum(x)) %>% mutate(real_slopes, std.clim = .)
  example_ts <- sim_slopes(slope.dat = real_slopes, yrs = yrs, return_ts = TRUE)
  sim_varexp <- plyr::ldply(seq_len(iter), function(i){
    simulated_slopes <- sim_slopes(slope.dat = real_slopes, yrs = yrs)
    cols = names(simulated_slopes)
    out <- fit_var(simulated_slopes$slope_sim, 
    							 ind = simulated_slopes[,grep(pred.var, cols)], 
    							 clim = simulated_slopes[,grep("std.clim", cols)],
    							 var2 = var2)
    out$.n <- i
    out
  },.progress = "text", .parallel = .parallel)
  cols = names(real_slopes)
  real_varexp <- fit_var(real_slopes$slope, 
  											 ind = real_slopes[,grep(pred.var, cols)],
  											 clim = real_slopes[,grep("std.clim", cols)],
  											 var2 = var2)
  list(real_varexp = real_varexp, real_slopes = real_slopes,
    sim_varexp = sim_varexp, example_ts = example_ts)
}

# wrapper function to run analyses across all predictor variables.
iter_null_sim <- function(flow.dat, response, pred.vars, var2, vars, iter, .parallel = FALSE){
	sapply(pred.vars, function(x){
		null_sim(flow.dat, response, x, var2, vars, iter, .parallel)
	}, simplify = F, USE.NAMES = T)
}

doParallel::registerDoParallel(cores = parallel::detectCores()-1)

set.seed(123)
vars = c("Area","emt.sd","ext.sd","map.sd","mat.sd","pas.sd")
out_doy2 <- iter_null_sim(Y.Data, "DOY2.logit", "Area", F, vars, 1000L, T)
out_min <- iter_null_sim(Y.Data, "log(min.log.sd)", "Area", F, vars, 1000L, T)
out_max <- iter_null_sim(Y.Data, "log(max.log.sd)", "Area", F, vars, 1000L, T)
out_med <- iter_null_sim(Y.Data, "log(med.log.sd)", "Area", F, vars, 1000L, T)

stopifnot(identical(sum(is.na(out_doy2$Area$sim_varexp$varexp)), 0L))
stopifnot(identical(sum(is.na(out_min$Area$sim_varexp$varexp)), 0L))
stopifnot(identical(sum(is.na(out_max$Area$sim_varexp$varexp)), 0L))
stopifnot(identical(sum(is.na(out_med$Area$sim_varexp$varexp)), 0L))

save(out_doy2, file = "out_doy2.RData")
save(out_min, file = "out_min.RData")
save(out_max, file = "out_max.RData")
save(out_med, file = "out_med.RData")

vars = c("Area","pas.sd","ppt.sd","tave.sd","tmax.sd","tmin.sd")
out_min_month <- plyr::dlply(M.Data, "nMonth", function(x)
  iter_null_sim(x, "log(min.log.sd)", "Area", F, vars, iter = 1000L),
  .parallel = TRUE, .paropts = list(.packages = "nlme"))
save(out_min_month, file = "min_month.RData")

out_max_month <- plyr::dlply(M.Data, "nMonth", function(x)
  iter_null_sim(x, "log(max.log.sd)", "Area", F, vars, iter = 1000L),
  .parallel = TRUE, .paropts = list(.packages = "nlme"))
save(out_max_month, file = "max_month.RData")

out_med_month <- plyr::dlply(M.Data, "nMonth", function(x)
  iter_null_sim(x, "log(med.log.sd)", "Area", F, vars, iter = 1000L),
  .parallel = TRUE, .paropts = list(.packages = "nlme"))
save(out_med_month, file = "med_month.RData")


#look at the distribution of exponent values for Area, Climate or both.
full = list()
full[["doy2"]] = out_doy2; full[["max"]] = out_max; full[["min"]] = out_min; full[["med"]] = out_med
full.t = plyr::ldply(full, function(i){
	#sim = i$Area$sim_varexp %>% select(varexpA, varexpC) %>% gather(.,key = "variable", value = "varexpS")
	#real = i$Area$real_varexp %>% select(varexpA, varexpC) %>% gather(.,key = "variable", value = "varexpR")
	sim = i$Area$sim_varexp %>% select(varexp) %>% gather(.,key = "variable", value = "varexpS")
	real = i$Area$real_varexp %>% select(varexp) %>% gather(.,key = "variable", value = "varexpR")
	full = full_join(sim,real,by = "variable")
	full
})

ggplot(full.t, aes(varexpS, color = variable)) + geom_density() + geom_vline(aes(xintercept = varexpR, color = variable)) +
	theme_classic() + facet_wrap(~.id)
####


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