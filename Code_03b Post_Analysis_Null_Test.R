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

#Incorporate Land cover data.
setwd("~/sfuvault/Simon_Fraser_University/PhD_Research/Projects/Data/Original_Data/GIS_Data/BC_Climate_Data/ClimateTrends/forest_stats")
#Gather Together all the sites timber harvest results.
df = dir() %>% 
	lapply(.,function(x){
		year = as.numeric(substr(x,1,4))
		read_csv(x,col_names = c("Station.ID","count")) %>% 
			mutate(Year = year)
	}) %>% 
	bind_rows()
#Convert the 100m X 100m raster count data to area of timber harvest in km^2.
df = df %>% mutate(harvest = count*(0.1*0.1))
#Create a rolling average column to account for recent cuts.
roll = plyr::ddply(df, "Station.ID", function(x){
	c = 1; FiveHarvest = NULL
	for(i in 1970:2007){
		FiveHarvest[c] = x %>% filter(Year <= i, Year >= i-5) %>% .$harvest %>% sum(.)
		c = c + 1
	}
	data.frame(FiveHarvest, Year = c(1970:2007))
})
df = df %>% filter(Year >= 1970) %>% left_join(., roll, by = c("Station.ID","Year"))

#
M.Data = df %>% select(-count) %>% left_join(M.Data, ., by = c("Station.ID","Year"))
Y.Data = df %>% select(-count) %>% left_join(Y.Data, ., by = c("Station.ID","Year"))
M.Data = M.Data %>% mutate(pHarvest = harvest/Area, p5Harvest = FiveHarvest/Area)
Y.Data = Y.Data %>% mutate(pHarvest = harvest/Area, p5Harvest = FiveHarvest/Area)
M.Data = M.Data %>% group_by(Station.ID) %>% mutate(p5H_Center = scale(p5Harvest, scale=F))
Y.Data = Y.Data %>% group_by(Station.ID) %>% mutate(p5H_Center = scale(p5Harvest, scale=F))
rm(df)

setwd("~/sfuvault/Simon_Fraser_University/PhD_Research/Projects/River-Network-Flow-Trends")
save(Y.Data, file = "05_MonthDat_ClimForest.RData")
save(M.Data, file = "05_AnnualDat_ClimForest.RData")

load("05_MonthDat_ClimForest.RData")
load("05_AnnualDat_ClimForest.RData")

# this function fits slopes to real data
fit_slopes <- function(flow.dat, response, pred.var, vars) {
	area_dat <- unique(flow.dat[,c("Station.ID", vars[-c(7,8)])]) #Edit
	equation1 <- as.formula(paste(response,"~Year.Center")) #Edit
	#equation2 <- as.formula(paste(response,"~Year.Center+p5H_Center")) #Edit
	models <- plyr::ddply(flow.dat,"Station.ID", function(x){
		library(nlme)
		#if(sum(x$p5Harvest)==0) mod <- gls(equation1, correlation = corAR1(), data = x) #Edit
		#if(sum(x$p5Harvest)!=0) mod <- gls(equation2, correlation = corAR1(), data = x) #Edit
		mod <- gls(equation1, correlation = corAR1(), data = x) #Edit
		slope <- coef(mod)[[2]]
		se <- summary(mod)$tTable[2,2]
		intercept <- coef(mod)[[1]]
		sigma <- mod$sigma
		phi <- coef(mod$model[[1]], unconstrained = F)[[1]]
		data.frame(intercept,slope,se,sigma,phi)
	})
	models <- plyr::join(models, area_dat, by = "Station.ID")
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
fit_var <- function(slopes, ind, clim) {
	scale_factor <- var(slopes)
	scaled_slope <- slopes / scale_factor
	x <- sqrt(ind)
	x <- x - mean(x) #Centered so the y-intercept is the basin wide mean response.
	v = sqrt(ind)/sd(sqrt(ind))
	#Climate index response variable, weighted by area and centered.
	w = clim - mean(clim)
		tryCatch({
			m <- gls(scaled_slope~x, weights = varExp(form= ~sqrt(ind)/1e3),
			#m <- gls(scaled_slope~w, weights = varExp(form= ~clim/10),
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
	retrn
}

# wrapper function for analysis of individual predictor variables.
null_sim <- function(flow.dat, response, pred.var, vars, iter, .parallel){
  yrs <- unique(flow.dat$Year.Center)
  real_slopes <- fit_slopes(flow.dat, response, pred.var, vars)
  real_slopes = real_slopes %>% select(8:ncol(.)) %>% apply(., 2, function(x) zero_one(x)) %>% 
  	apply(., 1, function(x) sum(x)) %>% mutate(real_slopes, std.clim = .)
  #Scale climate between one and zero and exponeniate to make things greater than zero.
  real_slopes = mutate(real_slopes, index = std.clim*exp(zero_one(Area))) 
  example_ts <- sim_slopes(slope.dat = real_slopes, yrs = yrs, return_ts = TRUE)
  sim_varexp <- plyr::ldply(seq_len(iter), function(i){
    simulated_slopes <- sim_slopes(slope.dat = real_slopes, yrs = yrs)
    cols = names(simulated_slopes)
    out <- fit_var(simulated_slopes$slope_sim, 
    							 ind = simulated_slopes[,grep(pred.var, cols)], 
    							 clim = simulated_slopes[,grep("index", cols)])
    out$.n <- i
    out
  },.progress = "text", .parallel = .parallel)
  cols = names(real_slopes)
  real_varexp <- fit_var(real_slopes$slope, 
  											 ind = real_slopes[,grep(pred.var, cols)],
  											 clim = real_slopes[,grep("index", cols)])
  list(real_varexp = real_varexp, real_slopes = real_slopes,
    sim_varexp = sim_varexp, example_ts = example_ts)
}

# wrapper function to run analyses across all predictor variables.
iter_null_sim <- function(flow.dat, response, pred.vars, vars, iter, .parallel = FALSE){
	sapply(pred.vars, function(x){
		null_sim(flow.dat, response, x, vars, iter, .parallel)
	}, simplify = F, USE.NAMES = T)
}

doParallel::registerDoParallel(cores = parallel::detectCores()-1)

set.seed(123)
vars = c("Area","emt.sd","ext.sd","map.sd","mat.sd","pas.sd","p5Harvest", "p5H_Center")
out_doy2 <- iter_null_sim(Y.Data, "DOY2.logit", "Area", vars, 10L, T)
out_min <- iter_null_sim(Y.Data, "log(min.log.sd)", "Area", vars, 1000L, T)
out_max <- iter_null_sim(Y.Data, "log(max.log.sd)", "Area", vars, 1000L, T)
out_med <- iter_null_sim(Y.Data, "log(med.log.sd)", "Area", vars, 1000L, T)

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
	sim = i$Area$sim_varexp %>% select(varexpA, varexpC) %>% gather(.,key = "variable", value = "varexpS")
	real = i$Area$real_varexp %>% select(varexpA, varexpC) %>% gather(.,key = "variable", value = "varexpR")
	#sim = i$Area$sim_varexp %>% select(varexp) %>% gather(.,key = "variable", value = "varexpS")
	#real = i$Area$real_varexp %>% select(varexp) %>% gather(.,key = "variable", value = "varexpR")
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