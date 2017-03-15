#supplementary plots
library(dplyr)
load("out_med.RData")
load("03_Data_Annual.RData")
logit = function(p){log(p/(1-p))}
Y.Data = plyr::ddply(Y.Data,"Station.ID",plyr::mutate, med.log.sd = scale(Median.F, center = F), max.log.sd = scale(Max.F, center = F), min.log.sd = scale(Min.F, center = F), DOY2.logit = logit(DOY2/365), Year.Center = Year-1988)


dat_sim = out_med$example_ts; names(dat_sim)[2] = "Year.Center"
dat = dplyr::left_join(dat_sim, Y.Data, by = c("Station.ID", "Year.Center"))
Adjuster = Y.Data %>%
	group_by(Station.ID) %>%
	summarise(adjust = sqrt(mean(Median.F^2)))

dat = dplyr::left_join(Adjuster, dat, by = "Station.ID")
dat = dat %>% mutate(med_sim_unscale = exp(y)*adjust)

ggplot(dat, aes(Year, med_sim_unscale)) +
	geom_line(colour = "#F2AD00") +
	geom_line(aes(Year.Center + 1988, Median.F), colour = "#67A9CF", alpha = 0.5) +
	facet_wrap(~Station.ID, scales = "free_y") +
	theme_classic(base_size = 9) +
	annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
	annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
	theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")) +
	labs(x = "Year", y = expression("Simulated & Observed Annual Median-Flow (m"^3%.%"sec"^-1*")"%.%"year"^-1))

ggsave("FigS3_site_simulation.pdf", width = 11, height = 8.5)

#clean up NA's in simulated data where no observed data existed.
dat$Area = apply(dat, 1, function(x){
	#browser()
	if(is.na(x["Area"])){
		as.numeric(na.omit(unique(dat[which(dat$Station.ID==x["Station.ID"]),"Area"])))
	} else as.numeric(x["Area"])
})

# this function fits slopes to real data
fit_slopes <- function(flow.dat, response) {
	area_dat <- na.omit(unique(flow.dat[,c("Station.ID", "Area")]))
	equation <- as.formula(paste(response,"~Year.Center"))
	models <- plyr::ddply(flow.dat,"Station.ID", function(x){
		#browser()
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
sim_out = fit_slopes(dat, "y")
m = gls(slope~sqrt(Area), data = sim_out, weights = varExp(form= ~sqrt(Area)/1e3), 
							control = glsControl(maxIter = 1000L, msMaxIter = 1000L))
varexp <- m$model[[1]][[1]]; sigma <- m$sigma; intercept <- coef(m)[[1]]; slope = m$coefficients[[2]]; sim_varexp = data.frame(varexp, sigma, intercept, slope)

var_upper = out_med$real_varexp$intercept + out_med$real_varexp$slope*(sqrt(sim_out$Area) - mean(sqrt(sim_out$Area))) + 1.96 * sqrt(out_med$real_varexp$sigma^2 * exp(2*(sqrt(sim_out$Area)/1e3)*sim_varexp$varexp))
var_lower = out_med$real_varexp$intercept + out_med$real_varexp$slope*(sqrt(sim_out$Area) - mean(sqrt(sim_out$Area))) - 1.96 * sqrt(out_med$real_varexp$sigma^2 * exp(2*(sqrt(sim_out$Area)/1e3)*sim_varexp$varexp))
sim_out = sim_out %>% mutate(label = rep('sim',nrow(sim_out)), var_upper, var_lower)

var_upper = out_med$real_varexp$intercept + out_med$real_varexp$slope*(sqrt(sim_out$Area) - mean(sqrt(sim_out$Area))) + 1.96 * sqrt(out_med$real_varexp$sigma^2 * exp(2*(sqrt(sim_out$Area)/1e3)*out_med$real_varexp$varexp))
var_lower = out_med$real_varexp$intercept + out_med$real_varexp$slope*(sqrt(sim_out$Area) - mean(sqrt(sim_out$Area))) - 1.96 * sqrt(out_med$real_varexp$sigma^2 * exp(2*(sqrt(sim_out$Area)/1e3)*out_med$real_varexp$varexp))
real_out = out_med$real_slopes %>% mutate(label = rep('real',nrow(out_med$real_slopes)), var_upper, var_lower)

final = bind_rows(sim_out, real_out)

labels.y = c(-0.01,0,0.01,0.02)
label.y = round(exp(labels.y*10)*100-100,0)

ggplot(final, aes(sqrt(Area), slope, colour = label)) +
	geom_point() +
	geom_smooth(aes(sqrt(Area), var_upper)) +
	geom_smooth(aes(sqrt(Area), var_lower)) +
	scale_color_manual(values=c("#67A9CF", "#F2AD00"), guide = F) +
	scale_x_continuous(breaks = c(0, 100, 200, 300, 400), labels = as.character(c(0, 100^2, 200^2, 300^2, 400^2))) +
	scale_y_continuous(breaks = labels.y, labels = as.character(label.y)) +
	theme_classic(base_size = 9) +
	geom_errorbar(aes(ymax = slope + se, ymin=slope - se, colour = label)) +
	annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
	annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
	theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")) +
	labs(y = expression("Median-Flow | %Change"%.%"Decade"^-1), x = expression("Area (km"^2*")"))

ggsave("FigS4_basin_simulation.pdf", width = 11, height = 8.5)


#S7 Plot the Climate Portfolio over each month 
load("out_max_month.RData")
dat = plyr::ldply(out_max_month, function(x){
	clim = x$Area$real_slopes$std.clim
})
names(dat)[2:56] = out_max_month$`1`$Area$real_slopes$Station.ID

df = gather(dat, key = "Station.ID",value = "value", 2:56)
df = df %>% group_by(Station.ID) %>% mutate(value.adj = zero_one(value))
df1 = df %>% mutate(year = 0)
df2 = df %>% mutate(year = 12)
df3 = df %>% mutate(year = 24)
df = bind_rows(df1,df2,df3)
df = df %>% mutate(time = nMonth + year)

ggplot(df, aes(time, value.adj, color = Station.ID)) + geom_smooth(se = F, span = 1, n = 36) + labs(x = "Month", y = "Cliamte Portfolio") + scale_x_continuous(breaks = c(13:24), limits = c(13,24), labels = as.character(1:12)) + theme_minimal() + theme(legend.position = "none")

ggsave("FigS7_Seasonal_Climate_Portfolio.pdf", width = 11, height = 8.5)