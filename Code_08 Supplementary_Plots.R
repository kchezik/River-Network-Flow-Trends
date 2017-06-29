#supplementary plots
library(tidyverse);library(attenPlot);library(RColorBrewer);library(lme4);library(zoo);library(lubridate)
load("05_MonthDat_ClimForest.RData")
load("05_AnnualDat_ClimForest.RData")

#S1
ggplot(Y.Data, aes(Year, Median.F, color = p5Harvest*100)) + geom_point() + 
	facet_wrap(~Station.ID, scales = "free") + geom_smooth(method = "lm") +
	scale_color_gradient2(name = expression(frac("%Harvest","5-Year")), midpoint=5, low="#67A9CF", mid="#F2F2F2",
												high="#FF0000", space ="Lab") +
	scale_x_continuous(name="Year", limits=c(1970, 2007), breaks = c(1970,1988,2007)) +
	ylab(label = expression("Median Annual Flow (m"^3%.%"sec"^-1~")")) +
	theme_classic(base_size = 9) +
	theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
folder = "~/sfuvault/Simon_Fraser_University/PhD_Research/Projects/River-Network-Flow-Trends/drafts/AGU_Journal/submission\ 2/"
ggsave(paste(folder,"FigS1_SiteLogModels.pdf", sep = ""), width = 11, height = 7.5)

#S2
ggplot(M.Data, aes(p5Harvest*100, log(med.log.sd))) + geom_point(alpha = 0.5) + 
	facet_wrap(~Month, scales = "free") + 
	geom_smooth(aes(group = Station.ID, color = Station.ID), method ="lm", se = F) +
	ylab(label = expression("log"["e"]~"Scaled Median Flow (m"^3%.%"sec"^-1*")"%.%"year"^-1)) +
	xlab(expression("%Harvest"%.%"5-Year"^-1)) +
	theme_classic(base_size = 9) +
	theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), legend.position = "none")
ggsave(paste(folder,"FigS2_SiteMonthLogModels.pdf", sep = ""), width = 11, height = 7.5)

#S3
MmodGH = glmer(formula = med.log.sd~pHarvest+(1|Station.ID), family = Gamma(link = "log"), data = Y.Data)
temp = bind_cols(Y.Data, data.frame(fixed = predict(MmodGH, Y.Data)))
ggplot(temp, aes(pHarvest*100, log(as.numeric(med.log.sd)), color = Station.ID)) + geom_point(alpha = 0.5) + 
	geom_line(aes(pHarvest*100, fixed), color = "black") + #geom_line(aes(pHarvest, random, color = Station.ID)) +
	ylab(label = expression("log"["e"]~"Scaled Median Flow (m"^3%.%"sec"^-1*")"%.%"year"^-1)) +
	xlab(expression("%Harvest"%.%"5-Year"^-1)) +
	theme_classic(base_size = 9) +
	theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), legend.position = "none")
ggsave(paste(folder,"FigS3_GlobalLogModel.pdf", sep = ""), width = 7, height = 4)

#S8
ggplot(Y.Data, aes(Year, p5Harvest*100, color = Area)) + geom_jitter(alpha = 0.5) +
	geom_smooth(aes(Year, p5Harvest*100, group = Station.ID), se = F) +
	scale_color_continuous(low = "#DEEBF7", high = "#2171B5", trans = "log", 
												 name = expression("Area (km"^2~")"), 
												 breaks = c(400, 8000, 150000), labels = c("400", "8000", "150000")) +
	ylab(expression("%Harvest"%.%"5-Year"^-1)) +
	theme_classic(base_size = 9) +
	theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
ggsave(paste(folder,"FigS8_HarvestYear.pdf", sep = ""), width = 10, height = 5.5)

#S4
load("out_med.RData")
load("03_Data_Annual.RData")
logit = function(p){log(p/(1-p))}
Y.Data = plyr::ddply(Y.Data,"Station.ID",plyr::mutate, med.log.sd = scale(Median.F, center = F), max.log.sd = scale(Max.F, center = F), min.log.sd = scale(Min.F, center = F), DOY2.logit = logit(DOY2/365), Year.Center = Year-1988)

dat_sim = out_med$Area$example_ts; names(dat_sim)[2] = "Year.Center"
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

ggsave(paste(folder,"FigS4_SiteSim.pdf", sep = ""), width = 11, height = 7.5)

#S5
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

rInt = out_med$Area$real_varexp$intercept
rSlope = out_med$Area$real_varexp$slope
rSigma = out_med$Area$real_varexp$sigma
rVarexp = out_med$Area$real_varexp$varexp

var_upper = rInt + rSlope*(sqrt(sim_out$Area) - mean(sqrt(sim_out$Area))) + 1.96 * sqrt(rSigma^2 * exp(2*(sqrt(sim_out$Area)/1e3)*sim_varexp$varexp))
var_lower = rInt + rSlope*(sqrt(sim_out$Area) - mean(sqrt(sim_out$Area))) - 1.96 * sqrt(rSigma^2 * exp(2*(sqrt(sim_out$Area)/1e3)*sim_varexp$varexp))
sim_out = sim_out %>% mutate(label = rep('sim',nrow(sim_out)), var_upper, var_lower)

var_upper = rInt + rSlope*(sqrt(sim_out$Area) - mean(sqrt(sim_out$Area))) + 1.96 * sqrt(rSigma^2 * exp(2*(sqrt(sim_out$Area)/1e3)*rVarexp))
var_lower = rInt + rSlope*(sqrt(sim_out$Area) - mean(sqrt(sim_out$Area))) - 1.96 * sqrt(rSigma^2 * exp(2*(sqrt(sim_out$Area)/1e3)*rVarexp))
real_out = out_med$Area$real_slopes %>% mutate(label = rep('real',nrow(out_med$Area$real_slopes)), var_upper, var_lower)

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

ggsave(paste(folder,"FigS5_BasinSim.pdf", sep = ""), width = 11, height = 7.5)

#S9
#Load Data
load("02b_Missing_Data_Predictions.RData")

#Calculate Rolling Average to smooth out daily extremes.
flow.stats = Data.Preds %>% group_by(Station.ID) %>% 
	arrange(Date) %>%
	do({
		z = zoo(.$Flow.Data, .$Date)
		x = rollapply(data = z, width = 5, by = 1, FUN = mean)
		data.frame(Date = index(x), mean5day = coredata(x))
	})
Data.Preds = Data.Preds %>% left_join(., flow.stats, by  = c("Station.ID", "Date"))

coast = c("08MH006","08MH029","08MH076","08MH090")
Data.Preds = Data.Preds %>% mutate(DOYAdj = if_else(Station.ID %in% coast, yday(ymd(Date)+200), yday(Date)))

colourCount = length(unique(Data.Preds$Year))
getPalette = colorRampPalette(brewer.pal(11, "RdYlBu"))

#Change DOY to DOYAdj for adjusted DOY for coastal sites.
ggplot(Data.Preds) + 
	geom_smooth(aes(DOY, mean5day, group = Year, color = Year), se = F) + 
	scale_color_gradientn(colours = getPalette(colourCount)) +
	labs(y = expression("Flow (m"^3%.%"s"^-1*")"), x = "Day of Year") + 
	theme_classic(base_size = 9) +
	facet_wrap(~Station.ID, scales = "free")

ggsave(paste(folder,"FigS9_FlowCurvesRaw.pdf", sep = ""), width = 11, height = 7.5)
