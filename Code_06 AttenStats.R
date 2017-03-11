#Attenuation Stats.
load("out_doy2.RData")
load("out_med.RData")
load("out_min.RData")
load("out_max.RData")

#This function finds the predicted attenuation values at any two catchment areas and returns the percent decrease in predicted range values when moving from small catchments to a large catchment.
pred.F = function(x, Area){
	prediction.1= x$real_varexp$slope*(sqrt(Area) - mean(sqrt(Area))) + 1.96 * sqrt(x$real_varexp$sigma^2 * exp(2*(sqrt(Area)/1e3)*x$real_varexp$varexp))
	prediction.2= x$real_varexp$slope*(sqrt(Area) - mean(sqrt(Area))) - 1.96 * sqrt(x$real_varexp$sigma^2 * exp(2*(sqrt(Area)/1e3)*x$real_varexp$varexp))
	percent.reduced = (1-sqrt(diff(c(prediction.1[2],prediction.2[2]))^2/diff(c(prediction.1[1],prediction.2[1]))^2))*100
	data.frame(percent.reduced)}

pred.F(out_max$Area,range(out_max$Area$real_slopes$Area))
pred.F(out_min$Area,range(out_max$Area$real_slopes$Area))
pred.F(out_med$Area,range(out_max$Area$real_slopes$Area))
pred.F(out_doy2$Area,range(out_max$Area$real_slopes$Area))
pred.F(out_med$Area,c(5000,60000))

#This function produces lower and upper range estimates for trend values among smaller catchments (Small_Area) and then at a larger catchment (Large_Area). Values are returned as the percent change per decade.
pred.F2 = function(x){
	upper = x$real_varexp$intercept + x$real_varexp$slope*(sqrt(x$real_slopes$Area) - mean(sqrt(x$real_slopes$Area))) + 1.96 * sqrt(x$real_varexp$sigma^2 * exp(2*(sqrt(x$real_slopes$Area)/1e3)*x$real_varexp$varexp))
	upper.est = round((exp(sort(range(upper),decreasing = T)*10)-1)*100,2)
	
	lower = x$real_varexp$intercept + x$real_varexp$slope*(sqrt(x$real_slopes$Area) - mean(sqrt(x$real_slopes$Area))) - 1.96 * sqrt(x$real_varexp$sigma^2 * exp(2*(sqrt(x$real_slopes$Area)/1e3)*x$real_varexp$varexp))
	lower.est = round((exp(range(lower)*10)-1)*100,2)
	data.frame(upper.est, lower.est, row.names = c("Small_Area", "Large_Area"))
	}
pred.F2(out_max$Area)
pred.F2(out_min$Area)
pred.F2(out_med$Area)

##############################################################
#     Determine the ratio of observed to null attenuation    #
##############################################################

ratio_atten = function(varexp_real, varexp_null, area1, area2){
	obs = sqrt(exp(2*sqrt(area1)/1e3*varexp_real)) / sqrt(exp(2*sqrt(area2)/1e3*varexp_real))
	null = sqrt(exp(2*sqrt(area1)/1e3*varexp_null)) / sqrt(exp(2*sqrt(area2)/1e3*varexp_null))
	print(quantile(obs / null, probs = c(0.05, 0.5, 0.95)) %>% round(1))
	#par(mfrow = c(1,2)); plot(density(obs / null)); plot(density(log10(obs/null)))
	#obs / null
}

Area_Small = min(out_med$Area$real_slopes$Area)
#Area_Small = 5000
Area_Large = max(out_med$Area$real_slopes$Area)
#Area_Large = 60000
expReal = out_max$Area$real_varexp$varexp
expNull = out_max$Area$sim_varexp$varexp
atten_compare = ratio_atten(expReal,expNull,Area_Small,Area_Large)

#Uncomment the last line of the ratio_atten function before plotting. Also, nice to comment out the print function and the plotting line for computational speed.  
max_Areas = seq(10000,215000,length.out = 100)

out = plyr::ldply(max_Areas, function(x){
	ratio_atten(expReal,expNull,5000,x)
}) %>% mutate(area = max_Areas)

ggplot(out, aes(out[,4],out[,2])) +
	geom_line() +
	geom_point() +
	geom_line(aes(out[,4],out[,1]),colour = "blue") +
	geom_line(aes(out[,4],out[,3]),colour = "blue") +
	xlab("Area") +
	ylab("Ratio") +
	theme_bw()

####################################################
#     Determine the DOY2 trends in human speak     #
####################################################

load("03_Data_Annual.RData")
logit = function(p){log(p/(1-p))}
Y.Data = plyr::ddply(Y.Data,"Station.ID",plyr::mutate, med.log.sd = scale(Median.F, center = F), max.log.sd = scale(Max.F, center = F), min.log.sd = scale(Min.F, center = F), DOY2.logit = logit(DOY2/365), Year.Center = Year-1988)

#function to predict doy to half annual flow given slope, intercept and year.
logit.pred = function(slope, intercept, year.center){data.frame(pred.points = plogis(intercept+slope*year.center))}
#expand the slope/intercept data so each site has a row for each year.
new.dat = expand.grid(Station.ID = out_doy2$real_slopes$Station.ID, year.center = sort(unique(Y.Data$Year.Center))) %>% inner_join(out_doy2$real_slopes)
#apply the logit.pred function to the new.dat dataframe.
for.plot = plyr::mdply(select(new.dat, intercept, slope, year.center), logit.pred)
#add the station.id to the predicted data.
for.plot = data.frame(for.plot, station.id = new.dat$Station.ID)
#plot the predicted doy to half annual flow over time for each site.
ggplot(for.plot, aes(year.center, pred.points*365, group = station.id))+
	geom_line(alpha = 0.2) +
	theme_classic()

#isolate the doy gls coefficients.
varexp = out_doy2$real_varexp$varexp
sigma = out_doy2$real_varexp$sigma
slope = out_doy2$real_varexp$slope
intercept = out_doy2$real_varexp$intercept
area = sort(unique(Y.Data$Area))

#predict slope function
doy.slope.pred  = function(intercept, slope, area, sigma, varexp){
	data.frame(slope.upper = (intercept + slope * (sqrt(area)-mean(sqrt(area))) + 1.96 * sqrt(sigma^2 * exp(2*(sqrt(area)/1e3)*varexp))), slope.lower = (intercept + slope * (sqrt(area)-mean(sqrt(area))) - 1.96 * sqrt(sigma^2 * exp(2*(sqrt(area)/1e3)*varexp))))
}

pred.slopes = doy.slope.pred(intercept, slope, range(area), sigma, varexp)

##### Updated upstream
get_2decade_change <- function(m) (plogis(m*19)-plogis(m*-18))*365


##### Stashed changes

m <- median(out_doy2$real_slopes$slope)
get_2decade_change(m)

m <- max(out_doy2$real_slopes$slope)
get_2decade_change(m)

m <- min(out_doy2$real_slopes$slope)
get_2decade_change(m)

get_2decade_change(pred.slopes$slope.upper)
pred.slopes$slope.upper/4*38*365

get_2decade_change(pred.slopes$slope.lower)
pred.slopes$slope.lower/4*38*365

