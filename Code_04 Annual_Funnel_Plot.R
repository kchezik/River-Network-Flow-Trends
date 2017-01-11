load("out_doy2.RData")
load("out_min.RData")
load("out_max.RData")
load("out_med.RData")

funnel.plots = function(x, col1="#737373", col2="#252525", col3="#737373",col4="#252525", pcol="#737373", lcol="#969696", shade1="99", shade2="10", axis.L, yaxis.break = F, iter = 200, xaxis = F, d.xaxis = F, pane, xcoord, ycoord){
	library(magrittr);library(wesanderson)
	#Real Data: Coefficients and Area
	Real.Dat = dplyr::tbl_df(x$real_slopes)%>%plyr::mutate(sqrt.Area = sqrt(Area))%>%plyr::arrange(Area)%>%plyr::mutate(., se.upper = slope+se, se.lower = slope-se); Area = sort(x$real_slopes$Area)
	#Initial Plot Region Parameters
	par(mar = c(0,5,0.2,0), family = "serif", bg = "white",fg = "white")

	#Control Panel
	Line.Width = 1
	Axis.Label.Size = 1.1
	Lab.Loc = 3.1
	vline = 4
	
	#Identify Plot Limits and Create Plotting Region with Axis.
	if(yaxis.break == TRUE){
		ylimits = pretty(c(min(Real.Dat$slope),max(Real.Dat$slope)))
		label = round((ylimits/4*38*365)/3.8,0)
	} else{
		ylimits = pretty(c(min(Real.Dat$se.lower), max(Real.Dat$se.upper)))
		label = round(exp(ylimits*10)*100-100,0)
	}
		plot(slope~sqrt.Area, xlim = c(0,500), ylim = range(ylimits), xlab = "", ylab = "", axes = F, data = Real.Dat)
		par(fg = "black")
		axis(2, lwd = Line.Width, cex.axis = Axis.Label.Size, las = 1, mgp = c(2,0.70,0), at = ylimits, labels = label)
		mtext(axis.L, side=2, line=Lab.Loc, cex = Axis.Label.Size, las = 0)  #y-axis label.
	
	if(xaxis == TRUE){
		axis(1, lwd = Line.Width, cex.axis = Axis.Label.Size, labels = c("0","10000","40000","90000","160000","250000"), at = c(0,100,200,300,400,500), mgp = c(2,0.70, 0), outer = T, hadj = 1)
		mtext(expression("Area (km"^2*")"), side = 1, line = Lab.Loc, cex = Axis.Label.Size) # x-axis label.
	} else axis(1, lwd = Line.Width, tick = TRUE, labels = FALSE, tck = 0)
	
	#Add Points and SE to Plotting Region.
	points(Real.Dat$sqrt.Area,Real.Dat$slope, col = paste(pcol,shade1,sep=""), pch = 16, cex = 1.5)
	segments(Real.Dat$sqrt.Area,Real.Dat$slope+Real.Dat$se,Real.Dat$sqrt.Area,Real.Dat$slope-Real.Dat$se, col = paste(lcol,shade1,sep=""), lwd = Line.Width)
	segments(0,0,500,0, lty = 2, lwd = Line.Width)
	
	#Add Simulation Lines to Plotting Region.
	line.sims = function(dat, col="colour", line.w = Line.W, shade="30", transp = T){apply(dat,1,function(y){
		if(transp == T) colur = paste(col,shade,sep="")
			else colur = col
		sim_upper = x$real_varexp$intercept + x$real_varexp$slope*(sqrt(Area) - mean(sqrt(Area))) + 1.96 * sqrt(x$real_varexp$sigma^2 * exp(2*(sqrt(Area)/1e3)*y[1]))
		lines(sqrt(Area), sim_upper, col = colur, lwd = line.w)
		sim_lower = x$real_varexp$intercept + x$real_varexp$slope*(sqrt(Area) - mean(sqrt(Area))) - 1.96 * sqrt(x$real_varexp$sigma^2 * exp(2*(sqrt(Area)/1e3)*y[1]))
		lines(sqrt(Area), sim_lower, col = colur, lwd = line.w)
	})}
	line.dat = dplyr::sample_n(x$sim_varexp, iter, weight = abs(varexp)) %>% plyr::arrange(.,.n)
	ld.up = dplyr::filter(line.dat, varexp > x[[1]][[1]])
	ld.low = dplyr::filter(line.dat, varexp < x[[1]][[1]])
	line.sims(ld.up, col1, Line.Width, shade2)
	line.sims(ld.low, col3, Line.Width, shade2)
	line.sims(x$real_varexp, col2, Line.Width + 3.5, shade1, transp = T)
	line.sims(x$real_varexp, col2, Line.Width + 3.5, shade1, transp = T)
	
	#Create Label For Each Panel.
	text(500,max(ylimits),labels = pane, pos = 2, cex = Axis.Label.Size)
	
	## Density Plot
	d = density(c(x$sim_varexp$varexp,x$real_varexp[[1]]), bw = "SJ")
	dat = dplyr::tbl_df(data.frame(varexp = d$x, dens = d$y)) # returns the density data
	par(mar = c(0,0.3,1.5,0.5))
	plot(dens~varexp, data = dat, type = "n", xlab = "", ylab = "", las = 1,
			 bty = "n", yaxt = "n", xaxt = "n", axes = F, xaxs = "i", xlim = c(-12,5))
	
	if(d.xaxis == TRUE){
		mtext(expression("Var. Exp. Param. ("*delta*")"), side = 1, line = Lab.Loc, cex = Axis.Label.Size - 0.2) # x-axis label.	
		axis(1, lwd = Line.Width, tck = -0.04, cex.axis = Axis.Label.Size, mgp = c(2,0.60,0), outer = T)
	} else axis(1, lwd = Line.Width, labels = F, tck = 0)
	
	Upper = dplyr::filter(dat, varexp>x[[1]][[1]])
	Upper = rbind(Upper, c(varexp = x[[1]][[1]],dens = 0))
	Lower = dplyr::filter(dat, varexp<x[[1]][[1]])
	Lower = rbind(Lower, c(varexp = x[[1]][[1]],dens = 0))
	
	if(is.null(Upper$varexp)==F){
		polygon(Upper$varexp,Upper$dens, col=paste(col1,shade1,sep=""),
						border=paste(col3,shade1,sep=""))
		prop = round(sum(x$real_varexp$varexp<x$sim_varexp$varexp)/nrow(x$sim_varexp),2)
		text(2,max(dat$dens)/2,
				 labels = as.character(prop), cex = Axis.Label.Size)
	}
	if(is.null(Lower$varexp)==F){
		polygon(Lower$varexp,Lower$dens, col=paste(col3,shade1,sep=""),
						border=paste(col4,shade1,sep=""))
		text(-8,max(dat$dens)/2,
				 labels = as.character(round(1-prop,2)), cex = Axis.Label.Size)
	}
	abline(v = x[[1]][[1]], col = paste(col2,shade1,sep=""), lwd = vline)
	abline(v = x[[1]][[1]], col = paste(col2,shade1,sep=""), lwd = vline)
	text(4,max(dat$dens),labels = pane, pos = 2, cex = Axis.Label.Size)
}

pdf("Fig3-Annual-Funnel.pdf", width = 11, height = 8.5)
layout(rbind(c(1,1,3,3,2),c(1,1,3,3,4),c(5,5,7,7,6),c(5,5,7,7,8)))
par(oma = c(4,0,0,0))
funnel.plots(out_doy2, col1="#F2AD00", col2="#67A9CF", col3="#F98400", col4="#FF0000", axis.L = expression("DOY"~scriptstyle(frac(1,2))~"Annual Flow | Days"%.%"Decade"^-1), yaxis.break = T, iter = 1000, pane = "a")

funnel.plots(out_min, col1="#F2AD00", col2="#67A9CF", col3="#F98400", col4="#FF0000", axis.L = expression("Minimum-Flow | %Change"%.%"Decade"^-1), iter = 1000, pane = "b")

funnel.plots(out_max, col1="#F2AD00", col2="#67A9CF", col3="#F98400", col4="#FF0000", axis.L = expression("Maximum-Flow | %Change"%.%"Decade"^-1), iter = 1000, xaxis = T, pane = "c")

funnel.plots(out_med, col1="#F2AD00", col2="#67A9CF", col3="#F98400", col4="#FF0000", axis.L = expression("Median-Flow | %Change"%.%"Decade"^-1), iter = 1000, xaxis = T, d.xaxis = T, pane = "d")
dev.off()


#Attenuation Stats.

#This function finds the predicted attenuation values at any two catchment areas and returns the percent decrease in predicted range values when moving from small catchments to a large catchment.
pred.F = function(x, Area){
	prediction.1= x$real_varexp$slope*(sqrt(Area) - mean(sqrt(Area))) + 1.96 * sqrt(x$real_varexp$sigma^2 * exp(2*(sqrt(Area)/1e3)*x$real_varexp$varexp))
	prediction.2= x$real_varexp$slope*(sqrt(Area) - mean(sqrt(Area))) - 1.96 * sqrt(x$real_varexp$sigma^2 * exp(2*(sqrt(Area)/1e3)*x$real_varexp$varexp))
	percent.reduced = (1-sqrt(diff(c(prediction.1[2],prediction.2[2]))^2/diff(c(prediction.1[1],prediction.2[1]))^2))*100
	data.frame(percent.reduced)}

pred.F(out_max,range(out_max$real_slopes$Area))
pred.F(out_min,range(out_max$real_slopes$Area))
pred.F(out_med,range(out_max$real_slopes$Area))
pred.F(out_doy2,range(out_max$real_slopes$Area))
pred.F(out_med,c(5000,60000))

#This function produces lower and upper range estimates for trend values among smaller catchments (Small_Area) and then at a larger catchment (Large_Area). Values are returned as the percent change per decade.
pred.F2 = function(x){
	upper = x$real_varexp$intercept + x$real_varexp$slope*(sqrt(x$real_slopes$Area) - mean(sqrt(x$real_slopes$Area))) + 1.96 * sqrt(x$real_varexp$sigma^2 * exp(2*(sqrt(x$real_slopes$Area)/1e3)*x$real_varexp$varexp))
	upper.est = round((exp(sort(range(upper),decreasing = T)*10)-1)*100,2)
	
	lower = x$real_varexp$intercept + x$real_varexp$slope*(sqrt(x$real_slopes$Area) - mean(sqrt(x$real_slopes$Area))) - 1.96 * sqrt(x$real_varexp$sigma^2 * exp(2*(sqrt(x$real_slopes$Area)/1e3)*x$real_varexp$varexp))
	lower.est = round((exp(range(lower)*10)-1)*100,2)
	data.frame(upper.est, lower.est, row.names = c("Small_Area", "Large_Area"))
	}
pred.F2(out_max)
pred.F2(out_min)
pred.F2(out_med)

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

Area_Small = min(out_med$real_slopes$Area)
#Area_Small = 5000
Area_Large = max(out_med$real_slopes$Area)
#Area_Large = 60000
expReal = out_max$real_varexp$varexp
expNull = out_max$sim_varexp$varexp
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

