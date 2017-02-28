

#Function for scaling values between 0 and 1.
zero_one = function(x) ((x-min(x)))/(diff(range(x)))

#Function for simplifying data and adding climate index value.
arrange.data = function(df, response){
	tp = df[[grep(response, names(df))]]$real_slopes
	tp = tp %>% select(8:ncol(tp)) %>% apply(.,2,function(x) zero_one(x)) %>% 
		apply(.,1,function(x) sum(x)) %>% mutate(tp, std.clim = .)
	names(tp)[grep(response,names(tp))] = "resp"
	tp %>% mutate(sqrt.resp = sqrt(resp)) %>% mutate(se.upper = slope+se, se.lower = slope-se) %>% arrange(resp)
}

#Add colour gradient to climate index.
color.gradient = function(df, pal = "GnBu", shade = "99"){
	#vector scaled 0-1 for function
	col = zero_one(df$std.clim)
	#color ramp function
	RdYlBu = brewer.pal(9, pal)[3:9]
	FUN = colorRamp(RdYlBu, bias=1)
	#apply function
	cols = FUN(col)
	cols = rgb(cols, maxColorValue=256)
	paste(cols, shade, sep = "")
}

#Axis label determination
axis.Fun = function(df, yaxis.DOY=F){
	if(yaxis.DOY == T) {
		ylimits <<- pretty(c(min(df$slope), max(df$slope)))
		label <<- round((ylimits/4*38*365)/3.8,0)
	} else {
		ylimits <<- pretty(c(min(df$se.lower), max(df$se.upper)))
		label <<- round(exp(ylimits*10)*100-100,0)
	}
}

#Draw x and y axis.
axis.Draw = function(LW, ALS, ALX, ALY, LL, YLim, YLab, xAxis = T){
	if(xAxis == T){
		axis(1, lwd = LW, cex.axis = ALS, outer = T, hadj = 1,
				 labels = c("0","10000","40000","90000","160000","250000"), at = c(0,100,200,300,400,500))
		mtext(ALX, side = 1, line = LL, cex = ALS) # x-axis label.
	} else axis(1, lwd = LW, tick = TRUE, labels = FALSE, tck = 0)
	axis(2, lwd = LW, cex.axis = ALS, at = YLim, labels = YLab)
	mtext(ALY, side=2, line=LL, cex = ALS, las = 0)  #y-axis label.
}

#Add points and SE to plotting region.
points.SE = function(df, lcol, cols, LW, shade = "99"){
	segments(df$sqrt.resp, df$slope+df$se, df$sqrt.resp, df$slope-df$se,
					 col = paste(lcol, shade, sep = ""), lwd = LW)
	segments(0,0,500,0, lty = 2, lwd = LW)
	points(df$sqrt.resp, df$slope, col = cols, pch = 16, cex = 1.5)
}

#Add Simulation Lines to Plotting Region.
line.sims = function(df, col, LW, shade = "10", transp = T, intercept, slope, resp.sqrt, sigma, varexp){
	apply(df,1,function(y){
	if(transp == T) colur = paste(col,shade,sep="")
	else colur = col
	upper = intercept + slope*(resp.sqrt - mean(resp.sqrt)) + 1.96 * sqrt(sigma^2 * exp(2*(resp.sqrt/1e3)*y[1]))
	lines(resp.sqrt, upper, col = colur, lwd = LW)
	lower = intercept + slope*(resp.sqrt - mean(resp.sqrt)) - 1.96 * sqrt(sigma^2 * exp(2*(resp.sqrt/1e3)*y[1]))
	lines(resp.sqrt, lower, col = colur, lwd = LW)
	})}

#Density Plot
densityP = function(Rvarexp, Svarexp, d.axis=F, LW, ALS, Lab.Loc,	
										shade, col1, col2, col3, col4, vline, pane){
	d = density(c(Svarexp,Rvarexp), bw = "SJ")
	df = tbl_df(data.frame(varexp = d$x, dens = d$y))
	par(mar = c(0,0.3,1.5,0.5), mgp = c(2,0.60,0))
	plot(dens~varexp, data = df, type = "n", xlab = "", ylab = "", las = 1,
			 bty = "n", yaxt = "n", xaxt = "n", axes = F, xaxs = "i", xlim = c(-12,5))
	
	if(d.axis == TRUE){
		mtext(expression("Var. Exp. Param. ("*delta*")"), side = 1, line = Lab.Loc, cex = ALS - 0.2) # x-axis label.	
		axis(1, lwd = LW, tck = -0.04, cex.axis = ALS, outer = T)
	} else axis(1, lwd = LW, labels = F, tck = 0)
	
	Upper = filter(df, varexp>Rvarexp)
	Upper = rbind(Upper, c(varexp = Rvarexp, dens = 0))
	Lower = filter(df, varexp<Rvarexp)
	Lower = rbind(Lower, c(varexp = Rvarexp, dens = 0))
	
	if(is.null(Upper$varexp)==F){
		polygon(Upper$varexp, Upper$dens, col=paste(col1, shade, sep=""),
						border=paste(col3, shade, sep=""))
		prop = round(sum(Rvarexp<Svarexp)/length(Svarexp),2)
		text(2,max(df$dens)/2,
				 labels = as.character(prop), cex = ALS)
	}
	if(is.null(Lower$varexp)==F){
		polygon(Lower$varexp, Lower$dens, col=paste(col3, shade, sep=""),
						border=paste(col4, shade, sep=""))
		text(-8,max(df$dens)/2,
				 labels = as.character(round(1-prop,2)), cex = ALS)
	}
	abline(v = Rvarexp, col = paste(col2, shade, sep=""), lwd = vline)
	abline(v = Rvarexp, col = paste(col2, shade, sep=""), lwd = vline)
	text(4, max(df$dens), labels = pane, pos = 2, cex = ALS)
}


# Funnel Plot Wrapper.
funnel.wrapper = function(dat, response, yaxis.DOY = F, xAxis = T, pane, axis.Lx=NULL, axis.Ly=NULL, d.axis = F, iter=1000){
	
	############################################ Prepare Data ###############################################
	#Control Panel
	L.Width = 1; Axis.Lab.Size = 1.1; Lab.Loc = 3.1; vLine = 4
	#Colour Control Panel
	col1="#F2AD00"; col2="#67A9CF"; col3="#F98400"; col4="#FF0000"; lcol="#969696"
	#Libraries to load.
	library(tidyverse);library(wesanderson);library(RColorBrewer)
	#Cleanup and arrange observed flow and climate trend data.
	flow.trends = arrange.data(dat,response)
	#Create a colour gradient for the climate index values.
	cols = color.gradient(flow.trends)
	
	########################################### Plot Raw Data ##############################################
	#Set initial plotting parameters
	par(mar = c(0,5,0.2,0), family = "serif", bg = "white",fg = "white", mgp = c(2,0.7,0))
	#Determine axis labels.
	axis.Fun(flow.trends, yaxis.DOY)
	#Plot initial blank region to be filled.
	plot(slope~sqrt.resp, xlim = c(0,500), ylim = range(ylimits), xlab = "", ylab = "", axes = F, data = flow.trends)
	#Make plotting visible.
	par(fg = "black")
	#Draw x and y axis.
	axis.Draw(LW = L.Width, ALS = Axis.Lab.Size, ALX = axis.Lx, ALY = axis.Ly, 
						LL = Lab.Loc, YLim = ylimits, YLab = label, xAxis = xAxis)
	#Add points and SE to Plotting Region.
	points.SE(flow.trends, lcol, cols, L.Width)
	#Create Label For Each Panel.
	text(500, max(ylimits), labels = pane, pos = 2, cex = Axis.Lab.Size)
	
	###################################### Plot Variance Parameter #########################################
	#Add real/simulation variance lines.
		#Subset simulation and real data trend datasets to be used as inputs.
	sims = dat$Area$sim_varexp 
	real = dat$Area$real_varexp
	Rvarexp = real$varexp; sigma = real$sigma; intercept = real$intercept; slope = real$slope
		#Sample simulation lines to be plotted and break them into those above and below the real variance param.
	sim.lines = sample_n(sims, iter) %>% plyr::arrange(.,.n)
	ld.up = filter(sim.lines, varexp > Rvarexp);	ld.low = filter(sim.lines, varexp < Rvarexp)
		#Draw lines.
			#Less extreme values.
	line.sims(df = ld.up, col = col1, LW = L.Width, shade = "10", 
						intercept = intercept, slope = slope, sigma = sigma, varexp = Rvarexp,
						resp.sqrt = flow.trends$sqrt.resp)
			#More extreme values.
	line.sims(df = ld.low, col = col3, LW = L.Width, shade = "10", 
						intercept = intercept, slope = slope, sigma = sigma, varexp = Rvarexp,
						resp.sqrt = flow.trends$sqrt.resp)
			#Actual values repeated twice.
	for(i in c(1:2)) {
		line.sims(df = real, col = col2, LW = L.Width + 3.5, shade = "99", 
							intercept = intercept, slope = slope, sigma = sigma, varexp = Rvarexp,
							resp.sqrt = flow.trends$sqrt.resp)
	}
	
	######################################## Density Plot ##################################################
	densityP(Rvarexp = Rvarexp, Svarexp = sims$varexp, 
					 d.axis = d.axis, LW = L.Width, ALS = Axis.Lab.Size, Lab.Loc = Lab.Loc,
					 shade = "99", col1 = col1, col2 = col2, col3 = col3, col4 = col4, vline = vLine, pane = pane)
}

load("out_doy2.RData")
load("out_min.RData")
load("out_max.RData")
load("out_med.RData")

pdf("Fig3_Annual-Funnel.pdf", width = 11, height = 8.5)
layout(rbind(c(1,1,3,3,2),c(1,1,3,3,4),c(5,5,7,7,6),c(5,5,7,7,8)))
par(oma = c(4,0,0,0))
funnel.wrapper(out_doy2, "Area", pane = "a", xAxis = F, yaxis.DOY = T,  
							 axis.Ly = expression("DOY"~scriptstyle(frac(1,2))~"Annual Flow | Days"%.%"Decade"^-1))
funnel.wrapper(out_min, "Area", pane = "b", xAxis = F, 
							 axis.Ly = expression("Minimum-Flow | %Change"%.%"Decade"^-1))
funnel.wrapper(out_max, "Area", pane = "c",
							 axis.Lx = expression("Area (km"^2*")"),
							 axis.Ly = expression("Maximum-Flow | %Change"%.%"Decade"^-1))
funnel.wrapper(out_med, "Area", pane = "d", d.axis = T,
							 axis.Lx = expression("Area (km"^2*")"),
							 axis.Ly = expression("Median-Flow | %Change"%.%"Decade"^-1))
dev.off()