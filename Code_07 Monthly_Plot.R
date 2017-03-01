###########################################################################################################
######################################## Sub-Functions ####################################################
###########################################################################################################


############################################ Funnel Plot Functions ###############################################

#Function for scaling values between 0 and 1.
zero_one = function(x) ((x-min(x)))/(diff(range(x)))

#Function for simplifying data and adding climate index values.
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
axis.Fun = function(df, yaxis.DOY=F, ylimit=NULL){
	if(yaxis.DOY == T) {
		ylimits <<- pretty(c(min(df$slope), max(df$slope)))
		label <<- round((ylimits/4*38*365)/3.8,0)
	} else {
		ylimits <<- pretty(c(min(df$se.lower), max(df$se.upper)))
		label <<- round(exp(ylimits*10)*100-100,0)
	}
	if(is.null(ylimit)==F){
		ylimits <<- pretty(ylimit)
		label <<- round(exp(ylimits*10)*100-100,0)
	}
}

#Draw x and y axis.
axis.Draw = function(LW, ALS, ALY, LL, YLim, YLab, xAxis = F){
	if(xAxis == T){
		axis(1, lwd = LW, cex.axis = ALS, outer = T, hadj = 1,
				 labels = c("0","10000","40000","90000","160000","250000"), at = c(0,100,200,300,400,500))
		mtext(expression("Area (km"^2*")"), side = 1, line = LL, cex = ALS) # x-axis label.
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

############################################ Density Plot Functions ###############################################

#Gather and arrange observed and simulated variance and intercept parameter estimates.
gather.density.data = function(df, response, type){
	tp = df[[grep(response, names(df))]]
	Rvarexp = tp$real_varexp$varexp 
	Svarexp = tp$sim_varexp$varexp
	Rint = tp$real_varexp$intercept
	Sint = tp$sim_varexp$intercept
	
	if(type == "int"){
		d <<- density(Sint)
		var.real <<- Rint}
	else{d <<- density(Svarexp)
	var.real <<- Rvarexp}
	varR <<- Rvarexp
	varS <<- Svarexp
}

#Plot proportional plots of the variance parameter depicting the simulated values greather than and less than the observed.
plygnPrptns = function(df, type, varR, varS, col1, col3, col4, shade = "99"){
	if(type != "int"){
		Upper = filter(df, var>varR) %>% bind_rows(., data.frame(var = varR, dens = 0))
		Lower = filter(df, var<varR) %>% bind_rows(., data.frame(var = varR, dens = 0))
		if(is.null(Upper)==F & nrow(Upper)>1)	polygon(Upper$var,Upper$dens, 
																									col=paste(col1,shade,sep=""),
																									border=paste(col1,shade,sep=""))
		if(is.null(Upper)==F & nrow(Lower)>1) polygon(Lower$var,Lower$dens, 
																									col=paste(col3,shade,sep=""),
																									border=paste(col4,shade,sep=""))
		prop = round(sum(varR<varS)/length(varS),2)
		mtext(prop, side = 3, adj = 0.05, line = -1.3, cex = 0.75)
	}
}

#Draw density plot axis' and labels.
axis.density.draw = function(type, ALS, Lab.Loc){
	if(type == "int"){
		label = round(exp(c(-0.008,-0.004,0,0.004,0.008,0.012,0.016)*10)*100-100,0)
		axis(1, lwd = 0.5, cex.axis = ALS, outer = T, hadj = 1, labels = label, at = c(-0.008,-0.004,0,0.004,0.008,0.012,0.016))
		mtext(expression("Intercept | %Change"%.%"Decade"^-1), side = 1, line = Lab.Loc, cex = ALS)
	} else{
		axis(1, lwd = 0.5, cex.axis = ALS, outer = T, hadj = 0.5, labels = c("-12","-8","-4","0","4"), at = c(-12,-8,-4,0,4), cex.axis = ALS)
		mtext(expression("Var. Exp. Param. ("*delta*")"), side = 1, line = Lab.Loc, cex = ALS)	
	}
}




###########################################################################################################
############################################### Wrappers ##################################################
###########################################################################################################


density_plot = function(data, response, type, xlimit){
	
	############################################ Prepare Data ###############################################
	#Libraries to load.
	library(tidyverse);library(wesanderson);library(RColorBrewer);library(lubridate)
	#Control Panel
	L.Width = 1; Axis.Lab.Size = 1.1; Lab.Loc = 3.1; vLine = 4
	#Colour Control Panel
	col1 = if_else(type == "int","#000000","#F2AD00"); col2= if_else(type=="int","#525252","#67A9CF")
	col3="#F98400"; col4="#FF0000"; lcol="#969696"; shade = "99"
	#Determine if we are plotting intercept (int) or exponent (exp) data and set boundaries accordingly.
	if(type == "int") par(oma = c(4,3.5,0.2,0.2), mar = c(0,0,0.2,0.5), family = "serif")
	else par(mar = c(0,0.5,0.2,0), family = "serif")
	#Create list of month labels to iterate through.
	Months = as.character(lubridate::month(as.numeric(names(data)), label = T)); count = 1
	
	
	plyr::l_ply(data,function(x){
		######## Gather and arrange observed and simulated variance and intercept parameter estimates. ########
		gather.density.data(x,response, type)
		d_dat = dplyr::tbl_df(data.frame(var = d$x, dens = d$y))
		
		########################################### Plot Raw Data ##############################################
		plot(dens~var, data = d_dat, type="l", xlab = "", ylab = "", las = 1,
				 bty = "n", yaxt = "n", xaxt = "n", xaxs = "i", xlim = xlimit, col = col1,
				 lwd = L.Width)
		#Add month label if plotting intercept (i.e., far left on page) data.
		if(type == "int") mtext(Months[count],side=2, line = 2, las = 1, adj = 0); count <<- count+1
		#If plotting variance exponent parameter data, then provide proportions and polygons.
		plygnPrptns(d_dat, type, varR, varS, col1, col3, col4)
		#Add vertical line for the real int/exp value.
		for(i in 1:2){abline(v = var.real ,col = paste(col2,shade,sep=""), lwd = vLine)}
		#Add dotted vertical line at zero.
		abline(v = 0 ,col = paste("#000000",shade,sep=""), lwd = 1, lty = "dashed")
		#Add an x-axis line without labels
		if(type == "int") axis(1, labels = F, tck = 0, at = c(-0.008,-0.004,0,0.004,0.008,0.012,0.016))
		else axis(1, labels = F, tck = 0, at = c(-12,-8,-4,0,4))
	})
	axis.density.draw(type, Axis.Lab.Size, Lab.Loc)
}



funnel.plots = function(dat, response, yaxis.DOY = F, d.xaxis = F, pane, funnel.y = "", axis.Ly = "", xAxis = F, ylimit = NULL, iter = 200){
	library(tidyverse);library(wesanderson)
	
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
	par(mar = c(0,5,0.2,0), family = "serif", bg = "white",fg = "white")
	#Determine axis labels.
	axis.Fun(flow.trends, yaxis.DOY, ylimit)
	#Plot initial blank region to be filled.
	plot(slope~sqrt.resp, xlim = c(0,500), ylim = range(ylimits), xlab = "", ylab = "", axes = F, data = flow.trends)
	#Make plotting visible.
	par(fg = "black")
	#Draw x and y axis.
	axis.Draw(LW = L.Width, ALS = Axis.Lab.Size, ALY = axis.Ly, 
						LL = Lab.Loc, YLim = ylimits, YLab = label, xAxis = xAxis)
	#Add points and SE to Plotting Region.
	points.SE(flow.trends, lcol, cols, L.Width)
	#Create Label For Each Panel.
	text(500, max(ylimits), labels = pane, pos = 2, cex = Axis.Lab.Size)
	mtext(funnel.y, side=2, at = 0.05, line=Lab.Loc, cex = Axis.Lab.Size, las = 0)  #y-axis label.
	
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
}
#Split up the display for two columns of twelve density plots and one column of 4 funnel plots.
screen = function(){layout(cbind(c(1:12),c(1:12),c(1:12),c(1:12),c(13:24),c(13:24),c(13:24),c(13:24),c(rep(25,3),rep(26,3),rep(27,3),rep(28,3)),c(rep(25,3),rep(26,3),rep(27,3),rep(28,3)),c(rep(25,3),rep(26,3),rep(27,3),rep(28,3)),c(rep(25,3),rep(26,3),rep(27,3),rep(28,3)),c(rep(25,3),rep(26,3),rep(27,3),rep(28,3))))}

load("max_month.RData")
#pdf("Fig4_Max-Monthly-Density-Funnel.pdf", width = 11, height = 8.5)
screen()
density_plot(out_max_month, "Area", type = "int", xlimit = c(-0.008,0.017))
density_plot(out_max_month, "Area", type = "exp", xlimit = c(-12,4))
funnel.plots(dat = out_max_month$`2`, response = "Area", yaxis.DOY = F, xAxis = F, d.xaxis = F, pane = "February", axis.Ly = NULL, ylimit = c(-0.03,0.025), iter = 500)
funnel.plots(dat = out_max_month$`5`, response = "Area", yaxis.DOY = F, xAxis = F, d.xaxis = F, pane = "May", axis.Ly = NULL, ylimit = c(-0.02,0.02), iter = 500)
funnel.plots(dat = out_max_month$`8`, response = "Area", yaxis.DOY = F, xAxis = F, d.xaxis = F, pane = "August", axis.Ly = NULL, ylimit = c(-0.04,0.02), iter = 500, funnel.y = expression("Maximum-Flow | %Change"%.%"Decade"^-1))
funnel.plots(dat = out_max_month$`11`, response = "Area", yaxis.DOY = F, xAxis = T, d.xaxis = F, pane = "November", axis.Ly = NULL, ylimit = c(-0.02,0.04), iter = 500)
#dev.off()

load("min_month.RData")
#pdf("Fig4_Min-Monthly-Density-Funnel.pdf", width = 11, height = 8.5)
screen()
density_plot(out_min_month, "Area", type = "int", xlimit = c(-0.008,0.017))
density_plot(out_min_month, "Area", type = "exp", xlimit = c(-12,4))
funnel.plots(dat = out_min_month$`2`, response = "Area", yaxis.DOY = F, xAxis = F, d.xaxis = F, pane = "February", axis.Ly = NULL, ylimit = c(-0.03,0.025), iter = 500)
funnel.plots(dat = out_min_month$`5`, response = "Area", yaxis.DOY = F, xAxis = F, d.xaxis = F, pane = "May", axis.Ly = NULL, ylimit = c(-0.025,0.04), iter = 500)
funnel.plots(dat = out_min_month$`8`, response = "Area", yaxis.DOY = F, xAxis = F, d.xaxis = F, pane = "August", axis.Ly = NULL, ylimit = c(-0.025,0.03), iter = 500, funnel.y = expression("Minimum-Flow | %Change"%.%"Decade"^-1))
funnel.plots(dat = out_min_month$`11`, response = "Area", yaxis.DOY = F, xAxis = T, d.xaxis = F, pane = "November", axis.Ly = NULL, ylimit = c(-0.02,0.04), iter = 500)
#dev.off()

load("med_month.RData")
#pdf("Fig4_Med-Monthly-Density-Funnel.pdf", width = 11, height = 8.5)
screen()
density_plot(out_med_month, "Area", type = "int", xlimit = c(-0.008,0.017))
density_plot(out_med_month, "Area", type = "exp", xlimit = c(-12,4))
funnel.plots(dat = out_med_month$`2`, response = "Area", yaxis.DOY = F, xAxis = F, d.xaxis = F, pane = "February", axis.Ly = NULL, ylimit = c(-0.025,0.03), iter = 500)
funnel.plots(dat = out_med_month$`5`, response = "Area", yaxis.DOY = F, xAxis = F, d.xaxis = F, pane = "May", axis.Ly = NULL, ylimit = c(-0.025,0.04),  iter = 500)
funnel.plots(dat = out_med_month$`8`, response = "Area", yaxis.DOY = F, xAxis = F, d.xaxis = F, pane = "August", axis.Ly = NULL, ylimit = c(-0.025,0.03), iter = 500, funnel.y = expression("Medimum-Flow | %Change"%.%"Decade"^-1))
funnel.plots(dat = out_med_month$`11`, response = "Area", yaxis.DOY = F, xAxis = T, d.xaxis = F, pane = "November", axis.Ly = NULL, ylimit = c(-0.02,0.04), iter = 500)
#dev.off()