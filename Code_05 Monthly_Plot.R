#########################################################################################################
################## Plot Monthly Flow Trends Against Area and Display Climate Index Data #################
#########################################################################################################

library(attenPlot)#funnel and density plot function library.

#density plot wrapper
density_plot = function(data, response, type, xlimit){
	
	############################################ prepare data ###############################################
	#libraries to load.
	library(tidyverse);library(wesanderson);library(RColorBrewer);library(lubridate)
	#control panel
	l.width = 0.5; axis.lab.size = 0.7; lab.loc = 2.1; vline = 2
	#colour control panel
	col1 = if_else(type == "int","#000000","#f2ad00"); col2= if_else(type=="int","#525252","#67a9cf")
	col3="#f98400"; col4="#ff0000"; lcol="#969696"; shade = "99"
	#determine if we are plotting intercept (int) or exponent (exp) data and set boundaries accordingly.
	if(type == "int") par(oma = c(3,1.5,0.2,0.2), mar = c(0,0,0.1,0.3), mgp = c(3,0.5,0), family = "serif")
	else par(mar = c(0,0.3,0.1,0), family = "serif")
	#create list of month labels to iterate through.
	months = as.character(lubridate::month(as.numeric(names(data)), label = T)); count = 1
	
	plyr::l_ply(data,function(x){
		
		######## gather and arrange observed and simulated variance and intercept parameter estimates. ########
		gather.density.data(x,response, type)
		d_dat = dplyr::tbl_df(data.frame(var = d$x, dens = d$y))
		
		########################################### plot raw data ##############################################
		plot(dens~var, data = d_dat, type="l", xlab = "", ylab = "", las = 1,
				 bty = "n", yaxt = "n", xaxt = "n", xaxs = "i", xlim = xlimit, col = col1,
				 lwd = l.width)
		#add month label if plotting intercept (i.e., far left on page) data.
		if(type == "int") mtext(months[count], side=2, line = 1.35, las = 1, adj = 0, cex = axis.lab.size); count <<- count+1
		#if plotting variance exponent parameter data, then provide proportions and polygons.
		plygnPrptns(d_dat, type, varR, varS, annual = F, col1, col2, col3, col4)
		#add vertical line for the real int/exp value.
		for(i in 1:2){abline(v = var.real ,col = paste(col2,shade,sep=""), lwd = vline)}
		#add dotted vertical line at zero.
		abline(v = 0 ,col = paste("#000000",shade,sep=""), lwd = 1, lty = "dashed")
		#add an x-axis line without labels
		if(type == "int") axis(1, labels = F, tck = 0, at = c(-0.008,-0.004,0,0.004,0.008,0.012,0.016))
		else axis(1, labels = F, tck = 0, at = c(-12,-8,-4,0,4))
	})
	axis.density.draw(type, axis.lab.size, lab.loc)
}

#funnel plot wrapper.
funnel.plots = function(dat, response, yaxis.doy = F, d.xaxis = F, pane, funnel.y = "", axis.ly = "", xaxis = F, ylimit = NULL, iter = 200, monthly = F, min = NA, max = NA){
	library(tidyverse);library(wesanderson)
	
	############################################ prepare data ###############################################
	#control panel
	l.width = 0.5; axis.lab.size = 0.7; lab.loc = 1.5; vline = 2
	#colour control panel
	col1="#f2ad00"; col2="#67a9cf"; col3="#f98400"; col4="#ff0000"; lcol="#969696"
	#libraries to load.
	library(tidyverse);library(wesanderson);library(RColorBrewer)
	#cleanup and arrange observed flow and climate trend data.
	flow.trends = arrange.data(dat,response)
	#create a colour gradient for the climate index values.
	cols = color.gradient(flow.trends$std.clim, monthly = monthly, min = min, max = max)
	
	########################################### plot raw data ##############################################
	
	#set initial plotting parameters
	par(mar = c(0,2.7,0.2,0), family = "serif", bg = "white",fg = "white", las = 1)
	#determine axis labels.
	axis.Fun(flow.trends, yaxis.doy, ylimit)
	#plot initial blank region to be filled.
	plot(slope~sqrt.resp, xlim = c(0,500), ylim = range(ylimits), xlab = "", ylab = "", axes = F, data = flow.trends)
	#make plotting visible.
	par(fg = "black")
	#draw x and y axis.
	axis.Draw(LW = l.width, ALS = axis.lab.size-0.3, ALY = axis.ly, 
						LL = lab.loc+0.6, YLim = ylimits, YLab = label, xAxis = xaxis, ALabS = axis.lab.size)
	#add points and se to plotting region.
	pointsSE(flow.trends, lcol, cols, l.width, ptsize = 0.75)
	#create label for each panel.
	text(500, mean(c(max(ylimits),ylimits[length(ylimits)-1])), labels = pane, pos = 2, cex = axis.lab.size)
	mtext(funnel.y, side=2, at = 0.03, line=lab.loc, cex = axis.lab.size, las = 0)  #y-axis label.
	
	###################################### plot variance parameter #########################################
	#add real/simulation variance lines.
	#subset simulation and real data trend datasets to be used as inputs.
	sims = dat$Area$sim_varexp 
	real = dat$Area$real_varexp
	rvarexp = real$varexp; sigma = real$sigma; intercept = real$intercept; slope = real$slope
	#sample simulation lines to be plotted and break them into those above and below the real variance param.
	sim.lines = sample_n(sims, iter) %>% plyr::arrange(.,.n)
	ld.up = filter(sim.lines, varexp > rvarexp);	ld.low = filter(sim.lines, varexp < rvarexp)
	#draw lines.
	#less extreme values.
	line.sims(df = ld.up, col = col1, LW = l.width, shade = "10", 
						intercept = intercept, slope = slope, sigma = sigma,
						resp.sqrt = flow.trends$sqrt.resp)
	#more extreme values.
	line.sims(df = ld.low, col = col3, LW = l.width, shade = "10", 
						intercept = intercept, slope = slope, sigma = sigma,
						resp.sqrt = flow.trends$sqrt.resp)
	#actual values repeated twice.
	for(i in c(1:2)) {
		line.sims(df = real, col = col2, LW = l.width + 1.5, shade = "99", 
							intercept = intercept, slope = slope, sigma = sigma,
							resp.sqrt = flow.trends$sqrt.resp)
	}
}

maxminclim = function(dat){
	tp = range(unlist(lapply(dat,function(x){
		range(x$Area$real_slopes$std.clim)
	})))
	minc <<- tp[1]
	maxc <<- tp[2]
}


load("max_month.rdata")
maxminclim(out_max_month)
pdf("fig4_max-monthly-density-funnel.pdf", width = 5.75, height = 4.75)
monthlyScreen(IntExp = 7, Atten = 10)
density_plot(out_max_month, "Area", type = "int", xlimit = c(-0.008,0.017))
density_plot(out_max_month, "Area", type = "exp", xlimit = c(-12,4))
funnel.plots(dat = out_max_month$`2`, response = "Area", yaxis.doy = F, xaxis = F, d.xaxis = F, pane = "February", axis.ly = NULL, ylimit = c(-0.03,0.025), iter = 500, monthly = T, min = minc, max = maxc)
funnel.plots(dat = out_max_month$`5`, response = "Area", yaxis.doy = F, xaxis = F, d.xaxis = F, pane = "May", axis.ly = NULL, ylimit = c(-0.02,0.02), iter = 500, monthly = T, min = 0.0290354, max = 4.7576590)
funnel.plots(dat = out_max_month$`8`, response = "Area", yaxis.doy = F, xaxis = F, d.xaxis = F, pane = "August", axis.ly = NULL, ylimit = c(-0.04,0.02), iter = 500, funnel.y = expression("Maximum-Flow | %Change"%.%"Decade"^-1), monthly = T, min = minc, max = maxc)
funnel.plots(dat = out_max_month$`11`, response = "Area", yaxis.doy = F, xaxis = T, d.xaxis = F, pane = "November", axis.ly = NULL, ylimit = c(-0.02,0.04), iter = 500, monthly = T, min = minc, max = maxc)
dev.off()

load("min_month.rdata")
#pdf("fig4_min-monthly-density-funnel.pdf", width = 11, height = 8.5)
monthlyscreen()
density_plot(out_min_month, "Area", type = "int", xlimit = c(-0.008,0.017))
density_plot(out_min_month, "Area", type = "exp", xlimit = c(-12,4))
funnel.plots(dat = out_min_month$`2`, response = "Area", yaxis.doy = F, xaxis = F, d.xaxis = F, pane = "February", axis.ly = NULL, ylimit = c(-0.03,0.025), iter = 500, monthly = T, min = minc, max = maxc)
funnel.plots(dat = out_min_month$`5`, response = "Area", yaxis.doy = F, xaxis = F, d.xaxis = F, pane = "May", axis.ly = NULL, ylimit = c(-0.025,0.04), iter = 500, monthly = T, min = minc, max = maxc)
funnel.plots(dat = out_min_month$`8`, response = "Area", yaxis.doy = F, xaxis = F, d.xaxis = F, pane = "August", axis.ly = NULL, ylimit = c(-0.025,0.03), iter = 500, funnel.y = expression("Minimum-Flow | %Change"%.%"Decade"^-1), monthly = T, min = minc, max = maxc)
funnel.plots(dat = out_min_month$`11`, response = "area", yaxis.doy = F, xaxis = T, d.xaxis = F, pane = "November", axis.ly = NULL, ylimit = c(-0.02,0.04), iter = 500, monthly = T, min = minc, max = maxc)
#dev.off()

load("med_month.rdata")
#pdf("fig4_med-monthly-density-funnel.pdf", width = 11, height = 8.5)
monthlyscreen()
density_plot(out_med_month, "Area", type = "int", xlimit = c(-0.008,0.017))
density_plot(out_med_month, "Area", type = "exp", xlimit = c(-12,4))
funnel.plots(dat = out_med_month$`2`, response = "Area", yaxis.doy = F, xaxis = F, d.xaxis = F, pane = "February", axis.ly = NULL, ylimit = c(-0.025,0.03), iter = 500, monthly = T, min = minc, max = maxc)
funnel.plots(dat = out_med_month$`5`, response = "Area", yaxis.DOY = F, xAxis = F, d.xaxis = F, pane = "May", axis.Ly = NULL, ylimit = c(-0.025,0.04),  iter = 500, monthly = T, min = minC, max = maxC)
funnel.plots(dat = out_med_month$`8`, response = "Area", yaxis.DOY = F, xAxis = F, d.xaxis = F, pane = "August", axis.Ly = NULL, ylimit = c(-0.025,0.03), iter = 500, funnel.y = expression("Medimum-Flow | %Change"%.%"Decade"^-1), monthly = T, min = minC, max = maxC)
funnel.plots(dat = out_med_month$`11`, response = "Area", yaxis.DOY = F, xAxis = T, d.xaxis = F, pane = "November", axis.Ly = NULL, ylimit = c(-0.02,0.04), iter = 500, monthly = T, min = minC, max = maxC)
#dev.off()