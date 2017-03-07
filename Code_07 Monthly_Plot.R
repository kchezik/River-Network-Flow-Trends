#########################################################################################################
################## Plot Monthly Flow Trends Against Area and Display Climate Index Data #################
#########################################################################################################

library(attenPlot)#Funnel and Density plot function library.

#Density Plot Wrapper
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
		plygnPrptns(d_dat, type, varR, varS, annual = F, col1, col2, col3, col4)
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

#Funnel Plot Wrapper.
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
	cols = color.gradient(flow.trends$std.clim)
	
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
	pointsSE(flow.trends, lcol, cols, L.Width)
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
						intercept = intercept, slope = slope, sigma = sigma,
						resp.sqrt = flow.trends$sqrt.resp)
	#More extreme values.
	line.sims(df = ld.low, col = col3, LW = L.Width, shade = "10", 
						intercept = intercept, slope = slope, sigma = sigma,
						resp.sqrt = flow.trends$sqrt.resp)
	#Actual values repeated twice.
	for(i in c(1:2)) {
		line.sims(df = real, col = col2, LW = L.Width + 3.5, shade = "99", 
							intercept = intercept, slope = slope, sigma = sigma,
							resp.sqrt = flow.trends$sqrt.resp)
	}
}

load("max_month.RData")
#pdf("Fig4_Max-Monthly-Density-Funnel.pdf", width = 11, height = 8.5)
monthlyScreen()
density_plot(out_max_month, "Area", type = "int", xlimit = c(-0.008,0.017))
density_plot(out_max_month, "Area", type = "exp", xlimit = c(-12,4))
funnel.plots(dat = out_max_month$`2`, response = "Area", yaxis.DOY = F, xAxis = F, d.xaxis = F, pane = "February", axis.Ly = NULL, ylimit = c(-0.03,0.025), iter = 500)
funnel.plots(dat = out_max_month$`5`, response = "Area", yaxis.DOY = F, xAxis = F, d.xaxis = F, pane = "May", axis.Ly = NULL, ylimit = c(-0.02,0.02), iter = 500)
funnel.plots(dat = out_max_month$`8`, response = "Area", yaxis.DOY = F, xAxis = F, d.xaxis = F, pane = "August", axis.Ly = NULL, ylimit = c(-0.04,0.02), iter = 500, funnel.y = expression("Maximum-Flow | %Change"%.%"Decade"^-1))
funnel.plots(dat = out_max_month$`11`, response = "Area", yaxis.DOY = F, xAxis = T, d.xaxis = F, pane = "November", axis.Ly = NULL, ylimit = c(-0.02,0.04), iter = 500)
#dev.off()

load("min_month.RData")
#pdf("Fig4_Min-Monthly-Density-Funnel.pdf", width = 11, height = 8.5)
monthlyScreen()
density_plot(out_min_month, "Area", type = "int", xlimit = c(-0.008,0.017))
density_plot(out_min_month, "Area", type = "exp", xlimit = c(-12,4))
funnel.plots(dat = out_min_month$`2`, response = "Area", yaxis.DOY = F, xAxis = F, d.xaxis = F, pane = "February", axis.Ly = NULL, ylimit = c(-0.03,0.025), iter = 500)
funnel.plots(dat = out_min_month$`5`, response = "Area", yaxis.DOY = F, xAxis = F, d.xaxis = F, pane = "May", axis.Ly = NULL, ylimit = c(-0.025,0.04), iter = 500)
funnel.plots(dat = out_min_month$`8`, response = "Area", yaxis.DOY = F, xAxis = F, d.xaxis = F, pane = "August", axis.Ly = NULL, ylimit = c(-0.025,0.03), iter = 500, funnel.y = expression("Minimum-Flow | %Change"%.%"Decade"^-1))
funnel.plots(dat = out_min_month$`11`, response = "Area", yaxis.DOY = F, xAxis = T, d.xaxis = F, pane = "November", axis.Ly = NULL, ylimit = c(-0.02,0.04), iter = 500)
#dev.off()

load("med_month.RData")
#pdf("Fig4_Med-Monthly-Density-Funnel.pdf", width = 11, height = 8.5)
monthlyScreen()
density_plot(out_med_month, "Area", type = "int", xlimit = c(-0.008,0.017))
density_plot(out_med_month, "Area", type = "exp", xlimit = c(-12,4))
funnel.plots(dat = out_med_month$`2`, response = "Area", yaxis.DOY = F, xAxis = F, d.xaxis = F, pane = "February", axis.Ly = NULL, ylimit = c(-0.025,0.03), iter = 500)
funnel.plots(dat = out_med_month$`5`, response = "Area", yaxis.DOY = F, xAxis = F, d.xaxis = F, pane = "May", axis.Ly = NULL, ylimit = c(-0.025,0.04),  iter = 500)
funnel.plots(dat = out_med_month$`8`, response = "Area", yaxis.DOY = F, xAxis = F, d.xaxis = F, pane = "August", axis.Ly = NULL, ylimit = c(-0.025,0.03), iter = 500, funnel.y = expression("Medimum-Flow | %Change"%.%"Decade"^-1))
funnel.plots(dat = out_med_month$`11`, response = "Area", yaxis.DOY = F, xAxis = T, d.xaxis = F, pane = "November", axis.Ly = NULL, ylimit = c(-0.02,0.04), iter = 500)
#dev.off()