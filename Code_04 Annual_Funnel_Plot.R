#########################################################################################################
################## Plot Annual Flow Trends Against Area and Display Climate Index Data ##################
#########################################################################################################

library(attenPlot) #Funnel and Density plot function library.

#Density Plot Wrapper
densityP = function(df, response, type = "exp", d.axis=F, LW=L.Width, ALS=Axis.Lab.Size, LL,	
										shade = "99", col1, col2, col3, col4, vline, pane){
	
	gather.density.data(df, response, type)
	d_dat = dplyr::tbl_df(data.frame(var = d$x, dens = d$y))
	
	par(mar = c(0,0.3,1.5,0.5), mgp = c(2,0.60,0))
	plot(dens~var, data = d_dat, type = "n", xlab = "", ylab = "", las = 1,
			 bty = "n", yaxt = "n", xaxt = "n", axes = F, xaxs = "i", xlim = c(-12,5))
	
	if(d.axis == TRUE){
		axis.density.draw(type = type, ALS = ALS+0.2, LL = LL, LW = LW, annual = T)
	} else axis(1, lwd = LW, labels = F, tck = 0)
	
	plygnPrptns(d_dat, type, annual = T, varR, varS)
	
	for(i in 1:2) abline(v = varR, col = paste(col2, shade, sep=""), lwd = vline)
	text(4, max(d_dat$dens)-0.01, labels = pane, pos = 2, cex = ALS+0.2)
}


#Funnel Plot Wrapper.
funnel.wrapper = function(dat, response, yaxis.DOY = F, xAxis = T, pane, axis.Ly=NULL, d.axis = F, iter=1000){
	
	############################################ Prepare Data ###############################################
	#Control Panel
	L.Width = 0.5; Axis.Lab.Size = 0.7; LabLoc = 2.1; vLine = 2.5
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
	par(mar = c(0,3.5,0.2,0), family = "serif", bg = "white",fg = "white", mgp = c(2,0.7,0), las = 1)
	#Determine axis labels.
	axis.Fun(flow.trends, yaxis.DOY)
	#Plot initial blank region to be filled.
	plot(slope~sqrt.resp, xlim = c(0,500), ylim = range(ylimits), xlab = "", ylab = "", axes = F, data = flow.trends)
	#Make plotting visible.
	par(fg = "black")
	#Draw x and y axis.
	axis.Draw(LW = L.Width, ALS = Axis.Lab.Size, ALY = axis.Ly, 
						LL = LabLoc, YLim = ylimits, YLab = label, xAxis = xAxis)
	#Add points and SE to Plotting Region.
	pointsSE(flow.trends, lcol, cols, L.Width)
	#Create Label For Each Panel.
	text(500, max(ylimits), labels = pane, pos = 2, cex = Axis.Lab.Size+0.3)
	
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
		line.sims(df = real, col = col2, LW = L.Width + 2.5, shade = "99", 
							intercept = intercept, slope = slope, sigma = sigma,
							resp.sqrt = flow.trends$sqrt.resp)
	}
	
	######################################## Density Plot ##################################################
	densityP(df = dat, response = response, d.axis = d.axis, LW = L.Width, ALS = Axis.Lab.Size, LL = LabLoc, vline = vLine, col2 = col2, pane = pane)
}

load("out_doy2.RData")
load("out_min.RData")
load("out_max.RData")
load("out_med.RData")

pdf("Fig3_Annual-Funnel.pdf", width = 7, height = 4.25)
annualScreen()
par(oma = c(3,0,0,0))
funnel.wrapper(out_doy2, "Area", pane = "a", xAxis = F, yaxis.DOY = T,  
							 axis.Ly = expression("DOY"~scriptstyle(frac(1,2))~"Annual Flow | Days"%.%"Decade"^-1))
funnel.wrapper(out_min, "Area", pane = "b", xAxis = F, 
							 axis.Ly = expression("Minimum-Flow | %Change"%.%"Decade"^-1))
funnel.wrapper(out_max, "Area", pane = "c",
							 axis.Ly = expression("Maximum-Flow | %Change"%.%"Decade"^-1))
funnel.wrapper(out_med, "Area", pane = "d", d.axis = T,
							 axis.Ly = expression("Median-Flow | %Change"%.%"Decade"^-1))
dev.off()