load("max_month.RData")
load("med_month.RData")
load("min_month.RData")

#Global Control Panel	
Line.Width = 1
Axis.Label.Size = 1.1
par(family = "serif", bg = "white", fg = "black")
Lab.Loc = 3.1
vline = 4
shade1="99"; shade2="10"
axisLabCol = "black"

funnel.plots = function(dat, response, col1="#F2AD00", col2="#67A9CF", col3="#F98400", col4="#FF0000", col5="#525252", pcol="#737373", lcol="#969696",funnel.y = "", axis.L, yaxis.break = F, iter = 200, xaxis = F, d.xaxis = F, pane, xcoord, ycoord, y.axis = F){
	library(tidyverse);library(wesanderson)
	#browser()
	
	#Identify predictor variable data for the given response variable.
	slot = grep(pattern = response, x = names(dat)); dat = dat[[slot]]
	#Change predictor variable column name to generic value.
	column = grep(pattern = response, x = names(dat$real_slopes))
	names(dat$real_slopes)[column] = "resp"
	
	#Real Data: Coefficients and Response Variable isolation and transformation.
	Real.Dat = dat$real_slopes %>% mutate(sqrt.resp = sqrt(resp)) %>% arrange(resp) %>% 
		mutate(se.upper = slope+se, se.lower = slope-se)
	resp = sort(Real.Dat$resp)
	
	#Initial Plot Region Parameters
	par(mar = c(0,5,0.2,0), bg = "white",fg = "white")
	
	#Create Plotting Region
	ylimits = pretty(range(scale(c(Real.Dat$se.upper,Real.Dat$se.lower),center = dat$real_varexp$intercept,scale = F)))
	label = round(exp(ylimits*10)*100-100,0)
	plot(slope~sqrt.resp, ylim = range(ylimits), xlab = "", ylab = "", axes = F, data = Real.Dat)
	
	#Add y-axis and labels
	#par(fg = "white")
	par(fg = "black")
	axis(2, lwd = Line.Width, cex.axis = Axis.Label.Size, las = 1, at = ylimits, labels = label, col.axis = axisLabCol)
	legend("topright", axis.L, bty="n", xjust = 0, cex = Axis.Label.Size)
	#mtext(axis.L, side=2, line=Lab.Loc, cex = Axis.Label.Size, las = 0)  #y-axis label.
	if(y.axis == T) {
		mtext(funnel.y, side=2, at = 0.05, line=Lab.Loc, cex = Axis.Label.Size, las = 0)  #y-axis label.
	}
	
	#Add x-axis w/wo labels.
	if(xaxis == TRUE){
		axis(1, lwd = Line.Width, cex.axis = Axis.Label.Size, labels = c("0","10000","40000","90000","160000","250000"), at = c(0,100,200,300,400,500), outer = T, hadj = 1, col.axis = axisLabCol)
		mtext(expression("Area (km"^2*")"), side = 1, line = Lab.Loc, cex = Axis.Label.Size) # x-axis label.
	} else axis(1, lwd = Line.Width, tick = TRUE, labels = FALSE, tck = 0)
	
	#Add Points and SE to Plotting Region.
	points(Real.Dat$sqrt.resp, Real.Dat$slope, col = paste(pcol,shade1,sep=""), pch = 16, cex = 1.5)
	segments(Real.Dat$sqrt.resp,Real.Dat$slope+Real.Dat$se,Real.Dat$sqrt.resp,Real.Dat$slope-Real.Dat$se, col = paste(lcol,shade1,sep=""), lwd = Line.Width)
	segments(0,0,500,0, lty = 2, lwd = Line.Width)
	
	#Add Simulation Lines to Plotting Region.
	line.sims = function(input, col="colour", line.w = Line.W, shade="30"){apply(input,1,function(y){
		sim_upper = dat$real_varexp$intercept + dat$real_varexp$slope*(sqrt(resp) - mean(sqrt(resp))) + 1.96 * sqrt(dat$real_varexp$sigma^2 * exp(2*(sqrt(resp)/1e3)*y[1]))
		lines(sqrt(resp), sim_upper, col = paste(col,shade,sep=""), lwd = line.w)
		sim_lower = dat$real_varexp$intercept + dat$real_varexp$slope*(sqrt(resp) - mean(sqrt(resp))) - 1.96 * sqrt(dat$real_varexp$sigma^2 * exp(2*(sqrt(resp)/1e3)*y[1]))
		lines(sqrt(resp), sim_lower, col = paste(col,shade,sep=""), lwd = line.w)
	})}
	line.dat = dplyr::sample_n(dat$sim_varexp, iter, weight = abs(varexp)) %>% plyr::arrange(.,.n)
	ld.up = dplyr::filter(line.dat, varexp > dat[[1]][[1]])
	ld.low = dplyr::filter(line.dat, varexp < dat[[1]][[1]])
	line.sims(ld.up, col1, Line.Width, shade2)
	line.sims(ld.low, col3, Line.Width, shade2)
	line.sims(dat$real_varexp, col2, Line.Width + 3.5, shade1)
	line.sims(dat$real_varexp, col2, Line.Width + 3.5, shade1)
	
	#Create Label For Each Panel.
	text(500,max(ylimits),labels = pane, pos = 2, cex = Axis.Label.Size)
}

density_plot = function(data, response, type, col1="#F2AD00", col2="#67A9CF", col3="#F98400", col4="#FF0000", xlimit, month.lab=FALSE){
	#browser()
	if(type == "int") par(oma = c(4,3.5,0.2,0.2), mar = c(0,0,0.2,0.5))
	else par(mar = c(0,0.5,0.2,0))
	Months = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"); count = 1
	
	plyr::l_ply(data,function(x){
		
		#Identify predictor variable data for the given response variable.
		slot = grep(pattern = response, x = names(x)); x = x[[slot]]
		#Change predictor variable column name to generic value.
		column = grep(pattern = response, x = names(x$real_slopes))
		names(x$real_slopes)[column] = "resp"
		
		if(type == "int"){
			d = density(x$sim_varexp$intercept)
			var.real = x$real_varexp$intercept}
		else{d = density(x$sim_varexp$varexp)
			var.real = x$real_varexp$varexp}
		
		d_dat = dplyr::tbl_df(data.frame(var = d$x, dens = d$y))
		#plot(dens~var, data = d_dat, type="l", xlab = "", ylab = "", las = 1,
		#		 bty = "n", yaxt = "n", xaxt = "n", xaxs = "i", xlim = xlimit, col = col1,
		#		 lwd = Line.Width)
		plot(dens~var, data = d_dat, type="l", xlab = "", ylab = "", las = 1,
				 bty = "n", yaxt = "n", xaxt = "n", xaxs = "i", col = col1,
				 lwd = Line.Width)
				 
		
		if(month.lab == T){
			mtext(Months[count],side=2, line = 2, las = 1, adj = 0)
			count <<- count+1}
		
		if(type != "int"){
			Upper = dplyr::filter(d_dat, var>var.real)
			Upper = rbind(Upper, c(var = var.real, dens = 0))
			Lower = dplyr::filter(d_dat, var<var.real)
			Lower = rbind(Lower, c(var = var.real, dens = 0))
			if(is.null(Upper)==F & nrow(Upper)>1)	polygon(Upper$var,Upper$dens, 
																										col=paste(col1,shade1,sep=""),
																										border=paste(col1,shade2,sep=""))
			if(is.null(Upper)==F & nrow(Lower)>1) polygon(Lower$var,Lower$dens, 
																										col=paste(col3,shade1,sep=""),
																										border=paste(col4,shade1,sep=""))
			prop = round(sum(var.real<x$sim_varexp$varexp)/nrow(x$sim_varexp),2)
			mtext(prop, side = 3, adj = 0.05, line = -1.3, cex = 0.75)
		}
		abline(v = var.real ,col = paste(col2,shade1,sep=""), lwd = vline)
		abline(v = var.real ,col = paste(col2,shade1,sep=""), lwd = vline)
		abline(v = 0 ,col = paste("#000000",shade1,sep=""), lwd = 1, lty = "dashed")
		#if(type == "int") axis(1, labels = F, tck = 0, at = c(-0.008,-0.004,0,0.004,0.008,0.012,0.016))
		#else axis(1, labels = F, tck = 0, at = c(-12,-8,-4,0,4))
		axis(1, labels = F, tck = 0)
	})
	if(type == "int"){
		label = round(exp(c(-0.008,-0.004,0,0.004,0.008,0.012,0.016)*10)*100-100,0)
		#axis(1, lwd = 0.5, cex.axis = Axis.Label.Size, outer = T, hadj = 1, labels = label, at = c(-0.008,-0.004,0,0.004,0.008,0.012,0.016), col.axis = axisLabCol)
		axis(1, lwd = 0.5, cex.axis = Axis.Label.Size, outer = T, hadj = 1, col.axis = axisLabCol)
		mtext(expression("Intercept | %Change"%.%"Decade"^-1), side = 1, line = Lab.Loc, cex = Axis.Label.Size)
	} else{
		#axis(1, lwd = 0.5, cex.axis = Axis.Label.Size, outer = T, hadj = 0.5, labels = c("-12","-8","-4","0","4"), at = c(-12,-8,-4,0,4), cex.axis = Axis.Label.Size, col.axis = axisLabCol)
		axis(1, lwd = 0.5, cex.axis = Axis.Label.Size, outer = T, hadj = 0.5, cex.axis = Axis.Label.Size, col.axis = axisLabCol)
		mtext(expression("Var. Exp. Param. ("*delta*")"), side = 1, line = Lab.Loc, cex = Axis.Label.Size)	
	}
}
	
	#pdf("Fig4_Max-Monthly-Density-Funnel.pdf", width = 11, height = 8.5)
	layout(cbind(c(1:12),c(1:12),c(1:12),c(1:12),c(13:24),c(13:24),c(13:24),c(13:24),c(rep(25,3),rep(26,3),rep(27,3),rep(28,3)),c(rep(25,3),rep(26,3),rep(27,3),rep(28,3)),c(rep(25,3),rep(26,3),rep(27,3),rep(28,3)),c(rep(25,3),rep(26,3),rep(27,3),rep(28,3)),c(rep(25,3),rep(26,3),rep(27,3),rep(28,3))))
	
	density_plot(out_max_month, "ppt.sd", type = "int", xlimit = c(-0.008,0.017), col1 ="#000000", col2 = "#525252",month.lab = T)
	density_plot(out_max_month, "ppt.sd", type = "exp", xlimit = c(-12,4))
	
	funnel.plots(out_max_month$`2`, "ppt.sd", y.axis = F, axis.L = "February", pane = "", iter = 500)
	funnel.plots(out_max_month$`5`, "ppt.sd", y.axis = F, axis.L = "May", pane = "", iter = 500)
	funnel.plots(out_max_month$`9`, "ppt.sd", y.axis = T, funnel.y = expression("Maximum-Flow | %Change"%.%"Decade"^-1), axis.L = "September", pane = "", iter = 500)
	funnel.plots(out_max_month$`11`, "ppt.sd", y.axis = F, axis.L = "November", xaxis=T, pane = "", iter = 500)
	#dev.off()

pdf("FigS2_Median-Monthly-Density-Funnel.pdf", width = 11, height = 8.5)
layout(cbind(c(1:12),c(1:12),c(1:12),c(1:12),c(13:24),c(13:24),c(13:24),c(13:24),c(rep(25,3),rep(26,3),rep(27,3),rep(28,3)),c(rep(25,3),rep(26,3),rep(27,3),rep(28,3)),c(rep(25,3),rep(26,3),rep(27,3),rep(28,3)),c(rep(25,3),rep(26,3),rep(27,3),rep(28,3)),c(rep(25,3),rep(26,3),rep(27,3),rep(28,3))))

density_plot(out_med_month, type = "int", xlimit = c(-0.008,0.017), col1 ="#000000", col2 = "#525252",month.lab = T)
density_plot(out_med_month, type = "exp", xlimit = c(-12,4))

funnel.plots(out_med_month$`2`, y.axis = F, axis.L = "February", pane = "", iter = 500)
funnel.plots(out_med_month$`5`, y.axis = F, axis.L = "May", pane = "", iter = 500)
funnel.plots(out_med_month$`8`, y.axis = T, funnel.y = expression("Median-Flow | %Change"%.%"Decade"^-1), axis.L = "August", pane = "", iter = 500)
funnel.plots(out_med_month$`11`, y.axis = F, axis.L = "November", xaxis=T, pane = "", iter = 500)
dev.off()

pdf("FigS1_Min-Monthly-Density-Funnel.pdf", width = 11, height = 8.5)
layout(cbind(c(1:12),c(1:12),c(1:12),c(1:12),c(13:24),c(13:24),c(13:24),c(13:24),c(rep(25,3),rep(26,3),rep(27,3),rep(28,3)),c(rep(25,3),rep(26,3),rep(27,3),rep(28,3)),c(rep(25,3),rep(26,3),rep(27,3),rep(28,3)),c(rep(25,3),rep(26,3),rep(27,3),rep(28,3)),c(rep(25,3),rep(26,3),rep(27,3),rep(28,3))))

density_plot(out_min_month, type = "int", xlimit = c(-0.008,0.017), col1 ="#000000", col2 = "#525252",month.lab = T)
density_plot(out_min_month, type = "exp", xlimit = c(-12,4))

funnel.plots(out_min_month$`2`, y.axis = F, axis.L = "February", pane = "", iter = 500)
funnel.plots(out_min_month$`5`, y.axis = F, axis.L = "May", pane = "", iter = 500)
funnel.plots(out_min_month$`8`, y.axis = T, funnel.y = expression("Minimum-Flow | %Change"%.%"Decade"^-1), axis.L = "August", pane = "", iter = 500)
funnel.plots(out_min_month$`11`, y.axis = F, axis.L = "November", xaxis=T, pane = "", iter = 500)
dev.off()
