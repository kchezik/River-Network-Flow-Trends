#' Density Plot Axis' and Labels.
#'
#' This function allows you to draw the density plot axis' and labels.
#' @param type Requires a character vector of "int" if plotting intercept data. Anything less (e.g., "exp") will default to plotting variance exponent data.
#' @param ALS Requires a numeric that defines the axis line size.
#' @param LL Requires a numeric that defines the margin line where the x-axis label will be drawn.
#' @param LW Requires a numeric that defines the line width.
#' @param annual Requires a T/F as to whether the data are annually summarized (T) or monthly summarized (F). Defaults to False.
#' @keywords axis, labels
#' @export
#' @examples
#' axis.density(type = "exp", ALS = 1, LL = 1)
#
axis.density.draw = function(type, ALS, LL, LW, annual = F){
	if(type == "int"){
		label = round(exp(c(-0.008,-0.004,0,0.004,0.008,0.012,0.016)*10)*100-100,0)
		axis(1, lwd = 0.5, cex.axis = ALS, outer = T, hadj = 1, labels = label, at = c(-0.008,-0.004,0,0.004,0.008,0.012,0.016))
		mtext(expression("Intercept | %Change"%.%"Decade"^-1), side = 1, line = LL, cex = ALS)
	} else{
		if(annual == T){
			mtext(expression("Var. Exp. Param. ("*delta*")"), side = 1, line = LL, cex = ALS - 0.2) # x-axis label.	
			axis(1, lwd = LW, tck = -0.04, cex.axis = ALS, outer = T)
		} else {
			axis(1, lwd = 0.5, cex.axis = ALS, outer = T, hadj = 0.5, labels = c("-12","-8","-4","0","4"), at = c(-12,-8,-4,0,4), cex.axis = ALS)
			mtext(expression("Var. Exp. Param. ("*delta*")"), side = 1, line = LL, cex = ALS)	
		}
	}
}