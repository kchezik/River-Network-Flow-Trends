#' Draw x and y axis.
#'
#' This function allows you to draw the x and y axis'.
#' @param LW Requires a numeric that defines the line width.
#' @param ALS Requires a numeric that defines the axis line size.
#' @param ALY Requires a character object that defines the y-axis label.
#' @param LL Requires a numeric that defines the margin line where the x-axis label will be drawn.
#' @param YLim Requires a numeric vector for the location of tick mark labels. These values are output from axis.Fun().
#' @param YLab Require a character vector fo the labels of tick mark labels. These values are output from axis.Fun().
#' @param xAxis Requires a boolean T/F and determines whether the x-axis is drawn. xAxis defaults to False.
#' @keywords draw axis'
#' @export
#' @examples
#' axis.Draw(LW = 1, ALS = 1, ALY = "Y-Axis", LL = 1, YLim = ylimits, YLab = labels, xAxis = T)
axis.Draw = function(LW, ALS, ALY, LL, YLim, YLab, xAxis = F){
	if(xAxis == T){
		axis(1, lwd = LW, cex.axis = ALS, outer = T, hadj = 1,
				 labels = c("0","10000","40000","90000","160000","250000"), at = c(0,100,200,300,400,500))
		mtext(expression("Area (km"^2*")"), side = 1, line = LL, cex = ALS) # x-axis label.
	} else axis(1, lwd = LW, tick = TRUE, labels = FALSE, tck = 0)
	axis(2, lwd = LW, cex.axis = ALS, at = YLim, labels = YLab, las = 1)
	mtext(ALY, side=2, line=LL, cex = ALS, las = 0)  #y-axis label.
}