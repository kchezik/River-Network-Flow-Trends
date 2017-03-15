#' Draw Simulation Lines
#'
#' This function allows you to add simulation lines to plotting region.
#' @param df A dataframe containing a column of variance exponent parameters.
#' @param col A character object defining the colour of the lines.
#' @param LW A numeric object defining the width of each variance line.
#' @param shade Requires a character object between 0 and 100. The lower the value the greater the transparency. Default is "10".
#' @param transp A on/off (T/F) switch for making the lines transparent.
#' @param intercept Requires a numeric object with the observed intercept.
#' @param slope Requires a numeric object with the observed slope.
#' @param resp.sqrt Requires a numeric object with the observed response variables. This variable (resp.sqrt) was added by arrange.data().
#' @param sigma Requires a numeric object with the observed sigma.
#' @keywords simlines
#' @export
#' @examples
#' line.sims(df, col = "#000000", LW = 1, shade = "10", transp = T, intercep = 1, slope = 1, resp.sqrt = df$resp.sqrt, sigma = 2)
line.sims = function(df, col, LW, shade = "10", transp = T, intercept, slope, resp.sqrt, sigma){
	apply(df,1,function(y){
		if(transp == T) colur = paste(col,shade,sep="")
		else colur = col
		upper = intercept + slope*(resp.sqrt - mean(resp.sqrt)) + 1.96 * sqrt(sigma^2 * exp(2*(resp.sqrt/1e3)*y[1]))
		lines(resp.sqrt, upper, col = colur, lwd = LW)
		lower = intercept + slope*(resp.sqrt - mean(resp.sqrt)) - 1.96 * sqrt(sigma^2 * exp(2*(resp.sqrt/1e3)*y[1]))
		lines(resp.sqrt, lower, col = colur, lwd = LW)
	})}

# line.sims = function(df, col, LW, shade = "10", transp = T, intercept, slope, resp.sqrt, sigma, clim){
# 	apply(df,1,function(y){
# 		if(transp == T) colur = paste(col,shade,sep="")
# 		else colur = col
# 		v = resp.sqrt/sd(resp.sqrt)
# 		w = clim/sd(clim)
# 		upper = intercept + slope*(resp.sqrt - mean(resp.sqrt)) + 1.96 * sqrt(sigma^2 * exp(2*(w*v)*y[1]))
# 		#upper = predict(loess(upper~resp.sqrt, span = 1))
# 		lines(resp.sqrt, upper, col = colur, lwd = LW)
# 		lower = intercept + slope*(resp.sqrt - mean(resp.sqrt)) - 1.96 * sqrt(sigma^2 * exp(2*(w*v)*y[1]))
# 		#lower = predict(loess(lower~resp.sqrt, span = 1))
# 		lines(resp.sqrt, lower, col = colur, lwd = LW)
# 	})}