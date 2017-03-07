#' Plot Density Plots
#'
#' Plot proportional plots of the variance parameter depicting the simulated values greather than and less than the observed.
#' @param df Requires a dataframe with model simulation results. Must have a column of "var".
#' @param type Requires a character vector of "int" if plotting intercept data. Anything less (e.g., "exp") will default to plotting variance exponent data.
#' @param varR Observed numeric value of the variance exponent. Output generated from gather_density_data().
#' @param varS Simulated numeric values of the variance exponent. Output generated from gather_density_data().
#' @param annual Requires a T/F as to whether the data are annually summarized (T) or monthly summarized (F). Defaults to False.
#' @param col1 Polygon color (character HEX) of simulated variance exponent parameters that are less extreme than the observed.
#' @param col1 Line color (character HEX) of real variance exponent parameter.
#' @param col3 Polygon color (character HEX) of simulated variance exponent parameters that are more extreme than the observed.
#' @param col4 Outline color  (character HEX) of simulated variance exponent parameters that are more extreme than the observed.
#' @param shade Requires a character object between 0 and 100. The lower the value the greater the transparency. Default is "99".
#' @keywords Plot, Density
#' @export
#' @examples
#' plygnPrptns(df, type = "exp", varR = varR, varS = varS, col1 = "#F2AD00", col3 = "#F98400", col4 = "#FF0000")
plygnPrptns = function(df, type, varR, varS, annual = F, col1="#F2AD00", col2 = "#67A9CF", col3="#F98400", col4="#FF0000", shade = "99"){
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
		if(annual == T) {
			text(2,max(df$dens)/2,
					 labels = as.character(prop), cex = 1.1)
			text(-8,max(df$dens)/2,
					 labels = as.character(round(1-prop,2)), cex = 1.1)
		} else mtext(prop, side = 3, adj = 0.05, line = -1.3, cex = 0.75)
	}
}