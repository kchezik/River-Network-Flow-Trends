#' Y-Axis Labels
#'
#' Manually or automatically define readable y-axis labels for logit transformed day of year (DOY) trend variables or log transformed trend variables.
#' @param df Requires a list with a nested dataframe of model results.
#' @param yaxis.DOY Requires a boolean T/F. This argument refers to whether or not the y-axis is logit transformed DOY or not. It defaults to False.
#' @param ylimit Allows the user to override estimates and force the bounds of the y-axis.
#' @keywords y-axis
#' @export
#' @examples
#' axis.Fun(df, yaxis.DOY = F, ylimit = c(-0.04,0.04))
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