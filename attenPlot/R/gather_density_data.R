#' Gather and Arrange Density Plot Data
#'
#' Gather and arrange observed and simulated variance and intercept parameter estimates.
#' @param df Requires a list with nested response variables with a further nested dataframe of model output.
#' @param response Requires a character object referencing one of the nested response variables in df.
#' @param type Requires a character vector of "int" if plotting intercept data. Anything less (e.g., "exp") will default to plotting variance exponent data.
#' @keywords density
#' @export
#' @examples
#' gather.density.data(df = out_doy2, response = "Area", type = "int")
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