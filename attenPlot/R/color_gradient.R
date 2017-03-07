#' Color ramp function.
#'
#' This function allows you to create a scaled color ramp for a vector of data.
#' @param df Requires a vector of data.
#' @param pal Requires a character object designating the color pallet to apply. Here we require palletes provided by RColorBrewer and default ot a Green Blue gradient pallet.
#' @param shade Requires a character object between 0 and 100. The lower the value the greater the transparency. Default is "99".
#' @keywords color
#' @export
#' @examples
#' color.gradient(vec, pal = "GnBu", shade = "99")
color.gradient = function(vec, pal = "GnBu", shade = "99"){
	library(RColorBrewer)
	#vector scaled 0-1 for function
	col = zero_one(vec)
	#color ramp function
	gradient = brewer.pal(9, pal)[3:9]
	FUN = colorRamp(gradient, bias=1)
	#apply function
	cols = FUN(col)
	cols = rgb(cols, maxColorValue=256)
	if(as.numeric(shade)<100) paste(cols, shade, sep = "")
}