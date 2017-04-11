#' Add Points and Standard Errors
#'
#' This function allows you to add points and SE lines to plotting region.
#' @param df Requires a dataframe with slope (slope), standard error (se) and sqrt reponse variable (sqrt.resp) columns. The sqrt.resp variable column should be produced by the arrange.data() function.
#' @param lcol Requires a character object defining the color of the lines. The default is grey.
#' @param cols Requires a character vector of either 1 or the same length as the number of plot points. These values should be output from the color.gradient() function.
#' @param LW Requires a numeric value that defines the line width of the SE segments.
#' @param shade Requires a character object between 0 and 100. The lower the value the greater the transparency. Default is "99".
#' @keywords points, SE-lines
#' @export
#' @examples
#' points.SE(df, lcol = "#969696", cols = cols, LW = 1, shade = "99")
pointsSE = function(df, lcol ="#969696", cols, LW, shade = "99", ptsize = 1){
	segments(df$sqrt.resp, df$slope+df$se, df$sqrt.resp, df$slope-df$se,
					 col = paste(lcol, shade, sep = ""), lwd = LW)
	segments(0,0,500,0, lty = 2, lwd = LW)
	points(df$sqrt.resp, df$slope, col = cols, pch = 16, cex = ptsize)
}