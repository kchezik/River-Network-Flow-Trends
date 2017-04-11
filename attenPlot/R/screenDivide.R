#' Break Up Canvas
#'
#' This function allows you to break up the screen canvas into two columns of twelve for density plots and a column of 4 rows for representitive seasonal funnel plots.
#' @param empty No arguments required.
#' @keywords scale
#' @export
#' @examples
#' zero_one()
monthlyScreen = function(IntExp = 4, Atten = 5){layout(cbind(matrix(data = c(1:12), nrow = 12, ncol = IntExp),matrix(data = c(13:24), nrow = 12, ncol = IntExp),matrix(c(rep(25,3),rep(26,3),rep(27,3),rep(28,3)), nrow = 12, ncol = Atten)))}

#' Break Up Canvas
#'
#' This function allows you to break up the screen canvas into 2 columns of 2 for funnel plots and a column of 4 rows for density plots.
#' @param empty No arguments required.
#' @keywords scale
#' @export
#' @examples
#' zero_one()
annualScreen = function(){layout(rbind(c(1,1,3,3,2),c(1,1,3,3,4),c(5,5,7,7,6),c(5,5,7,7,8)))}