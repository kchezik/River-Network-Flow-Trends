#' 0 to 1 scaling function.
#'
#' This function allows you to scale data between 0 and 1.
#' @param x Requires a vector of data.
#' @keywords scale
#' @export
#' @examples
#' zero_one()
zero_one = function(x) ((x-min(x)))/(diff(range(x)))