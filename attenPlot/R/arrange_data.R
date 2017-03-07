#' Simplify data and add climate index values.
#'
#' This function allows you to summarize climate data into a single index values across variables. It requires the zero_one() function.
#' @param df Requires a list with nested response variables with a further nested dataframe of model output. This dataframe must contain 7 initial columns of model results with all subsequent columns containing climate data.
#' @param response Requires a character object referencing one of the nested response variables in df.
#' @keywords climate index
#' @export
#' @examples
#' arrange.data(df = out_doy2, response = "Area")
#
arrange.data = function(df, response){
	tp = df[[grep(response, names(df))]]$real_slopes
	tp = tp %>% select(8:ncol(tp)) %>% apply(.,2,function(x) zero_one(x)) %>% 
		apply(.,1,function(x) sum(x)) %>% mutate(tp, std.clim = .)
	names(tp)[grep(response,names(tp))] = "resp"
	tp %>% mutate(sqrt.resp = sqrt(resp)) %>% mutate(se.upper = slope+se, se.lower = slope-se) %>% arrange(resp)
}