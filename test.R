library(attenPlot); library(tidyverse)
n = exp(seq(log(35),log(250000), by = 0.2))
index.sd = -1.3575 + log(n)*0.3869
plot(index.sd~sqrt(n))
df = expand.grid(n = n, sd.samp = c(1:1000)) %>% left_join(.,data.frame(n = n, index.sd = index.sd))

df = df %>% mutate(sd.sim = apply(., 1, function(x){
		sd(rnorm(n = x[["n"]], mean = 10000, sd = x[["index.sd"]]))
	}))

ggplot(df,aes(log(n),sd.sim)) + geom_point() + geom_abline(intercept = 1) + geom_line(aes(log(n),index.sd), color = "blue")