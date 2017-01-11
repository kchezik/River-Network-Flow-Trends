library(dplyr);library(nlme);library(ggplot2)
load("02a_Data_Clean.RData")
names(Data) <- tolower(names(Data))
dat = Data; rm(Data)

#Split up the date for processesing.
dat = dat %>% group_by(station.id) %>% 
	mutate(.,flow.scale = scale(flow.data,center = F))
dat$year.center = dat$year-1988

doParallel::registerDoParallel(cores = parallel::detectCores()-1)

lmeCoef = plyr::ddply(dat,"station.id",function(x){
	#browser()
	print(unique(x$station.id))
	mod = lme(log(flow.scale) ~ year.center, random = ~1+year|doy, data = x, control = lmeControl(maxIter = 5000L, msMaxIter = 5000L))
	int = mod$coefficients$fixed[[1]]
	slope = mod$coefficients$fixed[[2]]
	se = summary(mod)[[20]][4]
	data.frame(int,slope,se)
},.parallel = F)

load("01_Data-Wtshd-Orig.RData")
names(WS.2) = c("station.id","lat","long","area")
lmeCoef = left_join(lmeCoef,WS.2,by = "station.id")

limits = aes(ymax = slope + se, ymin = slope - se)
ggplot(lmeCoef, aes(sqrt(area),slope)) +
	geom_point() +
	geom_errorbar(limits) + 
	theme_classic()

m <- gls(slope~sqrt(area), data = lmeCoef, weights = varExp(form= ~sqrt(area)/1e3), 
				 control = glsControl(maxIter = 1000L, msMaxIter = 1000L))
summary(m)




#Analaysis by single site.
samp.dat = filter(dat,station.id == "08MH103")
mod = lme(log(flow.scale) ~ year.center, random = ~1+year|doy, data = samp.dat, control = lmeControl(maxIter = 5000L, msMaxIter = 5000L))
summary(mod)
mod.fit = data.frame(mod$fitted)
names(mod.fit) = c("fixed", "rand")
samp.dat = bind_cols(samp.dat, mod.fit)

ggplot(samp.dat, aes(year,log(flow.scale), by = factor(doy), colour = doy)) +
	geom_point() +
	geom_line(aes(year, rand)) +
	geom_line(aes(year, fixed), colour = "black", size = 3) +
	theme_classic()
	
plot(mod)
qqnorm(mod)
plot(mod$residuals~factor(samp.dat$year))
plot(mod$residuals~factor(samp.dat$doy))
plot(acf(mod$residuals[,"fixed"]))
plot(Variogram(mod))
plot(samp.dat$rand~samp.dat$doy)
