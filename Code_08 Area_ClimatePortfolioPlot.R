load("out_doy2.RData")
library(tidyverse); library(attenPlot)
df = out_doy2$Area$real_slopes
df = df %>% select(8:ncol(.)) %>% apply(.,2,function(x) zero_one(x)) %>% 
	apply(.,1,function(x) sum(x)) %>% mutate(df, clim.sd = ., logArea = log(Area), sqrtArea = sqrt(Area), cols = "#000000", loc = "interior")

#Model to add line to plot.
remove = c("08MH006","08MH076","08MH090","08MH056","08LG016","08LG048","08MH103","08MH001","08MH016","08MH029")
df$cols[which(df$Station.ID%in%remove)] = "#969696"
df$loc[which(df$Station.ID%in%remove)] = "coastal"
ggplot(df,aes(clim.sd, slope, color = cols)) + geom_point()

mod1 = lm(clim.sd~logArea, data = df, subset = which(Station.ID%in%remove==F))
mod2 = lm(clim.sd~logArea, data = df, subset = which(Station.ID%in%remove==T))
plot(clim.sd~logArea, data = df)
abline(mod1)
abline(mod2, lty = 2)

sim = data.frame(logArea = log(seq(100,250000,by = 100)))
preds1 = predict.lm(object = mod1, newdata = sim, se.fit = T)
preds2 = predict.lm(object = mod2, newdata = sim, se.fit = T)
preds = data.frame(area = exp(sim$logArea), preds1 = preds1$fit, se1 = preds1$se, preds2 = preds2$fit, se2 = preds2$se)

pdf("Fig2_ClimatePortfolio.pdf", width = 7.5, height = 5.25)

LW = 1; ALS = 1.1; LL = 3.1
par(oma = c(4,0,0,0), mar = c(0,5,0.2,0), family = "serif", bg = "white",fg = "white", mgp = c(2,0.7,0))
plot(clim.sd~sqrtArea, data = df, xlab = "", ylab = "", axes = F, xlim = c(0,sqrt(250000)), ylim = c(0,5))
par(fg = "black")
axis(1, lwd = LW, cex.axis = ALS, outer = T, hadj = 1,
		 labels = c("0", "10000", "40000", "90000", "160000", "250000"), at = c(0, 100, 200, 300, 400, 500))
mtext(expression("Area (km"^2 * ")"), side = 1, line = LL, 
			cex = ALS)
axis(2, lwd = LW, cex.axis = ALS, las = 1)
mtext("Climate Portfolio", side = 2, line = LL, cex = ALS, las = 0)
points(df$sqrtArea, df$clim.sd, pch = 16, cex = 1.5, col = df$cols)

lines(sqrt(preds$area), preds$preds2, col = "#F98400")
upper = data.frame(se = preds$preds2 + preds$se2, area = sqrt(preds$area))
lower = data.frame(se = preds$preds2 - preds$se2, area = sqrt(preds$area)) %>% arrange(desc(area))
poly = bind_rows(upper,lower) %>% select(area, se)
polygon(poly, col = "#96969660", lty = 1, border = "#96969660")

lines(sqrt(preds$area), preds$preds1, col = "#67A9CF")
upper = data.frame(se = preds$preds1 + preds$se1, area = sqrt(preds$area))
lower = data.frame(se = preds$preds1 - preds$se1, area = sqrt(preds$area)) %>% arrange(desc(area))
poly = bind_rows(upper,lower) %>% select(area, se)
polygon(poly, col = "#96969699", lty = 1, border = "#96969699")




library(nlme)
mod3 = lme(clim.sd~logArea, random = ~1+logArea|loc, data = df, control = lmeControl(niterEM = 1000, msMaxIter = 1000))

sim1 = data.frame(logArea = log(seq(100,250000,by = 100)), loc = "interior")
sim2 = data.frame(logArea = log(seq(100,250000,by = 100)), loc = "coastal")
sim = bind_rows(sim1,sim2)
sim$loc = as.factor(sim$loc)
preds.Mean = predict(object = mod3, newdata = sim, level = 0, se.Fit = T)
preds.Rand = predict(object = mod3, newdata = sim, level = 1)
preds = data.frame(area = exp(sim1$logArea), mean = preds.Mean[1:2500], randInterior = preds.Rand[1:2500], randCoastal = preds.Rand[2501:5000])

#create design matrix
Designmat <- model.matrix(eval(eval(mod3$call$fixed)[-2]), sim[-ncol(sim)])

#compute standard error for predictions
predvar <- diag(Designmat %*% mod3$varFix %*% t(Designmat))
preds$SE <- sqrt(predvar)[1:2500]
preds$SE2 <- sqrt(predvar+mod3$sigma^2)[1:2500]


# LW = 1; ALS = 1.1; LL = 3.1
# par(oma = c(4,0,0,0), mar = c(0,5,0.2,0), family = "serif", bg = "white",fg = "white", mgp = c(2,0.7,0))
# plot(clim.sd~sqrtArea, data = df, xlab = "", ylab = "", axes = F, xlim = c(0,sqrt(250000)), ylim = c(0,5))
# par(fg = "black")
# axis(1, lwd = LW, cex.axis = ALS, outer = T, hadj = 1,
# 		 labels = c("0", "10000", "40000", "90000", "160000", "250000"), at = c(0, 100, 200, 300, 400, 500))
# mtext(expression("Area (km"^2 * ")"), side = 1, line = LL, 
# 			cex = ALS)
# axis(2, lwd = LW, cex.axis = ALS, las = 1)
# mtext("Climate Portfolio", side = 2, line = LL, cex = ALS, las = 0)
# points(df$sqrtArea, df$clim.sd, pch = 16, cex = 1.5, col = df$cols)

lines(sqrt(preds$area), preds$mean, col = "#67A9CF", lty = 2)
upper = data.frame(se = preds$mean + preds$SE, area = sqrt(preds$area))
lower = data.frame(se = preds$mean - preds$SE, area = sqrt(preds$area)) %>% arrange(desc(area))
poly = bind_rows(upper,lower) %>% select(area, se)
polygon(poly, col = "#96969660", lty = 1, border = "#96969660")

lines(sqrt(preds$area), preds$randInterior, col = "#F2AD0060", lwd = 2)
lines(sqrt(preds$area), preds$randCoastal, col = "#F2AD0060", lwd = 2)

dev.off()





