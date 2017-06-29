load("out_med.RData")
library(MASS);library(tidyverse); library(attenPlot);
df = out_med$Area$real_slopes
df = df %>% mutate(logArea = log(Area), sqrtArea = sqrt(Area), cols = "#000000", loc = "interior")

#Model to add line to plot.
remove = c("08MH006","08MH076")
#"08MH090","08MH056","08LG016","08LG048","08MH103","08MH001","08MH016","08MH029"
df$cols[which(df$Station.ID%in%remove)] = "#969696"
df$loc[which(df$Station.ID%in%remove)] = "coastal"
ggplot(df,aes(exp(std.clim), slope, color = cols)) + geom_point()

#Look at each climate variable and the index against area.
gather(data = df, key = "climateIndex", value = "IndexValue", emt.sd, ext.sd, map.sd, mat.sd, pas.sd, std.clim) %>% 
	ggplot(., aes(logArea, IndexValue, label = Station.ID)) + geom_text(check_overlap = T) + geom_point(aes(color = cols)) + facet_wrap(~climateIndex, scales = "free_y") + theme(legend.position="none")

#Model with weighted regresssion to downweight outliers/leverage points.
mod1 = rlm(std.clim~logArea, data = df)
se = summary(mod1)[[4]][4]
int = summary(mod1)[[4]][1]
slope = summary(mod1)[[4]][2]

sim = log(seq(100,250000,by = 100))
preds = int + sim*slope
se_upper = preds + (2*se)
se_lower = preds - (2*se)
preds = data.frame(area = exp(sim), preds = preds, se_upper = se_upper, se_lower = se_lower)
#par(mar = c(0,0.3,1.5,0.5), mgp = c(2,0.60,0))
wdth = 8.7/2.54; hght = wdth*0.6
pdf("Fig2_ClimPort.pdf", width = wdth, height = hght)

LW = 1; ALS = 0.7; LL = 1.2
par(oma = c(2,0,0,0), mar = c(0,1.8,0.2,0), family = "serif", bg = "white", fg = "white", mgp = c(2,0.2,0))
plot(std.clim~sqrtArea, data = df, xlab = "", ylab = "", axes = F, xlim = c(0,sqrt(250000)), ylim = c(0,4))
par(fg = "black")
axis(1, lwd = LW, cex.axis = ALS, outer = T, hadj = 1,
		 labels = c("0", "10000", "40000", "90000", "160000", "250000"), at = c(0, 100, 200, 300, 400, 500))
mtext(expression("Area (km"^2 * ")"), side = 1, line = LL, cex = ALS)
par(mgp = c(2,0.6,0))
axis(2, lwd = LW, cex.axis = ALS, las = 1)
mtext("Climate Variability Index", side = 2, line = LL, cex = ALS, las = 0)
points(df$sqrtArea, df$std.clim, pch = 16, cex = 0.9, col = "#000000")

#lines(sqrt(preds$area), preds$preds, col = "black")
#upper = data.frame(se = preds$se_upper, area = sqrt(preds$area))
#lower = data.frame(se = preds$se_lower, area = sqrt(preds$area)) %>% arrange(desc(area))
#poly = bind_rows(upper,lower) %>% dplyr::select(area, se)
#polygon(poly, col = "#96969699", lty = 1, border = NA)

dev.off()