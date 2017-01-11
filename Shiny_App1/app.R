library(shiny) #Call shiny library

#######################################
#### Initialize the User Interface ####
#######################################

ui <- fluidPage(
	includeCSS("style.css"),
	fluidRow(
		column(2),
		column(4,tags$h4("Fraser River Annual Flow", align = "center"), style = "height: 70px; border-right: 1px solid #EEEEEE; top: 25px; position: relative"),
		column(4, sliderInput(inputId = "num",
													label = "Smoother Period (Years)",
													value = 10, min = 3, max = 15, ticks = F)
					 , align = "center", offset = 1, style = "font-family: 'Raleway'; postion: relative; top: 30px"),
		column(2)
	),
	tags$hr(),
	fluidRow(
		column(12, plotOutput("plot"), align = "center")
	)
)

#Load Data
load("app_data.RData")

#Set initial parameters
time = Y.Data$Year; response = Y.Data$Med.sc; weight = Y.Data$Area; group = Y.Data$Station.ID; xlab = "Year"; ylab = expression("Scaled Median Flow (m"^3%.%"sec"^-1*")"%.%"year"^-1); seq.inc = 0.1; Te.cex = 1.3

##################################
#### Colour and Scale of Data ####
##################################

#vector scaled 0-1 for function 
col = ((weight-min(weight))/(diff(range(weight))))^0.3

#color ramp function
library(RColorBrewer)
Blues = brewer.pal(9, "Blues")#; RdYlBu = RdYlBu[-c(5,6,7)]
FUN = colorRamp(Blues[c(2:7)], bias=1)

#apply function
cols = FUN(col)
cols = rgb(cols, maxColorValue=256)
cols = paste(cols, "99", sep = "")

#####################################
#### Legend Placement and Colour ####
#####################################

#Determination of point/line size and legend point data.
Area.width = ((((weight-min(weight))/(diff(range(weight))))^0.3)+0.01)*8 #Line width.
Area.size = (((weight-min(weight))/(diff(range(weight))))^0.3)*3 #Point size.
Area.values = c(500,1500,10000,100000,200000) #Area values of interest for the legends.
Legend.Area.size = (((Area.values-min(weight))/(diff(range(weight))))^0.3)*3 #Equivalent size given legend point size values.
Legend.Area.size.lines = (((Area.values-min(weight))/(diff(range(weight))))^0.3)*8 #Equivalent size given legend line size values.
Legend.x = seq(1994,2005,length.out = 5) #x values for line/point legend.
Legend.y = rep(max(pretty(response)), 5) #y values for line/point legend.
Legend.x.col = seq(1994,2005,length.out = 20000) #x values for color legend.
Legend.y.col = rep(max(pretty(response)), 20000) #y values for color legend.

#color legend point data scaled from 0-1.
legend.col = (Legend.x.col-min(Legend.x.col))/(diff(range(Legend.x.col)))
#Color ramp function
legend.cols = FUN(legend.col)
legend.cols = rgb(legend.cols, maxColorValue=256)
#Apply function
legend.cols = paste(legend.cols, "99", sep = "")

#######################
#### Rendered Plot ####
#######################

server <- function(input, output) {
	output$plot <- renderPlot({
		#Point Plot
		par(cex.axis = Te.cex, mgp = c(3,1,0), oma = c(3,5,3,0), mar=c(0,0,0,0), family = 'serif', fg = NA, las = 1, mex = 1, mfrow = c(1,1), bg = "#F7F7F7")
		plot(response~jitter(time,factor = 2), col = cols, axes = F, xaxt = 'n', yaxt = 'n', pch = 16, cex = Area.size)
		#points(time, response, bg = cols, pch = 21, cex = Area.size)
		
		#Add loess lines
		df = data.frame(group, time, response, weight, cols, Area.width, stringsAsFactors = F)
		plyr::d_ply(df, "group", function(x){
			m = loess(response~time, model = T, span = (input$num)/38, data = x)
			lines(predict(m, data.frame(time = seq(min(x$time), max(x$time),seq.inc)))~seq(min(x$time), max(x$time),seq.inc), col = x$cols, lwd = x$Area.width)
		})
		
		points(Legend.x.col, Legend.y.col-0.32, pch = 22, bg = legend.cols, cex = 2)
		par(fg = 'black')
		points(Legend.x, Legend.y-0.13, pch = 21, cex = Legend.Area.size, bg = "black")
		text(Legend.x, Legend.y, labels = c("500","1500","10000","100000","200000"), cex = Te.cex)
		text(Legend.x[5]+2, Legend.y[5], labels = expression("km"^2), cex = Te.cex)
		
		for(i in 1:5){
			lines(c(Legend.x[i]-0.5,Legend.x[i]+0.5),c(Legend.y[i]-0.24,Legend.y[i]-0.24), lwd = Legend.Area.size.lines[i])	
		}
		mtext(xlab, side = 1, line = 2.2, cex = Te.cex+0.1, outer = TRUE) # X axis label.
		mtext(ylab, side = 2, line = 3, cex = Te.cex+0.1, las = 0, outer = TRUE) # Y axis label.
		par(mgp = c(3,1,0))
		axis(2, at = seq(0,round(range(response)[2],2),length.out = 20), labels = round(seq(0,round(range(response)[2],2),length.out = 20),2),lwd = 0.5, line = -0.8, outer = TRUE) # y axis.
		axis(1,at=seq(min(time),max(time),1), labels = as.character(seq(min(time),max(time),1)),lwd = 0.5, line = -0.8, outer = T) # x axis.
	})
}

#########################################################
#### Combine UI and Server Code to Run the Shiny App ####
#########################################################

shinyApp(ui = ui, server = server)