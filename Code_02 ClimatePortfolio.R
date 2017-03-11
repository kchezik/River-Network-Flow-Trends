library(raster);library(sp);library(rgdal);library(GISTools);library(maptools) #GIS tools.
library(readr);library(tidyverse);library(nlme);library(multidplyr) #Data analysis tools.

##############################
#### Pre BC_Climate Tool ####
##############################

#Output a shapefile grid to predict climate data on after having clipped the grid by the Fraser Basin.
setwd("~/sfuvault/Simon_Fraser_University/PhD_Research/Projects/Data/Original_Data/GIS_Data/BC_Climate_Data/Point_Grid")
WGS84 = crs("+init=epsg:4326"); BCAlbers = crs("+init=epsg:3005") #Coordinate reference systems.
#Create a raster defined by an extent where grid points are seperated by 100m.
ras = raster(ext = extent(881827, 1553827, 434750, 1242550), crs = BCAlbers, resolution = c(1000,1000), vals = 1)
#Transform to points and from WGS84 to Albers BC.
grid = rasterToPoints(ras,spatial = T)
grid = spTransform(x = grid, CRSobj = WGS84)
#Add necessary columns for climate analysis.
grid$ID1 = c(1:nrow(grid))
grid$ID2 = c(2:(nrow(grid)+1))
grid$lat = coordinates(obj = grid)[,2]
grid$long = coordinates(obj = grid)[,1]
grid$el = NA
grid = grid[,-1]
#Transform to points back to Albers BC.
grid = spTransform(x = grid, CRSobj = BCAlbers)
#Clip grid by fraser basin.
fraser = readOGR(dsn = "../FraserWtshdAnalysis", layer = "fraser", p4s = c("+init=epsg:3005"))
grid = grid[fraser,]
#Extract elevation data from the clipped grid dataset.
dem = raster::raster(x = "../../DEMs/BC_DEM/dem_merge/IntMerDEM.tif")
grid$el = raster::extract(x = dem, y = grid, method = 'simple')
grid = data.frame(grid); grid = grid %>% select(-x,-y,-optional)
#Divide into many files for analysis in BC_Climate.
init = 1
for(i in 1:8){
	if(i == 8) end = nrow(grid)
	else end = round((nrow(grid)/8)*i,0)
	x = grid[c(init:end),]
	write.table(x, file = paste("FS",i,".csv", sep=""), quote=F, sep=",", row.names = F)
	init = end+1
}

##############################
#### Post BC_Climate Tool ####
##############################

################## Calculate Trends #####################
#Set working directory.
setwd("~/sfuvault/Simon_Fraser_University/PhD_Research/Projects/Data/Original_Data/GIS_Data/BC_Climate_Data/ClimateTrends/raw_data")

#Loop through all the climate data files.
for(i in dir()){
	#Report how long each loop takes.
	system.time({
		
		#Read in ClimateWNA data and select the columns of interest.
		df = read_csv(i) %>% 
			select(Year, ID1, Latitude, Longitude, Elevation, MAT, MAP,PAS, EMT, EXT, contains("Tave"), contains("Tmax"), contains("Tmin"), contains("PPT"), contains("PAS"), -contains("_wt"),-contains("_sp"), -contains("_sm"), -contains("_at"))
		names(df) = tolower(names(df)) #Change column headers to lower case.
		df1 = df %>% gather(key = measure, value = value, mat:pas12) #Tranform the data from wide to long format.
		
		#Set up clusters.
		df2 = partition(df1, id1)
		cluster_library(df2, "dplyr") #Load necessary libraries across cores.
		cluster_library(df2, "tidyr") #Load necessary libraries across cores.
		cluster_library(df2, "nlme") #Load necessary libraries across cores.
		
		#Perfrom Analysis
		df3 = df2 %>% 
			group_by(id1, measure) %>%
			do({
				tryCatch({
					mod = gls(value~year, data = ., correlation = corAR1())
					data.frame(slope = coef(mod)[[2]])
				}, error = function(e){
					data.frame(slope = NA)
				})
			})
		
		#Collect analysis results from clusters and compile into a single object.
		df4 = collect(df3)
		browser()
		df5 = df4 %>% spread(key = measure, value = slope)
		df6 = df %>% select(id1, latitude, longitude, elevation) %>% 
			left_join(.,df5,by="id1") %>% distinct()%>% 
			write_csv(paste("../trends_split/",i,sep = ""))
	})
	rm(df,df1,df2,df3,df4,df5)
}

####################################################

setwd("../trends_split")
#Gather Together all the sites trend results.
df = dir() %>% 
	lapply(read_csv) %>% 
	bind_rows()
write_rds(x = df, path = "../climate_trends.rds")

df = read_rds(path  = "../climate_trends.rds") %>% data.frame()
attr = names(df)[5:69]
write.table(attr, "../trends/attributes.txt", row.names = F, col.names = F, quote = F)

#For QGIS viewing and raster creation. Remember I've multiplied the trend by 10^7 to retain the precision of the trend estimates as they are veryclose together for some variables.
WGS84 = crs("+init=epsg:4326"); BCAlbers = crs("+init=epsg:3005") #Coordinate reference systems.
coords = df[,c("longitude","latitude")]
sp = df %>% select(emt:tmin12) %>% do(. * 10^7)
sp = df %>% select(1:4) %>% bind_cols(., sp) %>% data.frame()
sp = SpatialPointsDataFrame(coords = coords, data = sp, proj4string = WGS84)
sp = spTransform(x = sp, CRSobj = BCAlbers)
writeOGR(obj=sp, dsn="../../ClimateTrends", layer="trends", driver="ESRI Shapefile", overwrite_layer = T, morphToESRI = T)


#Produce individual pour points for each Fraser sub-watershed.
#Three pour points need to be manually moved in Whitebox as the vector estimtes of flow path for these three sites are dissimilar enough that they will not be properly snapped to proper river. These sites are... 08KH006, 08LD001 and 08LE031.
setwd("~/sfuvault/Simon_Fraser_University/PhD_Research/Projects/Data/Original_Data/GIS_Data/BC_Climate_Data/FraserWtshdAnalysis")
sites = readOGR(dsn = "sites", layer = "05_Sites", p4s = "+init=epsg:3005", stringsAsFactors = F, drop_unsupported_fields = T) 
sites = bind_cols(data.frame(sites@data[,2]),data.frame(sites@coords))
names(sites) = c("geolocid","utm_e","utm_n")

sites %>% group_by(geolocid) %>% do({
	coords = select(., utm_e,utm_n) %>% data.frame()
	BCAlbers = crs("+init=epsg:3005")
	sp = data.frame(.)
	sp = SpatialPointsDataFrame(data = sp, coords = coords, proj4string = BCAlbers)
	writeOGR(sp, dsn = "pour_pts", layer = sp$geolocid, driver="ESRI Shapefile", overwrite_layer = T, morphToESRI = T)
})

####################################################################################
#### After Watershed Delineation and Raster to Vector Conversion in WhiteboxGAT ####
####################################################################################

#Dissolve Polygon Features in Watersheds Shapefiles to One Single Polygon Feature.
setwd("~/sfuvault/Simon_Fraser_University/PhD_Research/Projects/Data/Original_Data/GIS_Data/BC_Climate_Data/FraserWtshdAnalysis/watersheds")
files = dir(pattern = "*.shp") %>% strsplit(x = ., split = "[.]") %>% lapply(.,function(x) x[1]) %>% unlist()
for(i in files){
	dat = readShapePoly(fn = i, IDvar = "FID", proj4string = crs("+init=epsg:3005"))
	if(length(dat@polygons)!=1){
		dat = gUnaryUnion(spgeom = dat, id = "1")
		dat = SpatialPolygonsDataFrame(Sr = dat, data = data.frame(FID = 1))
	}
	#browser()
	dat@data = dat@data %>% bind_cols(., data.frame(SITE = i)) %>% select(SITE)
	writePolyShape(x = dat, fn = paste("../watersheds/",i,sep = ""))
}

#Combine all the watershed delineations into a single shapefile. 
setwd("~/sfuvault/Simon_Fraser_University/PhD_Research/Projects/Data/Original_Data/GIS_Data/BC_Climate_Data/FraserWtshdAnalysis/watersheds")
sites = dir(pattern = "*.shp") %>% 
	lapply(., function(x){
	readShapePoly(fn = x, IDvar = "SITE", proj4string = crs("+init=epsg:3005"))
})
#Combine
df = sites[[1]]
for(i in 2:length(sites)){
	df = rbind.SpatialPolygons(df, sites[[i]])
}
df = as(df, "SpatialPolygonsDataFrame")
writePolyShape(x = df, fn = "../watersheds")

