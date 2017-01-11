library(readr); library(tidyverse);library(nlme);library(multidplyr)
setwd("~/sfuvault/Simon_Fraser_University/PhD_Research/Projects/Data/Original_Data/GIS_Data/BC Climate Data/Fraser_Annual_1970_2007")

#Read in all the data and compile into a single dataframe.
df <- dir() %>% 
	lapply(read_csv) %>% 
	bind_rows

#Select only variables of interest.
df = df %>% select(Year, ID1, Latitude, Longitude, Elevation, MAT, MAP,PAS, EMT, EXT)
names(df) = tolower(names(df))

#Calcuate a climate trend at each location for each climate variable. Retain only the slope coefficients.
cluster = create_cluster(8) #Initialize cores to be used.
df = df %>% partition(id1, cluster = cluster) #Partition the data across nodes.
df %>% cluster_library("nlme") #Load necessary libraries across cores.
df %>% cluster_library("dplyr") #Load necessary libraries across cores.

#Perform Analysis
result = df %>% group_by(id1) %>% do({
	mod.mat = gls(mat~year, data = ., correlation = corAR1())
	mod.map = gls(map~year, data = ., correlation = corAR1())
	mod.pas = gls(pas~year, data = ., correlation = corAR1())
	mod.emt = gls(pas~year, data = ., correlation = corAR1())
	mod.ext = gls(pas~year, data = ., correlation = corAR1())
	
	data_frame(matS = coef(mod.mat)[[2]], mapS = coef(mod.map)[[2]], pasS = coef(mod.pas)[[2]],
						 emtS = coef(mod.emt)[[2]], extS = coef(mod.ext)[[2]])
})
result = collect(result) #Gather results from cores.

#Add back in lat/long values.

result = df %>% select(id1, latitude, longitude) %>% left_join(., result, by = "id1")

#For QGIS viewing.
#result = read.csv(file = "~/Desktop/climate_slopes.csv")
#result$matS =  result$matS / 10^7
#result$mapS =  result$mapS / 10^7
#result$pasS =  result$pasS / 10^7
#result$emtS =  result$emtS / 10^7
#result$extS =  result$extS / 10^7
write.csv(x = result, file = "~/Desktop/climate_slopes.csv", row.names = F)

# Sample of sites for testing purposes.
temp = df %>% filter(id1 == 1) %>% mutate(yr.sc = scale(year, center = T, scale = F))
ggplot(temp, aes(year, mat)) +
	geom_smooth(method = "lm") +
	geom_point()
mod = gls(model = mat~year, data = temp, correlation = corAR1())
plot(mod, resid(., type = "normalized") ~ fitted(.) | id1, abline = 0)
plot(ACF(mod, resType = "normalized"))


###### Site Watershed Delineation ######
#Load Libraries
library(RSAGA); library(tidyverse); library(raster); library(maptools); library(rgeos)

###### Pre-Processing #####

#Set SAGA working environment. Important because I'm pulling from the QGIS folders rather than a traditional SAGA installation.
myenv = rsaga.env(workspace = "/Users/kylechezik/sfuvault/Simon_Fraser_University/PhD_Research/Projects/Data/Original_Data/GIS_Data/BC_DEMs/FraserDEM", path = "/Applications/QGIS.app/Contents/MacOS/bin", modules = "/Applications/QGIS.app/Contents/MacOS/lib/saga", version = '2.1.4', parallel = T)

#Load Site Data
setwd("~/sfuvault/Simon_Fraser_University/PhD_Research/Projects/River-Network-Flow-Trends")
sites = read.csv("05_Sites.csv")
#Isolate one sites.
site = sites[1,c(3,1,2)]
# turn into a spatial object
coordinates(site) = ~ X + Y
projection(site) = CRS("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs")

# read in the catchment area grid
catch_area = raster('/Users/kylechezik/sfuvault/Simon_Fraser_University/PhD_Research/Projects/Data/Original_Data/GIS_Data/BC_DEMs/FraserDEM/catchArea.sdat')
# exract a window around the gauge point. Here we get the maximum value within 500m of th gauge.
buffer = extract(catch_area, site, buffer = 500, cellnumber = T)[[1]] %>% as.data.frame()
# here we isolate the raster cell with the largest watershed area contibution.
snap_loc = buffer$cell[which.max(buffer$value)]
# here we get the xy coordinate of the nearest maximum watershed value to our gauge site.
snap_loc = xyFromCell(catch_area, snap_loc)

# read in the catchment area grid
rsaga.geoprocessor(lib = 'ta_hydrology', 4, env = myenv,
									 param = list(TARGET_PT_X = snap_loc[1,1],
									 						 TARGET_PT_Y = snap_loc[1,2],
									 						 ELEVATION = "/Users/kylechezik/sfuvault/Simon_Fraser_University/PhD_Research/Projects/Data/Original_Data/GIS_Data/BC_DEMs/FraserDEM/demFill/demFill.sgrd",
									 						 AREA = "/Users/kylechezik/sfuvault/Simon_Fraser_University/PhD_Research/Projects/Data/Original_Data/GIS_Data/BC_DEMs/FraserDEM/demFill/wtshd.sgrd",
									 						 METHOD = 2))

rsaga.geoprocessor(lib = 'shapes_grid', 6, env = myenv,
									 param = list(GRID = "/Users/kylechezik/sfuvault/Simon_Fraser_University/PhD_Research/Projects/Data/Original_Data/GIS_Data/BC_DEMs/FraserDEM/demFill/wtshd.sgrd",
									 						 POLYGONS = "/Users/kylechezik/sfuvault/Simon_Fraser_University/PhD_Research/Projects/Data/Original_Data/GIS_Data/BC_DEMs/FraserDEM/demFill/wtshd.shp",
									 						 CLASS_ALL = 0,
									 						 CLASS_ID = 100,
									 						 SPLIT = 0))

# read in the shapefile, I explicitly noted the projection.  This may not be necessary.
basin <- readShapeSpatial("/Users/kylechezik/sfuvault/Simon_Fraser_University/PhD_Research/Projects/Data/Original_Data/GIS_Data/BC_DEMs/FraserDEM/demFill/wtshd.shp", proj4string = CRS("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs"))
fill_dem <- raster('/Users/kylechezik/sfuvault/Simon_Fraser_University/PhD_Research/Projects/Data/Original_Data/GIS_Data/BC_DEMs/FraserDEM/demFill/demFill.sdat')

#Plot
plot(fill_dem)
plot(site, add = T)
coordinates(site) = ~ X + Y
projection(site) = CRS("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs")
plot(basin, add = T) 










##### Run All Sites #####
sites %>% group_by(Station.ID) %>% do({
	nm = as.character(.$Station.ID)
	coordinates(.) = ~ Long + Lat
	projection(.) = CRS("+init=epsg:4326")
	sites <- spTransform(., CRS("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs"))
	buffer <- extract(catch_area, ., buffer = 500, cellnumbers = T)[[1]] %>%
		as.data.frame
	snap_loc <- buffer$cell[which.max(buffer$value)]
	snap_loc <- xyFromCell(catch_area, snap_loc)
	
	rsaga.geoprocessor(lib = 'ta_hydrology', 4,
										 param = list(TARGET_PT_X = snap_loc[1,1],
										 						 TARGET_PT_Y = snap_loc[1,2],
										 						 ELEVATION = '/Users/kylechezik/sfuvault/Simon_Fraser_University/PhD_Research/Projects/Data/Original_Data/GIS_Data/BC_DEMs/FraserDEM/demFill.sgrd',
										 						 AREA = '/Users/kylechezik/sfuvault/Simon_Fraser_University/PhD_Research/Projects/Data/Original_Data/GIS_Data/BC_DEMs/FraserDEM/wtshd.sgrd',
										 						 METHOD = 0))
	
	rsaga.geoprocessor(lib = 'shapes_grid', 6,
										 param = list(GRID = '/Users/kylechezik/sfuvault/Simon_Fraser_University/PhD_Research/Projects/Data/Original_Data/GIS_Data/BC_DEMs/FraserDEM/wtshd.sgrd',
										 						 POLYGONS = paste(nm,'_boundary.shp',sep = ""),
										 						 CLASS_ALL = 0,
										 						 CLASS_ID = 100,
										 						 SPLIT = 0))
})
