library(doParallel)
library(ggplot2)
library(maptools)
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(data.table)
if(!exists("prepare.DATA", mode="function")) source("./00_GM_code/FUN_Prepare_Data.R")
if(!exists("calc.Interval", mode="function")) source("./00_GM_code/FUN_Calc_Interval.R")
if(!exists("read.NYCData", mode="function")) source("./00_GM_code/FUN_Read_Data.R")
if(!exists("iter.WorkingDay", mode="function")) source("./00_GM_code/FUN_Iter_WorkingDay.R")
if(!exists("calc.TripTime", mode="function")) source("./00_GM_code/FUN_Calc_TripTime.R")
if(!exists("create.bbox", mode="function")) source("./00_GM_code/FUN_Create_Bbox.R")
if(!exists("cut.taxis", mode="function")) source("./00_GM_code/FUN_Calc_TripTime.R")
if(!exists("raster.grid", mode="function")) source("./00_GM_code/FUN_Raster_grid.R")

registerDoParallel(cores = 4)#set number of cores
##LOAD THE DATA
#Pick the month i = month and the number of lines
#read the data and prepare the data.table 
month <- 4
foreach(i = month) %dopar% prepare.DATA(read.NYCData(i, -1))
save(paste("taxis_", month, sep = ""), file = "./data/taxis_4.RData")
load(paste("./data/taxis_", month, ".RData"))
taxis <- taxis_4; rm(taxis_4)
### For computing resource reason the analysis is carried out on a random sample 
### of the total dataset
#select random drivers 
Licenses <- unique(taxis$license)
n_Licenses <- length(Licenses)
perc <- 10 #Size in percent of the random sample
selection <- sample(Licenses, round(n_Licenses/(100/perc)))#create the random sample
taxis <- taxis[license %in% selection]#extract data of the selected drivers only
#Calculate interval between drop-off time and next pick-up time
#which corresponds to idle time
taxis[, Idle_mins := foreach(i = selection, .combine = "c") 
      %dopar% calc.Interval(taxis, i)]
taxis <- taxis[!is.na(taxis$Idle_mins) & Idle_mins > 0, ]#Remove negatives and zeros
#Calculate the difference between drop-off time and pick-up time
#which corresponds to the trip time, being the one in the dataset unreliable 
taxis[, TripTime_mins := foreach(i = selection, .combine = "c") 
      %dopar% calc.TripTime(taxis, i)]
taxis <- taxis[!is.na(taxis$TripTime_mins) & TripTime_mins > 0, ]#Remove NAs and negative values
taxis <- taxis[TripTime_mins > quantile(taxis$TripTime_mins, 0.01) & TripTime_mins < quantile(taxis$TripTime_mins, .95),]
save(taxis, file = paste("./data/taxis_", month, "_", perc, ".RData", sep = ""))
#Calculate time between two consecutive pick-ups
taxis[,partial_TOT := Idle_mins + TripTime_mins]

#Differentiate between idle time during working hours and breaks 
#(e.g. lunch time or night time)
#Stretches between pick-ups longer than three hours are considered to be breaks 
#from the working time
#Assign to these breaks the time length value of 0 so that they are not counted 
#in the calculation of the total time worked

taxis[Idle_mins > 60, partial_TOT := 0]
summary(taxis$partial_TOT)
nrow(taxis[partial_TOT == 0])

#########################################################################
################## DATA ANALYSIS - part 1 ###############################
#########################################################################
###By location
#New York's extent
min.x = -74.03027; max.x = -73.77487; min.y = 40.63720; max.y = 40.87865;
nyc_grid <- crop(nyc_map, create.bbox(list(c(min.x,max.x), c(min.y,max.y))))

taxis[,p_long_CAT := cut(p_long, breaks = 3)]; levels(taxis$p_long_CAT) <- c("E","C","W")
taxis[,p_lat_CAT := cut(p_lat, breaks = 3)]; levels(taxis$p_lat_CAT) <- c("S","C","N")
taxis[,loc_CAT := as.factor(paste(p_lat_CAT, p_long_CAT, sep = ""))]

#########################################################################
################## DATA ANALYSIS - part 2 ###############################
#########################################################################
################## Probability matrix ###################################
###Load the shape file of NYC
nyc_map <- readOGR("./data/NYC_shapefile/", "roads")
#################### RASTER GRID ###############################################
### Prepare the shape file
x1 = -74.042; x2 = -73.942; y1 = 40.7; y2 = 40.76; 
c_list <- create.bbox(list(c(x1,x2), c(y1,y2)))
nyc_c <- crop(nyc_map, c_list)
### Subset the database by hour and day
taxis_cut <- cut.taxis(taxis, 10, 8)

r <- raster.grid(d = 0.002, EXT = nyc_c)
#rg <- as(r, "SpatialGrid")
spdf <- SpatialPointsDataFrame(cbind(taxis_cut$p_long, taxis_cut$p_lat), data.frame(taxis_cut))
proj4string(spdf) <- CRS("+init=epsg:4326")
#proj4string(rg) <- CRS("+init=epsg:4326")
#spdf <- spdf[rg,]
r[] <- 0
tab <- table(cellFromXY(r, spdf))
tab <- tab/sum(tab)
r[as.numeric(names(tab))] <- tab

plot(r, col = heat.colors(10))
plot(nyc_c, add = T)

