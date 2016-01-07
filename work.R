library(doParallel)
library(ggplot2)
library(maptools)
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(data.table)
library(lubridate)
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
####Load the shape file of NYC
if(!(exists("nyc_map")|exists("nyc_grid")))nyc_map <- readOGR("./data/NYC_shapefile/", "roads")
#Create the extent
min.x = -74.03027; max.x = -73.77487; min.y = 40.63720; max.y = 40.87865;
if(!exists("nyc_grid"))nyc_grid <- crop(nyc_map, create.bbox(list(c(min.x,max.x), c(min.y,max.y))))
#Create location categories for long and lat
taxis[,p_long_CAT := cut(p_long, breaks = 3)]; levels(taxis$p_long_CAT) <- c("E","C","W")
taxis[,p_lat_CAT := cut(p_lat, breaks = 3)]; levels(taxis$p_lat_CAT) <- c("S","C","N")
taxis[,loc_CAT := as.factor(paste(p_lat_CAT, p_long_CAT, sep = ""))]

#########################################################################
################## DATA ANALYSIS - part 2 ###############################
#########################################################################
################## Probability matrix ###################################
### Subset the database by hour and day
taxis_cut <- cut.taxis(taxis, 10, 8)

r <- raster.grid(d = 0.002, EXT = nyc_grid)
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
###Extract taxis probability data frame "tp"
h <- 10
t <- 22.5
ht <- as.numeric(paste(h, formatC(t, width = 2, flag = "0"), sep = ""))
tp <- taxis[, .(license, p_time, d_time, p_long, p_lat, d_long, d_lat, DayDate, Idle_mins, TripTime_mins, partial_TOT)]
tp[, Hour := as.factor(hour(p_time))]

tp[, Int_CAT := cut(minute(p_time), breaks = c(0,15,30,45,60), labels = c("07.5","22.5","37.5","52.5"))]
tp[, Int1 := cut(minute(p_time), breaks = c(0,15,30,45,60), labels = c("00","15","30","45"))]
tp[, Int2 := cut(minute(p_time), breaks = c(0,15,30,45,60), labels = c("15","30","45","60"))]
tp$h1 <- as.numeric(paste(tp$Hour, tp$Int_CAT, sep = "")) - 37.5
tp$h2 <- as.numeric(paste(tp$Hour, tp$Int_CAT, sep = "")) + 37.5
tp$Int1 <- as.numeric(paste(tp$Hour, tp$Int1, sep = ""))
tp$Int2 <- as.numeric(paste(tp$Hour, tp$Int2, sep = ""))
tp <- tp[!is.na(Int1) & !is.na(Int2)]

tph <- tp[tp$h1 < ht & tp$h2 > ht]
TOT_drv_t <- length(unique(tph$license))
tph[,PT := as.numeric(paste(hour(p_time), formatC(minute(p_time), width = 2, flag = "0"), sep = ""))]
tph[,DT := as.numeric(paste(hour(d_time), formatC(minute(d_time), width = 2, flag = "0"), sep = ""))]
tph[,FREE := ifelse(PT < Int1 & DT > Int2, FALSE, TRUE)]
tpb <- tph[FREE != TRUE]
TOT_busy_t <- length(unique(tpb$license))

####################
####################
d <- 10#pick the day
Int1 <- 1025#Start of the interval
Int2 <- Int1 + 15
Int_h1 <- Int1 - 37.5
Int_h2 <- Int2 + 37.5
tp <- taxis[DayDate == d, .(license, p_time, d_time, p_long, p_lat, 
                d_long, d_lat, DayDate, 
                Idle_mins = round(Idle_mins), 
                TripTime_mins = round(TripTime_mins), 
                partial_TOT = round(partial_TOT))]
tp[,p_time := as.numeric(paste(hour(p_time), formatC(minute(p_time), width = 2, flag = "0"), sep = ""))]
tp[,d_time := as.numeric(paste(hour(d_time), formatC(minute(d_time), width = 2, flag = "0"), sep = ""))]
tp[,np_time := d_time + Idle_mins]

tph <- tp[(p_time > Int_h1 & p_time < Int_h2) | (d_time > Int_h1 & d_time < Int_h2)]
TOT_drv_t <- length(unique(tph$license))#Total number of drivers during the hour around the interval
tph[, BUSY := ifelse(p_time < Int1 & d_time > Int2, TRUE, FALSE)]
BUSY_drv_t <- length(tph$license[tph$BUSY])
tph[, PICK := ifelse(p_time >= Int1 & p_time <= Int2, TRUE, FALSE)]
tpp <- tph[tph$PICK,]
TOT_pick_t <- length(unique(tpp$license))



