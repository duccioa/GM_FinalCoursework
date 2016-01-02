library(lubridate)
library(dplyr)
library(ggplot2)
library(plyr)
library(data.table)
data_names <- names(trip_data_8)
fare_names <- names(trip_fare_8)

read.NYCData(8, 2000000)





prepare.DATA <- function(month = 1){
    require(lubridate)
    trip_data <- get(paste("trip_data_", month, sep = ""))
    trip_fare <- get(paste("trip_fare_", month, sep = ""))
    data_names <- make.names(names(trip_data))#remove illegal characters
    fare_names <- make.names(names(trip_fare))
    setnames(trip_data, data_names)
    setnames(trip_fare, fare_names)
    setkey(trip_data, medallion, X.hack_license, X.vendor_id, X.pickup_datetime)
    setkey(trip_fare, medallion, X.hack_license, X.vendor_id, X.pickup_datetime)
    print("Removing duplicates")
    trip_data <- trip_data[!duplicated(trip_data),]#remove row duplicates
    trip_fare <- trip_fare[!duplicated(trip_fare),]#remove row duplicates
    print("Merging trips and fares")
    taxis <- trip_data[trip_fare]#merge trips and fares
    taxis <- taxis[,.(X.hack_license, X.pickup_datetime, X.dropoff_datetime, X.trip_time_in_secs,
                      X.trip_distance, X.pickup_longitude, X.pickup_latitude, X.dropoff_longitude,
                      X.dropoff_latitude, X.fare_amount, X.surcharge, X.mta_tax, X.tip_amount, X.tolls_amount, 
                      X.total_amount)]#select relevant columns
    names_taxis <- c("license", "p_time", "d_time", "trip_time", 
                     "trip_dist", "p_long", "p_lat", "d_long",
                     "d_lat", "fare_amount", "surcharge", "mta_tax",
                     "tip_amount", "tolls_amount", "total_amount")
    setnames(taxis, names_taxis)
    #Remove outliers and wrong values
    taxis <- taxis[taxis$trip_time > 0]
    max_val <- quantile(taxis$trip_time, 0.99)
    min_val <- quantile(taxis$trip_time, 0.01)
    taxis <- taxis[taxis]
    
    print("Converting dates and times")
    print("p_time")
    taxis$p_time <- ymd_hms(taxis$p_time)
    print("d_time")
    taxis$d_time <- ymd_hms(taxis$d_time)
    print("Factor month")
    taxis$month <- as.factor(month)
    print("Factor wday")
    taxis$wday <- as.factor(wday(taxis$p_time))
    print("Weekday")
    taxis$weekday <- TRUE
    taxis$weekday[taxis$wday == 6 | taxis$wday == 7] <- FALSE
    
    #rm(paste("trip_data_", month, sep = ""), paste("trip_fare_", month, sep = ""))
    assign(paste("taxis", month, sep = "_"), taxis, envir = .GlobalEnv)
}


preliminary_ANALYSIS <- function(month = 1){
    require(dplyr)
    taxis <- get(paste("taxis", month, sep = "_"))
    
}
                     
library(doParallel)
registerDoParallel(cores = 6)
foreach(i = 8) %dopar% read.NYCData(i,-1)
foreach(i = 8) %dopar% prepare.DATA(i)
