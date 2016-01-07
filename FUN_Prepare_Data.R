#Takes as an input a list with three elements: one data.table with trips, one dt with fares
#and the number of the month
prepare.DATA <- function(dt_list){
    require(data.table)
    require(lubridate)
    trip_data <- dt_list[[1]]
    trip_fare <- dt_list[[2]]
    month <- dt_list[[3]]
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
    taxis <- taxis[,.(X.hack_license, X.pickup_datetime, X.dropoff_datetime, 
                      X.trip_distance, X.pickup_longitude, X.pickup_latitude, X.dropoff_longitude,
                      X.dropoff_latitude, X.fare_amount, X.surcharge, X.mta_tax, X.tip_amount, X.tolls_amount, 
                      X.total_amount)]#select relevant columns
    names_taxis <- c("license", "p_time", "d_time",  
                     "trip_dist", "p_long", "p_lat", "d_long",
                     "d_lat", "fare_amount", "surcharge", "mta_tax",
                     "tip_amount", "tolls_amount", "total_amount")
    setnames(taxis, names_taxis)
    ####Remove NAs
    print("Remove NAs")
    n<- nrow(taxis)
    taxis <- taxis[complete.cases(taxis), ]
    print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
    ####Remove outliers and wrong values
    print("Crop the data")
    #Remove outliers in p_long
    min.x = -74.03027; max.x = -73.77487; min.y = 40.63720; max.y = 40.87865;
    taxis <- taxis[p_long > min.x & p_long < max.x & 
                       p_lat > min.y & p_lat < max.y]#remove entries outside New York
    taxis <- taxis[d_long > min.x & d_long < max.x & 
                       d_lat > min.y & d_lat < max.y]#remove entries outside New York
    #Remove outliers in d_long
    print("d_lat")
    n<- nrow(taxis)
    max_val <- max.x
    min_val <- min.x
    taxis <- taxis[d_long >= min_val & d_long <= max_val]
    print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
    #Remove outliers in d_lat
    print("d_lat")
    n<- nrow(taxis)
    max_val <- max.y
    min_val <- min.y
    taxis <- taxis[d_lat >= min_val & d_lat <= max_val]
    print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
    ##Data manipulation
    print("DATA MANIPULATION AND EXTRA COLUMNS")
    print("Factor license")
    taxis$license <- as.factor(taxis$license)
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
    taxis$weekday <- "WEEKDAY"
    taxis$weekday[taxis$wday == 6 | taxis$wday == 7] <- "WEEKEND"
    taxis$weekday <- as.factor(taxis$weekday)
    print("DayTime")
    taxis$DayTime <- "DAY"
    taxis$DayTime[hour(taxis$p_time) >= 20 | hour(taxis$p_time) < 6] <- "NIGHT"
    taxis$DayTime <- as.factor(taxis$DayTime)
    print("DayDate")
    taxis[, DayDate := as.factor(day(p_time))]
   
    
    setorder(taxis, license, p_time)
    assign(paste("taxis", month, sep = "_"), taxis, envir = .GlobalEnv)
}

