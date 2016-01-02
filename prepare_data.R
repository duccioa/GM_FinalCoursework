prepare.DATA <- function(month = 1){
    require(data.table)
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
    ####Remove outliers and wrong values
    #Remove negative and very short and very long trip times
    print("Remove outliers")
    print("Trip_time")
    n<- nrow(taxis)
    taxis <- taxis[trip_time > 0]
    max_val <- quantile(taxis$trip_time, 0.99)
    min_val <- quantile(taxis$trip_time, 0.01)
    taxis <- taxis[trip_time >= min_val & trip_time <= max_val]
    print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
    #Remove outliers in trip distances
    print("Trip_dist")
    n<- nrow(taxis)
    max_val <- quantile(taxis$trip_dist, 0.999)
    min_val <- quantile(taxis$trip_dist, 0.01)
    taxis <- taxis[trip_dist >= min_val & trip_dist <= max_val]
    print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
    #Remove outliers in fare amount
    print("Fare_amount")
    n<- nrow(taxis)
    taxis <- taxis[fare_amount > 0]
    max_val <- quantile(taxis$fare_amount, 0.999)
    min_val <- quantile(taxis$fare_amount, 0.01)
    taxis <- taxis[fare_amount >= min_val & fare_amount <= max_val]
    print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
    #Remove outliers in surcharge
    print("Surcharge")
    n<- nrow(taxis)
    taxis <- taxis[surcharge >= 0]
    max_val <- quantile(taxis$surcharge, 0.999)
    min_val <- quantile(taxis$surcharge, 0.01)
    taxis <- taxis[surcharge >= min_val & surcharge <= max_val]
    print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
    #Remove outliers in mta_tax
    print("Mta_tax")
    n<- nrow(taxis)
    taxis <- taxis[mta_tax >= 0]
    max_val <- quantile(taxis$mta_tax, 0.999)
    min_val <- quantile(taxis$mta_tax, 0.01)
    taxis <- taxis[mta_tax >= min_val & mta_tax <= max_val]
    print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
    #Remove outliers in tip_amount
    print("Tip_amount")
    n<- nrow(taxis)
    taxis <- taxis[tip_amount >= 0]
    max_val <- quantile(taxis$tip_amount, 0.999)
    min_val <- quantile(taxis$tip_amount, 0.01)
    taxis <- taxis[tip_amount >= min_val & tip_amount <= max_val]
    print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
    #Remove outliers in tolls_amount
    print("Tolls_amount")
    n<- nrow(taxis)
    taxis <- taxis[tolls_amount >= 0]
    max_val <- quantile(taxis$tolls_amount, 0.999)
    min_val <- quantile(taxis$tolls_amount, 0.01)
    taxis <- taxis[tolls_amount >= min_val & tolls_amount <= max_val]
    print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
    #Remove outliers in total_amount
    print("Total_amount")
    n<- nrow(taxis)
    taxis <- taxis[total_amount >= 0]
    max_val <- quantile(taxis$total_amount, 0.999)
    min_val <- quantile(taxis$total_amount, 0.01)
    taxis <- taxis[total_amount >= min_val & total_amount <= max_val]
    print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
    
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
    
    assign(paste("taxis", month, sep = "_"), taxis, envir = .GlobalEnv)
}

