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
    print("REMOVE OUTLIERS AND WRONG ENTRIES")
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
    #Remove outliers in p_long
    print("P_long")
    n<- nrow(taxis)
    max_val <- quantile(taxis$p_long, 0.99)
    min_val <- quantile(taxis$p_long, 0.01)
    taxis <- taxis[p_long >= min_val & p_long <= max_val]
    print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
    #Remove outliers in p_lat
    print("P_lat")
    n<- nrow(taxis)
    max_val <- quantile(taxis$p_lat, 0.99)
    min_val <- quantile(taxis$p_lat, 0.01)
    taxis <- taxis[p_lat >= min_val & p_lat <= max_val]
    print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
    #Remove outliers in d_long
    print("d_lat")
    n<- nrow(taxis)
    max_val <- quantile(taxis$d_long, 0.99)
    min_val <- quantile(taxis$d_long, 0.01)
    taxis <- taxis[d_long >= min_val & d_long <= max_val]
    print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
    #Remove outliers in d_lat
    print("d_lat")
    n<- nrow(taxis)
    max_val <- quantile(taxis$d_lat, 0.99)
    min_val <- quantile(taxis$d_lat, 0.01)
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
    #setcolorder(taxis, c("license", "p_time", "month", "wday", "weekday", "DayTime",
                         #"d_time", "trip_dist", "p_long", "p_lat", "d_long", "d_lat",
                         #"fare_amount", "surcharge", "mta_tax", "tip_amount",
                         #"tolls_amount", "total_amount"))
    
    setorder(taxis, license, p_time)
    assign(paste("taxis", month, sep = "_"), taxis, envir = .GlobalEnv)
}

