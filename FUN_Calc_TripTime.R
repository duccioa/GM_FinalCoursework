calc.TripTime <- function(dt, license_num){
    require(data.table)
    dt_byL <- dt[license == license_num, .(license, p_time)]
    n <- nrow(dt_byL)
    if(n > 100){#exclude drivers with less than 100 trips in a month
        TripTime <- NULL
        for(i in 1:(n-1)){
            TripTime <- append(TripTime, 
                               difftime(dt_byL$p_time[i+1], dt_byL$p_time[i], units = "mins"),#Interval in seconds
                               after = length(TripTime))
        }
        TripTime <- append(TripTime, NA, after = length(TripTime))
    }
    else{TripTime <- rep(NA, n)}
    return(TripTime)
}