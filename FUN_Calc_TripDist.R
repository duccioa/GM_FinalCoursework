calc.TripDist <- function(dt, license_num){
    require(data.table)
    dt_byL <- dt[license == license_num, .(license, p_time)]
    n <- nrow(dt_byL)
    if(n > 100){#exclude drivers with less than 100 trips in a month
        TripDist <- NULL
        for(i in 1:(n-1)){
            TripDist <- append(TripDist, 
                               difftime(dt_byL$p_time[i+1], dt_byL$p_time[i], units = "mins"),#Interval in seconds
                               after = length(TripDist))
        }
        TripDist <- append(TripDist, NA, after = length(TripDist))
    }
    else{TripDist <- rep(NA, n)}
    return(TripDist)
}