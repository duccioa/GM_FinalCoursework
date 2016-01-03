calc.Interval <- function(dt, license_num){
    require(data.table)
    dt_byL <- dt[license == license_num, .(license,p_time, d_time)]
    n <- nrow(dt_byL)
    if(n > 100){#exclude drivers with less than 100 trips in a month
        Interval <- NULL
        for(i in 1:(n-1)){
            Interval <- append(Interval, 
                               difftime(dt_byL$p_time[i+1], dt_byL$d_time[i], units = "mins"),#Interval in seconds
                               after = length(Interval))
        }
        Interval <- append(Interval, NA, after = length(Interval))
    }
    else{Interval <- rep(NA, n)}
    return(Interval)
}
