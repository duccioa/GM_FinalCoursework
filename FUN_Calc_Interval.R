calc.Interval <- function(dt, license_num){
    setkey(dt, license)
    dt_byL <- dt[license == license_num, .(license,p_time, d_time)]
    n <- nrow(dt_byL)
    if(n > 100){#exclude drivers with less than 100 trips in a month
    Interval <- dt_byL[2:n, p_time] - dt_byL[1:(n-1), d_time]
    Interval <- append(Interval, NA, after = length(Interval))
    }
    else{Interval <- rep(NA, n)}
    return(Interval)
}

iter.calc.Int <- function(dt){
    licenses <- unique(dt$license)
    Interval <- NULL
    for(i in licenses){
        v <- calc.Interval(dt, i)
        Interval <- append(Interval, v, after = length(v))
    }
    return(Interval)
}