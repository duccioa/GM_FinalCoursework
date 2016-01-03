calc.Interval <- function(dt){
    require(data.table)
    new_dt <- dt[0,]
    new_dt$Interval <- 0
    dt <- setorder(dt, license, p_time)
    Licenses <- unique(dt$license)
    for(i in Licenses){
        dt_byL <- dt[license == i]
        n <- nrow(dt_byL)
        d_time <- dt_byL[1:(n-1), d_time]
        next_p_time <- dt_byL[2:n, p_time]
        Interval <- next_p_time - d_time
        dt_byL <- dt_byL[-n,]
        dt_byL$Interval <- as.numeric(Interval)
        new_dt <- rbind(new_dt, dt_byL)
    }
    return(new_dt)
}
new_taxis <- calc.Interval(taxis_8)
new_taxis <- foreach(i = taxis_8) %dopar% calc.Interval(taxis_8)
debug(calc.Interval)
