## Given dataframe or datatable taxis, return the subset by DAY and HOUR of the day
cut.taxis <- function(df,DAY, HOUR){
    require(lubridate)
    return(df[hour(df$p_time) == HOUR & day(df$p_time) == DAY, ])
}
#####