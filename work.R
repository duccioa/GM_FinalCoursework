library(doParallel)
if(!exists("prepare.DATA", mode="function")) source("./00_GM_code/FUN_Prepare_Data.R")
if(!exists("calc.Interval", mode="function")) source("./00_GM_code/FUN_Calc_Interval.R")
if(!exists("read.NYCData", mode="function")) source("./00_GM_code/FUN_Read_Data.R")
if(!exists("iter.WorkingDay", mode="function")) source("./00_GM_code/FUN_Iter_WorkingDay.R")
if(!exists("calc.TripTime", mode="function")) source("./00_GM_code/FUN_Calc_TripTime.R")

registerDoParallel(cores = 7)#set number of cores
##LOAD THE DATA
#Pick the month i = month and the number of lines
foreach(i = 4) %dopar% prepare.DATA(read.NYCData(i, -1))#read the data and prepare the dt
#select random drivers
Licenses <- unique(taxis$license)
n_Licenses <- length(Licenses)
perc <- 5 #Size in percent of the random sample
selection <- sample(Licenses, round(n_Licenses/(100/perc)))#create the random sample
taxis <- taxis_4[license %in% selection]#extract data of the selected drivers only
#Calculate the drop_off-next_pick_up intervals and trip_dist and partial_tot
taxis[, Int_mins := foreach(i = selection, .combine = "c") %dopar% calc.Interval(taxis, i)]
taxis[, TripTime_mins := foreach(i = selection, .combine = "c") %dopar% calc.TripTime(taxis, i)]
#backup
taxis <- taxis[!is.na(taxis$Int_mins) & Int_mins > 0 ]#Remove negatives and zeros
taxis <- taxis[!is.na(taxis$TripTime_mins) & TripTime_mins > 0 & 
                   TripTime_mins < quantile(taxis$TripTime_mins, .95)]#Remove outliers
taxis[,partial_TOT := Int_mins + TripTime_mins]#Calculate time between pick-ups
taxis[partial_TOT > 180, partial_TOT := 0]#Stretches between pick-ups longer than three hours are considered as breaks
taxis[, DayDate := as.factor(day(p_time))]

#########################################################################
###################DATA ANALYSIS#########################################
#########################################################################
###FIT by WEEKEND/WEEKDAY and DAY/NIGHT
by_DateTime_WeekDay <- taxis[,.(HourlyFare = sum(fare_amount)*60/sum(partial_TOT)), by = .(DayTime, weekday)]
by_DateTime_WeekDay <- by_DateTime_WeekDay[HourlyFare != Inf,]
fit1 <- lm(HourlyFare ~ weekday + DayTime, by_DateTime_WeekDay)
summary(fit1)
###FIT by day of the week
by_Wday <- taxis[,.(HourlyFare = sum(fare_amount)*60/sum(partial_TOT)), by = .(wday)]
by_Wday <- by_Wday[HourlyFare != Inf,]
fit2 <- lm(HourlyFare ~ weekday + DayTime, by_Wday)
summary(fit2)

HourlyFare_byLicense <- TimeFare_byDay[,.(AvgHrFare = mean(HourlyFare)), by = .(license, DayTime)]
HourlyFare_byLicense <- HourlyFare_byLicense[AvgHrFare != Inf,]


freq.byLicense <- function(dt){
    by_license <- dt[,.(Monthly_Income = sum(total_amount)), by = license]
    med <- median(by_license$Monthly_Income)
    hist(by_license$Monthly_Income, breaks = 40, col = "darkseagreen", 
         main = "Monthly income by license", xlab = "$", ylab = "Freq")
    abline(v = med, col ="red", lwd = 2)
    text(x = med-med*0.1, y = 1500, labels = paste("Median Income:", round(med)),srt = 90, col = "black")
}

