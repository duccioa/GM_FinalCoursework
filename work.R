library(doParallel)
if(!exists("prepare.DATA", mode="function")) source("./00_GM_code/FUN_Prepare_Data.R")
if(!exists("calc.Interval", mode="function")) source("./00_GM_code/FUN_Calc_Interval.R")
if(!exists("read.NYCData", mode="function")) source("./00_GM_code/FUN_Read_Data.R")
if(!exists("calc.pInt", mode="function")) source("./00_GM_code/FUN_Calc_pInt.R")
registerDoParallel(cores = 7)#set number of cores
foreach(i = 4) %dopar% prepare.DATA(read.NYCData(i, -1))#read the data and prepare the dt
#select random drivers
setkey(taxis, license, p_time)
Licenses <- unique(taxis$license)
n_Licenses <- length(Licenses)
perc <- 5 #percentage of random samples
selection <- sample(Licenses, round(n_Licenses/(100/perc)))
taxis <- taxis_4[license %in% selection]#extract data of the selected drivers only
#Calculate the drop_off-next_pick_up intervals and trip_dist
taxis$Int_mins <- foreach(i = selection, .combine = "c") %dopar% calc.Interval(taxis, i)
taxis$TripDist_mins <- foreach(i = selection, .combine = "c") %dopar% calc.TripDist(taxis, i)
taxis <- taxis[!is.na(taxis$Int_mins) & Int_mins > 0 ]
taxis <- taxis[!is.na(taxis$TripDist_mins) & TripDist_mins > 0 & TripDist_mins < quantile(taxis$TripDist_mins, .95)]
#intervals above 2 hours are considered to be BREAKS, otherwise just idle time between pickups 
taxis$Int_Status <- ifelse(taxis$Int_mins <= 120, c("WORK"), c("BREAK"))

freq.byLicense <- function(dt){
    by_license <- dt[,.(Monthly_Income = sum(total_amount)), by = license]
    med <- median(by_license$Monthly_Income)
    hist(by_license$Monthly_Income, breaks = 40, col = "darkseagreen", 
         main = "Monthly income by license", xlab = "$", ylab = "Freq")
    abline(v = med, col ="red", lwd = 2)
    text(x = med-med*0.1, y = 1500, labels = paste("Median Income:", round(med)),srt = 90, col = "black")
}

