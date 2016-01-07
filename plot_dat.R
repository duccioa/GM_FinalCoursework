library(doParallel)
library(ggplot2)
library(maptools)
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(data.table)
source("./00_GM_code/FUN_AlphaCol.R")
source("./00_GM_code/FUN_SampleTaxis.R")
source("./00_GM_code/FUN_Summary2CSV.R")
min.x = -74.03027; max.x = -73.77487; min.y = 40.63720; max.y = 40.87865;

#Plot MAP grid
pdf("./Figures/nyc_grid.pdf")
v1 <- min.x + (abs(min.x-max.x)/3);v2 <- min.x + (2*abs(min.x-max.x)/3)
h1 <- min.y + (abs(min.y-max.y)/3);h2 <- min.y + (2*abs(min.y-max.y)/3)
plot(nyc_grid, col = "gray62", bg = "black", lwd = .001, 
     xlim = c(min.x, max.x), ylim = c(min.y, max.y),
     main = "Pick-up location grid")
abline(v = c(min.x, v1, v2, max.x), col = "white", lwd = .1)
abline(h = c(min.y, h1, h2, max.y), col = "white", lwd = .1)
lab <- as.character(levels(taxis$loc_CAT))
lab_loc <- cbind(c(min.x+3*(v2-v1)/2, min.x+5*(v2-v1)/2, min.x+(v2-v1)/2,
                   min.x+3*(v2-v1)/2, min.x+5*(v2-v1)/2, min.x+(v2-v1)/2,
                   min.x+3*(v2-v1)/2, min.x+5*(v2-v1)/2, min.x+(v2-v1)/2),
                 c(min.y+3*(h2-h1)/2, min.y+3*(h2-h1)/2, min.y+3*(h2-h1)/2,
                   min.y+5*(max.y-h2)/2, min.y+5*(max.y-h2)/2, min.y+5*(max.y-h2)/2,
                   min.y+(h1-min.y)/2, min.y+(h1-min.y)/2, min.y+(h1-min.y)/2))
text(labels = lab, x = lab_loc[,1], y = lab_loc[,2], cex = 1, col = "white")
dev.off()
     
#Plot MAP pick-ups Friday night
taxis_sampled <- sample.taxis(taxis, 10)
taxis_sampled <- taxis_sampled[wday == 5 & DayTime == "NIGHT"]
pdf("./Figures/nyc_p_friday_NIGHT.pdf")
plot(nyc_grid, col = "gray62", bg = "black", lwd = .001, 
     xlim = c(min.x, max.x), ylim = c(min.y, max.y),
     main = "Pick-ups on a Friday night")
abline(v = c(min.x, v1, v2, max.x), col = "white", lwd = .1)
abline(h = c(min.y, h1, h2, max.y), col = "white", lwd = .1)
points(taxis_sampled$p_long, taxis_sampled$p_lat, pch = 20, cex = .15, col = add.alpha("yellow", .3))
rm(taxis_sampled)
dev.off()
#Plot MAP pick-ups Friday day
taxis_sampled <- sample.taxis(taxis, 10)
taxis_sampled <- taxis_sampled[wday == 5 & DayTime == "DAY"]
pdf("./Figures/nyc_p_friday_DAY.pdf")
plot(nyc_grid, col = "gray62", bg = "black", lwd = .001, 
     xlim = c(min.x, max.x), ylim = c(min.y, max.y),
     main = "Pick-ups on a Friday at daytime")
abline(v = c(min.x, v1, v2, max.x), col = "white", lwd = .1)
abline(h = c(min.y, h1, h2, max.y), col = "white", lwd = .1)
points(taxis_sampled$p_long, taxis_sampled$p_lat, pch = 20, cex = .15, col = add.alpha("yellow", .3))
rm(taxis_sampled)
dev.off()

#How many hours per day on average?
aggr_table <- taxis[,.(HrsDay = sum(partial_TOT)), by = .(license, DayDate)]
aggr_table <- aggr_table[,.(AvgHrsDay = mean(HrsDay)/60), by = .(license)]
pdf("./Figures/AvgWorkingDay_Hrs.pdf")
par(lwd = .1)
hist(aggr_table$AvgHrsDay, breaks = 24, xlim = c(0,24), col = "burlywood", lwd = 0.1,
     main = "Average working day length", xlab = "Hours per day")
abline(v = median(aggr_table$AvgHrsDay), col = "red")
text(x = 20, y = 400, 
     labels = paste("Median:", round(median(aggr_table$AvgHrsDay),1), "hrs"), 
     col = "black", cex = 0.8)
dev.off()
#What is the median hourly fare?
aggr_table <- taxis[,.(HourlyFare = sum(fare_amount)*60/sum(partial_TOT)), by = .(license)]
aggr_table <- aggr_table[HourlyFare != Inf & HourlyFare < quantile(aggr_table$HourlyFare, 0.995)]
summary(aggr_table$HourlyFare)
pdf("./Figures/HourlyFare_dist.pdf")
par(lwd = .1)
hist(aggr_table$HourlyFare, breaks = 40, col = "goldenrod3", 
     main = "Hourly fare distribution by license", xlab = "$/hr", xlim = c(0,80), lwd = .1)
abline(v = median(aggr_table$HourlyFare), col = "red")
text(x = 50, y = 700, 
     labels = paste("Median hourly fare:", round(median(aggr_table$HourlyFare),2), "$/hr"), 
     col = "black", cex = 0.8)
dev.off()

####### LINEAR REGRESSIONS #########################
###FIT00
aggr_table <- taxis[,.(HourlyFare = sum(fare_amount)*60/sum(partial_TOT)), by = .(license, loc_CAT, DayTime, weekday)]
aggr_table <- aggr_table[HourlyFare < Inf & HourlyFare < 200]
fit00 <- lm(formula = HourlyFare ~ loc_CAT * weekday + loc_CAT * DayTime, 
            data = aggr_table)
summary.2.csv(fit00, "00")

###FIT01 by location
aggr_table <- taxis[,.(HourlyFare = sum(fare_amount)*60/sum(partial_TOT)), by = .(license, loc_CAT)]
aggr_table <- aggr_table[HourlyFare < Inf & HourlyFare < quantile(aggr_table$HourlyFare, 0.99)]
fit01 <- lm(HourlyFare ~ loc_CAT, aggr_table)
summary.2.csv(fit01, "01")
pdf("./Figures/HourlyFare_by_Location.pdf")
par(lwd = .1)
boxplot(HourlyFare ~ loc_CAT, data = aggr_table,
        names = lab, main = "Hourly Fare by location", ylim = c(0,250),
        pch = 20,cex = .1,
        col = "azure1")
dev.off()

###FIT02 by WEEKEND/WEEKDAY
aggr_table <- taxis[,.(HourlyFare = sum(fare_amount)*60/sum(partial_TOT)), by = .(license, weekday)]
aggr_table <- aggr_table[HourlyFare < Inf & HourlyFare < quantile(aggr_table$HourlyFare, 0.99)]
fit02 <- lm(HourlyFare ~ weekday, aggr_table)
summary(fit02)
summary.2.csv(fit02, "02")

pdf("./Figures/HourlyFare_by_Weekday.pdf")
par(lwd = .1)
boxplot(HourlyFare ~ weekday, data = aggr_table,
        names = c("Weekday", "Weekend"), main = "Hourly Fare by WEEKDAY/WEEKEND",
        pch = 20,cex = .1,
        col = "azure3")
dev.off()

###FIT03 by DayTime
aggr_table <- taxis[,.(HourlyFare = sum(fare_amount)*60/sum(partial_TOT)), by = .(license, DayTime)]
aggr_table <- aggr_table[HourlyFare < Inf & HourlyFare < quantile(aggr_table$HourlyFare, 0.99)]
fit03 <- lm(HourlyFare ~ DayTime, aggr_table)
summary.2.csv(fit03, "03")

pdf("./Figures/HourlyFare_by_DayTime.pdf")
par(lwd = .1)
boxplot(HourlyFare ~ DayTime, data = aggr_table, 
        names = c("Day", "Night"),
        pch = 20,cex = .1,
        col = "azure4", main = "Hourly Fare by Time of the Day")
dev.off()



###FIT04 by the day of the week
aggr_table <- taxis[,.(HourlyFare = sum(fare_amount)*60/sum(partial_TOT)), by = .(license, wday)]
aggr_table <- aggr_table[ HourlyFare < Inf]
                             
fit04 <- lm(HourlyFare ~ wday, aggr_table)
summary.2.csv(fit04, "04")
pdf("./Figures/HourlyFare_by_Day.pdf")
par(lwd = .1)
boxplot(HourlyFare ~ wday, data = aggr_table, 
        names = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), ylim = c(0,60), 
        pch = 20,cex = .1,
        col = "cadetblue")
dev.off()


