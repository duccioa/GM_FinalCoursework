library(lubridate)
library(dplyr)
library(ggplot2)
library(plyr)


read.NYCData(10, 100000)
data_names <- names(trip_data_10)
fare_names <- names(trip_fare_10)
trip_data_10 <- trip_data_10[,c(2,6,7,9,10,11,12,13,14)]
trip_fare_10 <- trip_fare_10[,c(2,4,6,7,8,9,10,11)]
#taxis10 <- merge(trip_data_10, trip_fare_10, by = ("hack_license"), sort = FALSE)
#taxis10 <- join(trip_data_10, trip_fare_10, type = "inner")
taxis10 <- join(trip_data_10, trip_fare_10)
names(taxis10) <- c("license", "p_time", "d_time", "trip_time", 
                    "trip_dist", "p_long", "p_lat", "d_long",
                    "d_lat", "fare_amount", "surcharge", "mta_tax",
                    "tip_amount", "tolls_amount", "total_amount")

                     

nyc_taxis <- read.csv("./Archive/NYC_Taxi_Data/nyc_taxi_data.csv.gz", nrows = 1000000)
head(nyc_taxis)
col.names <- data.frame(names(nyc_taxis))
str(nyc_taxis)

trips <- read.csv("./data/trip_data_1.csv", stringsAsFactors = FALSE)
fares <- read.csv(".data/trip_fare_1.csv", stringsAsFactors = FALSE)
trip_names <- data.frame(names(trips))
trips$pickup_datetime <- ymd_hms(trips$pickup_datetime)
trips$dropoff_datetime <- ymd_hms(trips$dropoff_datetime)
trips$medallion <- as.factor(trips$medallion)
trips$hack_license <- as.factor(trips$hack_license)
trips$vendor_id <- as.factor(trips$hack_license)
distance <- trips[,c(1,2,9,10)]
smr_dist <- summarise(group_by(distance, hack_license), avg_dist = mean(trip_distance))
smr_dist <- smr_dist[smr_dist$avg_dist < quantile(smr_dist$avg_dist, 0.99),]
g <- ggplot(smr_dist, aes(x = avg_dist, y = ..density..))
g <- g + geom_histogram(colour = "black", fill = "goldenrod1", binwidth = .1)
g + geom_density(colour = "blue", size = 1, adjust = 4)
