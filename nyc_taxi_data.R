library(lubridate)
library(dplyr)
library(ggplot2)
read.NYCData <- function(month = 1, nrow = 100000){
    address <- "./data/zip_files/"
    
    file_data <- paste("trip_data_", month, ".csv.gz", sep = "")
    file_fare <- paste("trip_fare_", month, ".csv.gz", sep = "")
    data <- read.table(paste(address, file_data, sep = ""), nrows = nrow, stringsAsFactors = FALSE)
    #fare <- read.table(paste(address,file_fare, sep = ""), nrows = nrow, stringsAsFactors = FALSE)
    assign(paste("trip_data_", month, sep = ""), data, envir = .GlobalEnv)
    #assign(paste("trip_fare_", month, sep = ""), fare, envir = .GlobalEnv)
}
system.time(read.table("./data/zip_files/trip_data_1.csv.gz", nrows = 5000000))
system.time(read.csv(unz("./data/zip_files/trip_data_1.csv.zip", "trip_data_1.csv"), nrows = 5000000))
system.time(read.csv("./data/zip_files/trip_data_1.csv", nrows = 5000000))








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


