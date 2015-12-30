#Read dateset as downloaded from http://www.andresmh.com/nyctaxitrips/
#corresponding to the month from the folder "./data/zip_files"
read.NYCData <- function(month = 1, num_rows = -1){
    address <- "./data/zip_files/"
    
    data_csv <- paste("trip_data_", month, ".csv", sep = "")
    fare_csv <- paste("trip_fare_", month, ".csv", sep = "")
    data_zip <- paste(address, data_csv, ".zip", sep = "")
    fare_zip <- paste(address, fare_csv, ".zip", sep = "")
    data <- read.csv(unz(data_zip, data_csv), nrows = num_rows, stringsAsFactors = FALSE, row.names = NULL)
    fare <- read.csv(unz(fare_zip, fare_csv), nrows = num_rows, stringsAsFactors = FALSE, row.names = NULL)
    assign(paste("trip_data_", month, sep = ""), data, envir = .GlobalEnv)
    assign(paste("trip_fare_", month, sep = ""), fare, envir = .GlobalEnv)
}










