#Read dateset as downloaded from http://www.andresmh.com/nyctaxitrips/
#corresponding to the month from the folder "./data/zip_files"
#Use num_rows = -1, to read the whole csv
read.NYCData <- function(month = 1, num_rows = -1){
    require(data.table)
    address <- "./data/zip_files/"
    data_csv <- paste("trip_data_", month, ".csv", sep = "")
    fare_csv <- paste("trip_fare_", month, ".csv", sep = "")
    data <- fread(paste(address,data_csv, sep = ""), nrows = num_rows)
    fare <- fread(paste(address,fare_csv, sep = ""), nrows = num_rows)             
    return(list(data, fare, month))
}










