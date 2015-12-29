address <- "./data/zip_files/"
files.zip <- list.files(address, pattern = "\\.zip$")
files.csv <- gsub("[.]zip", "", files.zip)
files <- gsub("[.]csv", "", files.csv)

for(i in 1:length(files)){
file_zip <- files.zip[i]
file_csv <- files.csv[i]
file <- files[i]
table <- read.csv(unz(paste(address, file_zip, sep = ""), file_csv))
write.table(table, paste(address, file_csv, sep = ""))
command <- paste("gzip ", paste(address, file_csv, sep = ""))
system(command)
}

