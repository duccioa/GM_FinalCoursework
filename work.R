library(doParallel)
registerDoParallel(cores = 6)
foreach(i = 8) %dopar% prepare.DATA(read.NYCData(i, 100000))
freq.byLicense <- function(dt){
    by_license <- dt[,.(Monthly_Income = sum(total_amount)), by = license]
    med <- median(by_license$Monthly_Income)
    hist(by_license$Monthly_Income, breaks = 40, col = "darkseagreen", 
         main = "Monthly income by license", xlab = "$", ylab = "Freq")
    abline(v = med, col ="red", lwd = 2)
    text(x = med-med*0.1, y = 1500, labels = paste("Median Income:", round(med)),srt = 90, col = "black")
}