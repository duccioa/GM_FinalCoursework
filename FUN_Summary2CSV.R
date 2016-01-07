#Save summary fit to csv

summary.2.csv <- function(fit, num = 00){
    summ <- summary(fit)
    sum_fit <- data.frame(summ$coefficients)
    res_fit <- data.frame(Min = summary(summ$residuals)[1],
                            First_Qua = summary(summ$residuals)[2],
                            Median = summary(summ$residuals)[3],
                            Mean = summary(summ$residuals)[4],
                            Third_Qua = summary(summ$residuals)[5],
                            Max = summary(summ$residuals)[6],
                            row.names = "Residuals"
    )
    write.csv2(x = sum_fit, file = paste("./Figures/sum_fit", num, ".csv", sep = ""))
    write.csv2(x = res_fit, file = paste("./Figures/res_fit", num, ".csv", sep = ""))
}