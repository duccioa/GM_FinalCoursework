#Takes a vector of WORK/BREAK factors and return an number of the day
#which increments at every BREAK
Working.Day <- function(v){
    x <- rep(0, length(v))
    flag <- 1
    for(i in 1:length(v)){
        if(v[i] == "WORK"){x[i] <- flag}
        else {
            x[i] <- paste(flag, "B", sep = "")
            flag <- flag + 1
        }
    }
    return(x)
}
#Iterate the function above to be used with foreach() %dopar%
iter.WorkingDay <- function(dt, license_num){
    Int_Status <- dt[license == license_num, Int_Status]
    return(working.day(Int_Status))
}
