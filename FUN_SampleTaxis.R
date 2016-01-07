sample.taxis <- function(taxis, perc){
    require(data.table)
    Licenses <- unique(taxis$license)
    n_Licenses <- length(Licenses)
    selection <- sample(Licenses, round(n_Licenses/(100/perc)))#create the random sample
    taxis <- taxis[license %in% selection]#extract data of the selected drivers only
    return(taxis)
}