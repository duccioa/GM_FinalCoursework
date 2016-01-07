##### Create a raster with cells of equal x and y dimensions d 
##### and extent EXT - class Spatial

raster.grid <- function(d = 0.0001, EXT){
    dim.x <- d
    dim.y <- d
    bb <- bbox(EXT)
    cells.x <- (bb[1,2]-bb[1,1])/dim.x
    cells.y <- (bb[2,2]-bb[2,1])/dim.x
    round.vals <- function(x){
        if(as.integer(x) < x){x <- as.integer(x)+1}
        else{x <- as.integer(x)}
    }
    cells.x <<- round.vals(cells.x)
    cells.y <<- round.vals(cells.y)
    ext <- extent(c(bb[1,1], bb[1,1] + cells.x*d, bb[2,1], bb[2,1] + cells.y*d))
    r <- raster(ncols = cells.x, nrow = cells.y)
    extent(r) <- ext
    return(r)
}