library(maptools)
library(rgdal)
library(rgeos)
library(sp)
library(raster)
####FUNCTION Alpha
add.alpha <- function(col, alpha=1){
    if(missing(col))
        stop("Please provide a vector of colours.")
    apply(sapply(col, col2rgb)/255, 2, 
          function(x) 
              rgb(x[1], x[2], x[3], alpha=alpha))  
}
create.bbox <- function(x1 = x1, x2 = x2, y1 = y1, y2 = y2){
    Sr1 = Polygon(cbind(c(x1, x2, x2, x1, x1),
                        c(y1, y1, y2, y2, y1)))
    Sr1 = Polygons(list(Sr1), "s1")
    Sr1 = SpatialPolygons(list(Sr1), 1:1)
    proj4string(Sr1) <- CRS("+init=epsg:4326")
    centroids <- coordinates(Sr1)
    x <- centroids[,1]
    y <- centroids[,2]
    Sr1 <- SpatialPolygonsDataFrame(Sr1,
                                    data=data.frame(x=x, y=y, row.names=row.names(Sr1)))
    return(Sr1)
}

###Bounding box NYC#
x1 = -74.1433; x2 = -73.7629; y1 = 40.5622; y2 = 40.8741; Sr1 <- create.bbox(x1, x2, y1, y2);
#writeOGR(Sr1, "./data/NYC_shapefile/", "bbox", driver = "ESRI Shapefile")


###Bounding box Manhattan#
x1 = -74.0348; x2 = -73.9009; y1 = 40.6869; y2 = 40.8741; Sr2 <- create.bbox(x1, x2, y1, y2);

###Bounding box close-up
x1 = -74.0245; x2 = -73.9606; y1 = 40.6963; y2 = 40.7566; Sr3 <- create.bbox(x1, x2, y1, y2);

###Bounding box close-up2
x1 = -73.9950; x2 = -73.9613; y1 = 40.7509; y2 = 40.7670; Sr4 <- create.bbox(x1, x2, y1, y2);

###Load the shape file of NYC
nyc <- readOGR("./data/NYC_shapefile/", "roads")
nyc_crop <- crop(nyc, Sr1)
manhattan <- crop(nyc, Sr2)
nyc_closeup <- crop(nyc, Sr3)
nyc_closeup2 <- crop(nyc, Sr4)
###Plot data
plot(nyc_crop, col = "white", bg = "black", lwd = .1)
points(taxis10$p_long, taxis10$p_lat, pch = 20, cex = .1, col = add.alpha("green", .1))

plot(manhattan, col = "white", bg = "black", lwd = .1)
points(taxis10$p_long, taxis10$p_lat, pch = 20, cex = .1, col = add.alpha("green", .1))

plot(nyc_closeup, col = "white", bg = "black", lwd = .3)
points(taxis10$p_long, taxis10$p_lat, pch = 20, cex = .1, col = add.alpha("yellow", .1))

plot(nyc_closeup2, col = "white", bg = "black", lwd = .3)
points(taxis10$p_long, taxis10$p_lat, pch = 20, cex = .2, col = add.alpha("yellow", .1))
points(taxis10$d_long, taxis10$d_lat, pch = 20, cex = .2, col = add.alpha("red", .1))