### Given a list of coordinates of the lower left and upper right corners 
### obtained with locator(), return the bounding box
create.bbox <- function(coords_list){
    require(sp)
    x1 <- coords_list[[1]][1]
    x2 <- coords_list[[1]][2]
    y1 <- coords_list[[2]][1]
    y2 <- coords_list[[2]][2]
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