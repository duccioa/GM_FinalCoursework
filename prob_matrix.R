library(lubridate)
library(maptools)
library(rgdal)
library(rgeos)
library(sp)
library(raster)
#################### FUNCTIONS #################################################
cut.taxis <- function(df,DAY, HOUR){
    require(lubridate)
    return(df[hour(df$p_time) == HOUR & day(df$p_time) == DAY, ])
}
#####
create.bbox <- function(coords_list){
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
#################### READ THE MAP ##############################################
nyc_map <- readOGR("./data/NYC_shapefile/", "roads")
#################### RASTER and VECTOR GRID ####################################
### Prepare the shape file
x1 = -74.042; x2 = -73.942; y1 = 40.7; y2 = 40.76; c_list <- create.bbox(list(c(x1,x2), c(y1,y2)));
nyc_c <- crop(nyc_map, c_list)
taxis_cut <- cut.taxis(taxis, 5, 10)
r <- raster.grid(d = 0.001, EXT = nyc_c)
rg <- as(r, "SpatialGrid")
spdf <- SpatialPointsDataFrame(cbind(taxis_cut$p_long, taxis_cut$p_lat), data.frame(taxis_cut))
proj4string(spdf) <- CRS("+init=epsg:4326")
proj4string(rg) <- CRS("+init=epsg:4326")
spdf <- spdf[rg,]
plot(rg, lwd = 1)
plot(spdf, pch = 20, cex = .5, col = "red", add = T)
plot(nyc_c, lwd = .5, add = TRUE)

r[] <- 0
tab <- table(cellFromXY(r, spdf))
r[as.numeric(names(tab))] <- tab
plot(r)
