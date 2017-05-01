library("rLiDAR", lib.loc="~/R/win-library/3.2")
library(fields) 

inFile=readLAS("C:\\lidar_file.las", short=FALSE)
e=extent(inFile[,1:2])

#Create a digital surface model
resolution=1
dsmr <- raster(e, ncol=(e[2]-e[1])/resolution, nrow=(e[4]-e[3])/resolution)
DSM <- rasterize(inFile[, 1:2], dsmr, inFile[,3], fun=max)

#Create a map of pulse density
resolution=20
ppmr<- raster(e, ncol=(e[2]-e[1])/resolution, nrow=(e[4]-e[3])/resolution)
ppm <- rasterize(subset(inFile,inFile[,5]==1)[, 1:2], ppmr, inFile[,3], fun='count')

#Create a map of pulse density
resolution=1
ppmr<- raster(e, ncol=(e[2]-e[1])/resolution, nrow=(e[4]-e[3])/resolution)
ppm <- rasterize(subset(inFile,inFile[,5]==1)[, 1:2], ppmr, inFile[,3], fun='count')

#Create a digital elevation model
ground_ps=inFile[inFile[,9]==2,]
resolution=1
DEMr <- raster(e, ncol=(e[2]-e[1])/resolution, nrow=(e[4]-e[3])/resolution)
DEM <- rasterize(ground_ps[, 1:2], DEMr, ground_ps[,3], fun=max)

#thin plate spline interpolation to remove holes in the DEM
ra=aggregate(DEM,10)
xy <- data.frame(xyFromCell(ra, 1:ncell(ra)))
v <- getValues(ra)
tps <- Tps(xy, v)
DEM=interpolate(DEM, tps)

#Generate a canopy height model
CHM=DSM-DEM
