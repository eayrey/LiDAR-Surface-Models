#Develops surface models from LIDAR point clouds using a triangulated irregular network
#Can be very slow with large datasets.

library("rLiDAR")
library("tripack")
library("RANN")

inFile=readLAS("C:\\lidar_file.las", short=FALSE)
e=extent(inFile[,1:2])
#specify desired resolution
res=1

#High points finder algorithm
#A new method for generating canopy height models from discrete return LiDAR point clouds, Liu and Dong 2014
pnt_fnder=function(inFile){
  #number of points is twice the expected point density given a 1.5m window
  hmm=nn2(inFile[,1:2],treetype='kd',k=round(nrow(inFile)*2*.017675),searchtype = 'radius', radius=2)
  zs_75_quantile=apply(hmm$nn.idx, 1, function(x){quantile(inFile[x[x>0],3],.8)})
  high_pts=inFile[,][inFile[,3]>=zs_75_quantile,]
  #high_pts[complete.cases(high_pts),]
}

#Triangulated irregular network
tiner=function(frst_rtns,e,res){
  #make tin
  frst_rtns=frst_rtns[!duplicated(frst_rtns[,1:2]),]
  tin=tryCatch({
    tri.mesh(frst_rtns[,1], frst_rtns[,2] )
  }, error=function(e){
    frst_rtns <- frst_rtns[sample(nrow(frst_rtns)),]
    tri.mesh(frst_rtns[,1], frst_rtns[,2] )
  }
  )
  #make raster cell centers
  grd=data.frame(expand.grid(x=seq(from=e[1]+res*.5, to=e[2], by=res),y=seq(from=e[3]+res*.5, to=e[4],by=res)))
  
  #find triangle corners for each raster cell
  nrst_verticies=apply(grd,1, function(x) tri.find(tin, x[1], x[2]))
  grd=cbind(grd, data.frame(matrix(unlist(nrst_verticies), nrow=length(nrst_verticies), byrow=T)))
  #basic linear inerpolation within each triangle
  cell_values=apply(grd,1,function(x){
    p1=frst_rtns[as.numeric(x[3]),1:3]
    p2=frst_rtns[as.numeric(x[4]),1:3]
    p3=frst_rtns[as.numeric(x[5]),1:3]
    
    d_p1=sqrt((x[1]-p1[1])^2+(x[2]-p1[2])^2)
    d_p2=sqrt((x[1]-p2[1])^2+(x[2]-p2[2])^2)
    d_p3=sqrt((x[1]-p3[1])^2+(x[2]-p3[2])^2)
    
    total_d=sum(d_p1,d_p2,d_p3, na.rm=TRUE)
    total_p=sum((1-d_p1/total_d), (1-d_p2/total_d), (1-d_p3/total_d), na.rm=TRUE)
    
    #proportions of each vertecies value by distance
    z_value=sum(p1[3]*((1-d_p1/total_d)/total_p), p2[3]*((1-d_p2/total_d)/total_p), p3[3]*((1-d_p3/total_d)/total_p),na.rm=TRUE)
    z_value
  })
  #cell_values=matrix(cell_values, nrow=sqrt(nrow(grd)), ncol=sqrt(nrow(grd)))
  #rotate <- function(x) t(apply(x, 2, rev))
  #cell_values=rotate(rotate(rotate(cell_values)))
  r <- raster(e, ncol=(e[2]-e[1])/resolution, nrow=(e[4]-e[3])/resolution)
  values(r)=cell_values
  #r=raster::mask(r, plotShape_b)
  r
}

#Create digital surfact model
high_pts=pnt_fnder(inFile)
DSM=tiner(high_pts,e, res)

#Create digital elevation model
ground_ps=inFile[inFile[,9]==2,]
DEM=tiner(ground_ps,e, res)

#Create CHM
CHM=DSM-DEM