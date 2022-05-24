library("plyr")
library("fields")
library("rgdal")
library("maptools")
library("wesanderson")
#load the coastlines data by readOGR function from sp package
coastlines <- readOGR("/lfs/home/ychen/GIS/Coastline/ne_110m_coastline/ne_110m_coastline.shp")
library("sp")
# load the function 
library("raster")
source("./src_function_ncdf4.R")

# array to raater funtion 
fun_a2r <- function(input.arr,xmin=-180,xmax=180,ymin=-90.,ymax=90.)
        {    
         nx=nrow(input.arr)
         ny=ncol(input.arr)
         print(paste("nx:",nx,"ny:",ny,sep=" "))
         xmin=-180
         xmax=180.
         ymin=-90.
         ymax=90.
         out.grid <- raster(ncol=nx, nrow=ny)
         extent(out.grid) <- extent(xmin, xmax ,ymin,ymax)
         #assign values to raster obj
         tmp <- input.arr
         input.arr[1:(nx/2), ] <- tmp[((nx/2)+1):nx,] 
         input.arr[((nx/2)+1):nx,] <- tmp[1:(nx/2), ]

         values(out.grid) <- c(input.arr[,ny:1])
         return(out.grid)
         }

#my.color for ploting 
#my.color <- colorRampPalette(c("blue","lightblue","white","pink","red"))(32)
my.color <- colorRampPalette(
         c("cyan","cyan","cyan","blue","blue",
           "darkgreen","forestgreen",
           "springgreen3","springgreen1","plum",
           "tan4","tan1",
           "pink1",
           "gold","yellow","lightyellow","wheat",
           "pink"))(18*7)
 
cz.color <- colorRampPalette(
         c("cyan","cyan","cyan","blue","blue",
           "darkgreen","forestgreen",
           "springgreen3","springgreen1","plum",
           "tan4","tan1",
           "pink1",
           "gold","yellow","lightyellow","wheat",
           "pink"))(18)

ez.color <- colorRampPalette(
         c("cyan","blue",
           "springgreen4","springgreen1",
           "gold","pink"))(6)

all.nc.fnames <- list.files("./cz_nc_files/",pattern="*.nc")


n.files <- length(all.nc.fname)

for (i in 2:n.files) {
#for (i in 1:1) {


nc.fname <- all.nc.fnames[i]
poly.fname <- substr(nc.fname, start=1, stop=37) 
 
cz_taiesm <-  fun_read_nc(paste("./cz_nc_files/",nc.fname,sep=""))

#raster to polygon
#cz_taiesm_poly <- rasterToPolygons(cz_taiesm, dissolve=TRUE)


#raster to polygon
#cz_taiesm_poly <- rasterToPolygons(cz_taiesm, dissolve=TRUE)

# plot the result
#plot(cz_metzger, col=rainbow(128))
#lines(cz_taiesm_poly, lwd=0.8, col="black")

# validation caclulation
#cz_m_arr <- raster::as.array(cz_metzger)
#cz_t_arr <- raster::as.array(cz_taiesm)

#cz_m <- cz_m_arr[,,1]
#cz_t <- cz_t_arr[,,1]


mask    <- fun_read_nc("../mask/land_frac.nc")
# land mask manipulation
mask$LANDFRAC_PFT[mask$LANDFRAC_PFT <= 0.5] <- NA
mask$LANDFRAC_PFT[mask$LANDFRAC_PFT > 0.5] <- 1.0
land_mask <- mask$LANDFRAC_PFT

cz.tai <- (cz_taiesm$CZ_index *land_mask[,ny:1])



plot.gd <- fun_a2r(input.arr=cz.tai[,ny:1])

#raster to polygon
cz.tai.poly <- rasterToPolygons(plot.gd, dissolve=TRUE)

#output ploygon as shp 
writeOGR(cz.tai.poly, dsn = './cz_esri_files/', layer = poly.fname, driver = "ESRI Shapefile")


leg.at  <-  c("2.5","4.5","6","7","8",
              "9","10","11","12",
              "13","14","15","16","17","18")
leg.txt <-  c("Artic/Alpine","Boreal/Alpine","Ext cold/wet","Ext cold/mesic","Cold mesic",
              "Cool tmp/xeric","Cool tmp/moist","Warm tmp/mesic ","Warm tmp/xeric",
              "Sub-trop.","Hot/dry","Hot/arid","Ext hot/arid","Ext hot/xeric","Tropical")
#
#
pdf(paste("./cz_esri_files/",poly.fname,".pdf",sep=""), width=12.5, height=6)
plot(plot.gd,  ylim=c(-65,90), xaxt='n', yaxt='n',
# main="Daily mean bias of Growing Degree-Days (oC)",
# xlab="", ylab="Latitude", col=wes_palette("Zissou1", 50, type = "continuous"),
  xlab="", ylab="Latitude", col=cz.color,
 smallplot=c(0.8,0.9, 0.1, 0.8), horizontal=F,
       axis.args=list(at=leg.at, labels=leg.txt, cex.axis=0.8),
       legend.args=list(text= "Veg. Statras" , side=3, line=.5, cex=0.5))
#add coastline
lines(cz.tai.poly, lwd=0.8, add=T,col=cz.color)
lines(coastlines, lwd=0.8, add=T)
  degs.nlat <- seq(15,90, 30)
  axis(side = 2, at = degs.nlat, srt=0, las=1,
       labels = paste0(degs.nlat,"°","N") , tck = -0.02)
  degs.slat <- seq(-15,-90, -30)
  axis(side = 2, at = degs.slat, srt=0, las=1,
       labels = paste0(degs.slat*-1,"°","S") , tck = -0.02)
  
  degs.elon <- seq(45,180, 45)
  axis(side = 1, at = degs.elon, srt=0, las=1,
       labels = paste0(degs.elon,"°","E") , tck = -0.02)
  axis(side = 1, at = 0, srt=0, las=1,
       labels = c("0") , tck = -0.02)
  degs.wlon <- seq(-45,-180, -45)
  axis(side = 1, at = degs.wlon, srt=0, las=1,
       labels = paste0(degs.wlon*-1,"°","W") , tck = -0.02)

dev.off()

} # end of i loop
