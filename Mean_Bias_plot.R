library("plyr")
library("fields")
library("rgdal")
library("maptools")
library("wesanderson")
#load the coastlines data by readOGR function from sp package
coastlines <- readOGR("/lfs/home/ychen/GIS/Coastline/ne_110m_coastline/ne_110m_coastline.shp")

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
my.color <- colorRampPalette(c("blue","lightblue","white","pink","red"))(32)


#save(bias, file="historial_bias.rda")
load(file="historial_bias.rda")
load(file="historial_gdd_TaiESM.rda")
load(file="historial_bias_precp.rda")
load(file="historial_precp_TaiESM.rda")


mask    <- fun_read_nc("../mask/land_frac.nc")
# land mask manipulation
mask$LANDFRAC_PFT[mask$LANDFRAC_PFT <= 0.5] <- NA
mask$LANDFRAC_PFT[mask$LANDFRAC_PFT > 0.5] <- 1.0
land_mask <- mask$LANDFRAC_PFT


#  deg.E <- seq(10,150"150")
#  axis(side = 1, at = seq(10,150,length.out=7),
#       labels = paste0(deg.E,"°","E"), tck = -0.02)
#  mtext("Longitude" , side=1, line=4.0, cex=2.5)
 

ld_go <- TRUE
# plot the raster grid with coastlines
if (ld_go) {
# create pdf device 
pdf (file="TaiESM1_GDD_Prep_bias.pdf",  width =12.5, height = 6) 
par( mfrow = c( 2, 2)  )  
par(mar=c(1,4,4,3) )
#layout.matrix <- matrix( c(1, 3, 2, 4), nrow = 2, ncol = 2)

#layout(mat = layout.matrix)
#       heights = c(1, 2), # Heights of the two rows
#       widths = c(2, 2)) # Widths of the two columns


leg.at  <-  c("0","2500", "5000","7500","10000","12500")
leg.txt <-  c("0","2,500","5,000", "7,500","10,000","12,500")
#
plot.gd <- fun_a2r(input.arr=gdd.arr*land_mask)
#
plot(plot.gd,  zlim=c(0,12500),ylim=c(-65,90), xaxt='n', yaxt='n',
# main="Daily mean bias of Growing Degree-Days (oC)",
 xlab="", ylab="Latitude", col=wes_palette("Zissou1", 50, type = "continuous"),
 smallplot=c(0.85,0.9, 0.2, 0.7), horizontal=F,
       axis.args=list(at=leg.at, labels=leg.txt, cex.axis=0.8),
       legend.args=list(text= "GDD (oC)" , side=3, line=.5, cex=0.5))
#add coastline
lines(coastlines, lwd=0.8, add=T)
  degs.lon <- seq(-180,180, 45)
  axis(side = 1, at = degs.lon, srt=0, las=1,
       lab=rep("",9), tck = -0.02)
 
  degs.nlat <- seq(15,90, 30)
  axis(side = 2, at = degs.nlat, srt=0, las=1,
       labels = paste0(degs.nlat,"°","N") , tck = -0.02)
  degs.slat <- seq(-15,-90, -30)
  axis(side = 2, at = degs.slat, srt=0, las=1,
       labels = paste0(degs.slat*-1,"°","S") , tck = -0.02)

  par(xpd=TRUE)
  text(x=-210,y=105, labels="a",cex=1.2, font=2)
 
par(mar=c(1,4,4,3))
#
#
leg.at  <-  c("-10","-5", "0","5","10")
leg.txt <-  c("-10","-5", "0","5","10")
# convert arraty to raster grid 

plot.gd <- fun_a2r(input.arr=bias)
#
plot(plot.gd/365,  zlim=c(-10,10),ylim=c(-65,90), xaxt='n', yaxt='n',
# main="Daily mean bias of Growing Degree-Days (oC/day)",
 xlab="", ylab="", col=my.color,
 smallplot=c(0.85,0.9, 0.2, 0.7), horizontal=F,
       axis.args=list(at=leg.at, labels=leg.txt, cex.axis=0.8),
       legend.args=list(text= "Bias (oC/day)" , side=3, line=.5, cex=0.5))
#add coastline
lines(coastlines, lwd=0.8, add=T)
  degs.lon <- seq(-180,180, 45)
  axis(side = 1, at = degs.lon, srt=0, las=1,
       labels = rep("",9) , tck = -0.02)
  degs.nlat <- seq(15,90, 30)
  axis(side = 2, at = degs.nlat, srt=0, las=1,
       labels = paste0(degs.nlat,"°","N") , tck = -0.02)
  degs.slat <- seq(-15,-90, -30)
  axis(side = 2, at = degs.slat, srt=0, las=1,
       labels = paste0(degs.slat*-1,"°","S") , tck = -0.02)

  par(xpd=TRUE)
  text(x=-210,y=105, labels="b",cex=1.2, font=2)

par(mar=c(4.5,4,1,3))
#
#
leg.at  <-  c("0","750", "1500","2250","3000")
leg.txt <-  c("0","750","1,500", "2,250","3,000")
#
prcp.arr[prcp.arr>3000.] <- 3000.
plot.gd <- fun_a2r(input.arr=prcp.arr*land_mask)

#
plot(plot.gd,ylim=c(-65,90),zlim=c(0,3000), xaxt='n', yaxt='n', 

# main="Daily mean bias of Growing Degree-Days (oC)",
 xlab="Longitude", ylab="Latitude", col=wes_palette("Zissou1", 50, type = "continuous"),
 smallplot=c(0.85,0.9, 0.3, 0.8), horizontal=F,
       axis.args=list(at=leg.at, labels=leg.txt, cex.axis=0.8),
       legend.args=list(text= "Precp. (mm)" , side=3, line=.5, cex=0.5))
#add coastline
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
  par(xpd=TRUE)
  text(x=-210,y=105, labels="c",cex=1.2, font=2)
 

par(mar=c(4.5,4,1,3))
#
leg.at  <-  c("-10","-5", "0","5","10")
leg.txt <-  c("-10","-5", "0","5","10")
bias.pr <- bias.pr/365
bias.pr[bias.pr>=10] <- 10
# convert arraty to raster grid 
plot.gd <- fun_a2r(input.arr=bias.pr)
#
plot(plot.gd,  zlim=c(-10,10),ylim=c(-65,90), xaxt='n', yaxt='n',
# main="Daily mean bias of Growing Degree-Days (oC/day)",
 xlab="Longitude", ylab="", col=my.color,
 smallplot=c(0.85,0.9, 0.3, 0.8), horizontal=F,
       axis.args=list(at=leg.at, labels=leg.txt, cex.axis=0.8),
       legend.args=list(text= "Bias (mm/day)" , side=3, line=.5, cex=0.5))
#add coastline

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

  par(xpd=TRUE)
  text(x=-210,y=105, labels="d",cex=1.2, font=2)
 

#close pdf device 
dev.off()
} #end ld_go


