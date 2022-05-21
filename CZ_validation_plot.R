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
 
#save(bias, file="historial_bias.rda")
load(file="historial_bias.rda")
load(file="historial_gdd_TaiESM.rda")
load(file="historial_bias_precp.rda")
load(file="historial_precp_TaiESM.rda")

cz_metzger <- fun_read_nc("Gens_Global_one_degree_zero.nc")
cz_taiesm <-  fun_read_nc("TaiESM1_historical_r1i1p_climate_zone.nc")


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

cz.met <- (cz_metzger$Gens_index*land_mask[,ny:1])
cz.tai <- (cz_taiesm$CZ_index *land_mask[,ny:1])


ez.met <- array(NA,dim=c(nx,ny))
ez.tai <- array(NA,dim=c(nx,ny))
# start to combine cz.met and cz.tai to GDD-based EN zone
# Extremely cold(1), Cold(2), Cool temperate(3), Warm temperate(4), Hot(5), Extremely hot(6)

#Convert  Metzger Vegetation Stratas to GDD Environmental Zone 
ez.met[((cz.met>=1.)&(cz.met<=12.))|((cz.met>=15.)&(cz.met<=23.))|((cz.met>=25.)&(cz.met<=29.))] <- 1. 
ez.met[(cz.met==13.)|(cz.met==14.)| (cz.met==24.) |((cz.met>=30.)&(cz.met<=42.))|(cz.met==44.)|(cz.met==47.)|(cz.met==48.)] <- 2. 
ez.met[(cz.met==43.)|(cz.met==45.)| (cz.met==46.) |((cz.met>=49.)&(cz.met<=65.))|(cz.met==69)] <- 3.
ez.met[((cz.met>=66.)&(cz.met<=68.))|((cz.met>=70.)&(cz.met<=82.))|(cz.met==84.)|(cz.met==86.)|(cz.met==89.)] <- 4. 
ez.met[((cz.met>=90.)&(cz.met<=107.))|(cz.met==83.)|(cz.met==85.)|(cz.met==87.)|(cz.met==88.)] <- 5. 
ez.met[((cz.met>=108.)&(cz.met<=125.))] <- 6. 

#Convert  TaiESM Climate Zone to GDD Environmental Zone 
ez.tai[(cz.tai==1.)|(cz.tai==2.)|(cz.tai==3.)|(cz.tai==4.)|(cz.tai==6.)] <- 1.
ez.tai[(cz.tai==5.)|(cz.tai==6.)|(cz.tai==7)] <- 2.
ez.tai[(cz.tai==8.)|(cz.tai==9.)|(cz.tai==10.)] <- 3.
ez.tai[(cz.tai==11.)|(cz.tai==12.)] <- 4.
ez.tai[(cz.tai==13.)|(cz.tai==14.)|(cz.tai==15.) ] <- 5.
ez.tai[(cz.tai==16.)|(cz.tai==17.)|(cz.tai==18.)] <- 6.

#  deg.E <- seq(10,150"150")
#  axis(side = 1, at = seq(10,150,length.out=7),
#       labels = paste0(deg.E,"°","E"), tck = -0.02)
#  mtext("Longitude" , side=1, line=4.0, cex=2.5)
 

ld_go <- TRUE
# plot the raster grid with coastlines
if (ld_go) {
# create pdf device 
pdf (file="TaiESM1_Metzger_GDD_Zone_validation.pdf",  width =12.5, height = 6) 
par( mfrow = c( 2, 2)  )  
par(mar=c(1,4,4,3) )
#layout.matrix <- matrix( c(1, 3, 2, 4), nrow = 2, ncol = 2)

#layout(mat = layout.matrix)
#       heights = c(1, 2), # Heights of the two rows
#       widths = c(2, 2)) # Widths of the two columns


leg.at  <-  c("1","25","50","75","100","125")
leg.txt <-  c("1","25","50","75","100","125")
#
plot.gd <- fun_a2r(input.arr=cz.met[,ny:1])
#
plot(plot.gd,  ylim=c(-65,90), xaxt='n', yaxt='n',
# main="Daily mean bias of Growing Degree-Days (oC)",
# xlab="", ylab="Latitude", col=wes_palette("Zissou1", 50, type = "continuous"),
  xlab="", ylab="Latitude", col=my.color,
 smallplot=c(0.85,0.9, 0.2, 0.7), horizontal=F,
       axis.args=list(at=leg.at, labels=leg.txt, cex.axis=0.8),
       legend.args=list(text= "Veg. Statras" , side=3, line=.5, cex=0.5))
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
leg.at  <-  c("1","2","3","4","5","6")
leg.txt <-  c("Ext cold","Cold","Cool","Warm","Hot","Ext hot")
# convert arraty to raster grid 

plot.gd <- fun_a2r(input.arr=ez.met[,ny:1])
#
plot(plot.gd, ylim=c(-65,90), xaxt='n', yaxt='n',
# main="Daily mean bias of Growing Degree-Days (oC/day)",
 xlab="", ylab="", col=ez.color,
 smallplot=c(0.85,0.9, 0.2, 0.7), horizontal=F,
       axis.args=list(at=leg.at, labels=leg.txt, cex.axis=0.8),
       legend.args=list(text= "GDD Zone" , side=3, line=.5, cex=0.5))
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
leg.at  <-  c("2.5","4.5","6","7","8",
              "9","10","11","12",
              "13","14","15","16","17","18")
leg.txt <-  c("Artic/Alpine","Boreal/Alpine","Ext cold/wet","Ext cold/mesic","Cold mesic",
              "Cool tmp/xeric","Cool tmp/moist","Warm tmp/mesic ","Warm tmp/xeric",
              "Sub-trop.","Hot/dry","Hot/arid","Ext hot/arid","Ext hot/xeric","Tropical")
#

plot.gd <- fun_a2r(input.arr=cz.tai[,ny:1])
#
plot(plot.gd,ylim=c(-65,90), xaxt='n', yaxt='n', 

# main="Daily mean bias of Growing Degree-Days (oC)",
 xlab="Longitude", ylab="Latitude", col=cz.color,
 smallplot=c(0.85,0.9, 0.3, 0.8), horizontal=F,
       axis.args=list(at=leg.at, labels=leg.txt, cex.axis=0.5),
       legend.args=list(text= "GDD EnZone" , side=3, line=.5, cex=0.5))
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
leg.at  <-  c("1","2","3","4","5","6")
leg.txt <-  c("Ext cold","Cold","Cool","Warm","Hot","Ext hot")
#

# convert arraty to raster grid 
plot.gd <- fun_a2r(input.arr=ez.tai[,ny:1])
#
plot(plot.gd, ylim=c(-65,90), xaxt='n', yaxt='n',
# main="Daily mean bias of Growing Degree-Days (oC/day)",
# xlab="Longitude", ylab="", col=my.color,
  xlab="Longitude", ylab="", col=ez.color,
 smallplot=c(0.85,0.9, 0.3, 0.8), horizontal=F,
       axis.args=list(at=leg.at, labels=leg.txt, cex.axis=0.8),
       legend.args=list(text= "GDD Zone" , side=3, line=.5, cex=0.5))
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

pdf (file="TaiESM1_Metzger_GDD_Zone_scater_plot.pdf",  width =6, height = 6) 

# state
df <- data.frame(taiesm=summary(as.factor(ez.tai))[1:6], metzger=summary(as.factor(ez.met))[1:6],zone=seq(1:6))

plot(df$taiesm ~ df$metzger, cex=2.5, pch= 22, col=rep("black",6),bg=ez.color, 
xlab="Extend of GDD zone, TaiESM1 simulated (grids)",
ylab="Extend of GDD zone, Metzger Statra Estimated (grids)", xlim=c(0,5000),ylim=c(0,5000))

legend("bottomright",legend=leg.txt,
, fill = ez.color, cex=1.0)


abline(a=0,b=1,lty="dashed") 
dev.off()

