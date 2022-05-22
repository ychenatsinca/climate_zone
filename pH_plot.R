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
 
#save(bias, file="historial_bias.rda")
#load(file="historial_bias.rda")
#load(file="historial_gdd_TaiESM.rda")
#load(file="historial_bias_precp.rda")
#load(file="historial_precp_TaiESM.rda")

ph_FAO <- fun_read_nc("S_PH_one_degree_zero_corrected.nc")


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

ph.fao <- (ph_FAO$sub_soil_PH*land_mask[,ny:1])



ld_go <- TRUE
# plot the raster grid with coastlines
if (ld_go) {
# create pdf device 
pdf (file="Soil_pH_FAO.pdf",  width =12.5, height = 6) 
par( mfrow = c( 1, 1)  )  
par(mar=c(6,4,6,3) )
#layout.matrix <- matrix( c(1, 3, 2, 4), nrow = 2, ncol = 2)

#layout(mat = layout.matrix)
#       heights = c(1, 2), # Heights of the two rows
#       widths = c(2, 2)) # Widths of the two columns


leg.at  <-  c("3.5","4","5","6","7","8","9","10")
leg.txt <-  c("3.5","4","5","6","7","8","9","10")
#
plot.gd <- fun_a2r(input.arr=ph.fao[,ny:1])
#
plot(plot.gd, ylim=c(-65,90), xaxt='n', yaxt='n',
# main="Daily mean bias of Growing Degree-Days (oC)",
# xlab="", ylab="Latitude", col=wes_palette("Zissou1", 50, type = "continuous"),
  xlab="Longitude", ylab="Latitude", col=rainbow(30),
 smallplot=c(0.85,0.9, 0.2, 0.7), horizontal=F,
       axis.args=list(at=leg.at, labels=leg.txt, cex.axis=0.8),
       legend.args=list(text= "Soil pH" , side=3, line=.5, cex=1.0))
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
  #text(x=-210,y=105, labels="a",cex=1.2, font=2)
 
#close pdf device 
dev.off()
} #end ld_go


