#check tas data for 10 yrs GDD calculation  
# 
#

#fun_chech_plot <- function( 
#                  tas_fname = c("tas_day_TaiESM1_historical_r1i1p1f1_gn_20100101-20141231.nc"),
#                  pr_fname =  c("pr_day_TaiESM1_historical_r1i1p1f1_gn_20100101-20141231.nc")  ) 
#{
#  simple funtion to convert arrary to raster 
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


library("plyr")
library("fields")
library("rgdal")
library("maptools")

#load the coastlines data by readOGR function from sp package
coastlines <- readOGR("/lfs/home/ychen/GIS/Coastline/ne_110m_coastline/ne_110m_coastline.shp")

# load the function 
library("raster")
source("./src_function_ncdf4.R")


#tas_files <- c("tas_day_TaiESM1_historical_r1i1p1f1_gn_19500101-19591231.nc",
#"tas_day_TaiESM1_historical_r1i1p1f1_gn_19600101-19691231.nc",
#"tas_day_TaiESM1_historical_r1i1p1f1_gn_19700101-19791231.nc",
#"tas_day_TaiESM1_historical_r1i1p1f1_gn_19800101-19891231.nc",
#"tas_day_TaiESM1_historical_r1i1p1f1_gn_19900101-19991231.nc")


#my.color for ploting 
my.color <- colorRampPalette(c("blue","lightblue","lightgray","pink","red"))(32)

# Artic/Alpine: 1-4, Boreal/Alpine:5-7, Cool temp:8-9 Warm temp:11-12
# sub-trop:13, Dryland:14-17, Tropical:18
cz.color <- colorRampPalette(
         c("cyan","cyan","blue","blue",
           "blueviolet","darkgreen","forestgreen",
           "springgreen3","springgreen1","plum",
           "tan4","tan1",
           "pink1",
           "gold","yellow","lightyellow","wheat",
           "pink"))(18)
                               

#tas_files <- c("tas_day_TaiESM1_ssp585_2010_2020.nc")


cenario = c("ssp126")


run_years <- c ("2010_2020", "2020_2030", "2030_2040", "2040_2050", "2050_2060",
                "2060_2070", "2070_2080", "2080_2090", "2090_2100")

nruns = 9 
#for historical run
#gdd.arr <- array(0, dim=c(nx,ny))  


for (irun in 5:nruns) {

#growing degree days
nx <- 288 
ny <- 192 

gdd.arr <- array(0, dim=c(nx,ny))  

#for ssp runs
tas_fname <- paste("tas_day_TaiESM1_",cenario,"_",run_years[irun],".nc",sep="") 

# for historical run
#tas_fname <- tas_files[irun] 


sub_dir <- paste("/",cenario,"/",sep="")

# load the on degree dataset mask:land_mask, tas:near surafce temperature, ard: aridity index  
mask    <- fun_read_nc("../mask/land_frac.nc")
tas_day <- fun_read_nc(paste("../tas_day/",sub_dir, tas_fname, sep="") )
ard     <- fun_read_nc("Aridity_one_degree.nc")

# land mask manipulation
mask$LANDFRAC_PFT[mask$LANDFRAC_PFT <= 0.5] <- NA
mask$LANDFRAC_PFT[mask$LANDFRAC_PFT > 0.5] <- 1.0
land_mask <- mask$LANDFRAC_PFT

# dimensions 
ndays <- length(tas_day$time)
#nyrs <- 50 
nyrs <- ndays/365
#n

print(paste("N-years:",nyrs,sep=""))

# aridity index manipulation/unit conversion
ard.ind <- ard$Aridity_index/10000.0
ard.ind[ard.ind >= 1.5] <- 1.5
# reverse and shifting the indexes 
ard.ind  <- ard.ind[,ny:1]
tmp <- ard.ind
ard.ind[1:(nx/2),] <- tmp[(nx/2+1):nx,]
ard.ind[(nx/2 +1):nx,] <- tmp[1:(nx/2),]
#apply mask to the dataset
ard.ind <- ard.ind*land_mask

#tmp <- g_map$lon
#g_map$lon[1:144] <- tmp[145:288]
#g_map$lon[145:288] <- tmp[1:144]

#gdd.nc <- fun_read_nc("GDD_one_degree.nc")
#gdd.org  <- gdd.nc$GDD_index[,ny:1]
#tmp <- gdd.org
#gdd.org[1:(nx/2),] <- tmp[(nx/2+1):nx,]
#gdd.org[(nx/2 +1):nx,] <- tmp[1:(nx/2),]
#apply mask to the dataset
#gdd.org <- gdd.org*land_mask


 
# varibales/arries allocation 

#climate zone 
cz.arr <- array(0, dim=c(nx,ny)) 


#start to calculate the growing degree days
ld_cal<-TRUE
if (ld_cal) {

#tas_day <- fun_read_nc(paste("../tas_day/",sub_dir,tas_fname, sep="") )

tas_day <- fun_read_nc(paste("../tas_day/",sub_dir,tas_fname, sep="") )


print(paste("work on years:",tas_fname,sep=""))
#
for (it in 1:ndays) {
   print(paste("working on day:", it, "/",ndays,".",sep=""))
   for (ix in 1:nx) {
   for (iy in 1:ny) {
        # check temperature > 273.15 
        if (tas_day$tas[ix,iy,it] >= 273.15 ) {  
            gdd.arr[ix,iy]  <-  (tas_day$tas[ix,iy,it]-273.15) + gdd.arr[ix,iy]
        } 
   } # iy
   } # ix 
} # it


# take annual average from n*10 years data  
gdd.arr <- gdd.arr/ as.numeric(nyrs)

}# end of ld_cal

#} # end iruns for historical case
#gdd.arr <- gdd.arr/ as.numeric(nyrs)



#apply the mask to the dataset
gdd.arr <- gdd.arr*land_mask

# gdd unit conversion
#gdd.org <- gdd.org/10.
#bias <- gdd.arr - gdd.org

#save(bias, file="historial_bias.rda")
load(file="historial_bias.rda")

#save(gdd.org, file="historial_gdd_Metzger.rda")
#save(gdd.arr, file="historial_gdd_TaiESM.rda")
# doing the bias correction 
gdd.arr <- gdd.arr - bias 

#look up table for the allocation of the climate zone for one degree resolution 
# Reference: 
# Global Ecology and Biogeography, (Global Ecol. Biogeogr.) Metzger, J. Marc et al. (2013) 22, 630–638
# DOI: 10.1111/geb.12022
# Table 2
#  -Growing Degree Days(GDD)-  Name     
#  [   0, 1000)           Ext. cold
#  [1000, 2500)           Cold     
#  [2500, 4500)           Cool temperature
#  [4500, 7000)           Warm temperature
#  [7000, 9000)           Hot
#  [9000, inf )           Ext. hot
#
#  -Aridity (Add)-  
#  [0.0, 0.1)             Arid
#  [0.1, 0.3)             Xeric
#  [0.3, 0.6)             Dry
#  [0.6, 1.0)             Mesic
#  [1.0. 1.5)             Mosit
#  [1.5, inf)             Wet

#   Condition                      Climate Zone Name   Index
#   GDD[0, 1000)    & Ard[1.5,inf) Ext. cold & Wet      4
#   GDD[1000, 2500) & Ard[1.5,inf) Cold & wet           5

#   GDD[0, 1000)    & Ard[0.6,1.0) Ext. cold & mesic    6
#   GDD[1000, 2500) & Ard[0.6,1.0) Cold & mesic         7
#   GDD[2500, 4500) & Ard[0.3,0.6) Cool tmp. & dry      8
#   GDD[2500, 4500) & Ard[0.1,0.3) Cool tmp. & xeric    9
#   GDD[2500, 4500) & Ard[1.0,1.5) Cool tmp. & moist   10

#   GDD[4500, 7000) & Ard[0.6,1.0) Warm tmp. & mesic   11
#   GDD[4500, 7000) & Ard[0.1,0.3) Warm tmp. & xeric   12
#   GDD[7000, 9000) & Ard[0.6,1.0) Hot & mesic         13
#   GDD[7000, 9000) & Ard[0.3,0.6) Hot & dry           14
#   GDD[7000, 9000) & Ard[0.0,0.1) Hot & arid          15

#   GDD[9000, inf)  & Ard[0.0,0.1) Ext. hot & arid     16
#   GDD[9000, inf)  & Ard[0.1,0.3) Ext. hot & xeric    17
#   GDD[9000, inf)  & Ard[1.0,1.5) Ext. hot & moist    18

# starting the climate zoning by the above table
cz.arr[(gdd.arr <= 0.) & (ard.ind <= 1.5) ]   <- 1  # Artic
cz.arr[(gdd.arr <= 1000.) & ((ard.ind >= 1.5 )) ] <- 2  # Artic
cz.arr[(gdd.arr <= 1000.) & ((ard.ind < 1.5))  ]  <- 3  # Artic 
cz.arr[(((gdd.arr>0.)&(gdd.arr<=2500))&((ard.ind>=1.5)))]   <- 4      # Ext. Cold & wet 
cz.arr[(((gdd.arr>0.)&(gdd.arr<=2500))&((ard.ind<1.5)))]   <- 4     # Ext. Cold & wet 
cz.arr[(((gdd.arr>1000.)&(gdd.arr<=2500))&((ard.ind>=1.0)))] <- 5     # Cold & wet
cz.arr[(((gdd.arr>1000.)&(gdd.arr<=2500))&((ard.ind>=1.0)))] <- 5   # Cold & dry


cz.arr[(((gdd.arr>=0.)&(gdd.arr<=1000))&((ard.ind>0.3)&(ard.ind<=1.0)))] <- 6     # Ext. Cold & mesic
cz.arr[(((gdd.arr>1000.)&(gdd.arr<=2500))&((ard.ind>0.6)&(ard.ind<=1.5)))] <- 7  # Cold & mesic  
cz.arr[(((gdd.arr>2500.)&(gdd.arr<=4500))&((ard.ind>0.3)&(ard.ind<=0.6)))] <- 8  # Cool & dry
cz.arr[(((gdd.arr>2500.)&(gdd.arr<=4500))&((ard.ind>=0.0)&(ard.ind<=0.3)))] <- 9  # Cool & xeric
cz.arr[(((gdd.arr>2500.)&(gdd.arr<=4500))&((ard.ind>0.6)&(ard.ind<=1.0)))] <- 9.  # Cool & mesic
cz.arr[(((gdd.arr>2500.)&(gdd.arr<=4500))&((ard.ind>1.0)&(ard.ind<=1.5)))] <- 10 # Cool & moist 

cz.arr[(((gdd.arr>4500.)&(gdd.arr<=7000))&((ard.ind>0.6)&(ard.ind<=1.5)))] <- 11 # Warm & mesic
cz.arr[(((gdd.arr>4500.)&(gdd.arr<=7000))&((ard.ind>0.1)&(ard.ind<=0.6)))] <- 12 # Warm & xeric 
cz.arr[(((gdd.arr>4500.)&(gdd.arr<=7000))&((ard.ind>=0.0)&(ard.ind<=0.1)))] <- 12.  # Warm & arid
cz.arr[(((gdd.arr>7000.)&(gdd.arr<=9000))&((ard.ind>0.6)&(ard.ind<=1.5)))] <- 13 # Hot & mesic

cz.arr[(((gdd.arr>7000.)&(gdd.arr<=9000))&((ard.ind>0.3)&(ard.ind<=0.6)))] <- 14 # Hot & dry
cz.arr[(((gdd.arr>7000.)&(gdd.arr<=9000))&((ard.ind>=0.0)&(ard.ind<=0.1)))] <- 15 # Hot & arid
cz.arr[(((gdd.arr>7000.)&(gdd.arr<=9000))&((ard.ind>0.1)&(ard.ind<=0.3)))] <- 15. # Hot & xeric

cz.arr[(((gdd.arr>9000.)&(gdd.arr<=99999))&((ard.ind>=0.0)&(ard.ind<=0.1)))] <- 16 # Ext hot & arid
cz.arr[(((gdd.arr>9000.)&(gdd.arr<=99999))&((ard.ind>0.1)&(ard.ind<=0.3)))] <- 17 # Ext hot & xeric
cz.arr[(((gdd.arr>9000.)&(gdd.arr<=99999))&((ard.ind>0.3)&(ard.ind<=1.0)))] <- 17. # Ext hot & dry to xeric
cz.arr[(((gdd.arr>9000.)&(gdd.arr<=99999))&((ard.ind>1.0)&(ard.ind<=1.5)))] <- 18 # Ext hot & moist

cz.arr<-cz.arr*land_mask
# output the cz_arr to netcdf file
nc.filename <- paste("./cz_nc_files/",substr(tas_fname,start=9,stop=32),"_climate_zone.nc",sep="") 
cz.grid <- raster(ncol=288, nrow=192)
extent(cz.grid) <- extent(0, 360 ,-90,90)
#assign values to raster obj
values(cz.grid) <- c(cz.arr[,192:1])

#save climate zone raster as netcdf file
writeRaster(x=cz.grid, filename=nc.filename,
            overwrite=TRUE, format="CDF", varname="CZ_index", varunit="-",
            longname="Climate_Zone_index", xname="lon", yname="lat")


### plot the climate zone pdf file
ld_go <- TRUE
# plot the raster grid with coastlines
if (ld_go) {
# create pdf device 
pdf.filename <- paste( "./cz_pdf/",substr(tas_fname,start=9,stop=32),"_climate_zone.pdf",sep="") 
pdf (file=pdf.filename,  width =12, height = 6) 
leg.at  <-  c("2.5","4.5","6","7","8",
              "9","10","11","12",
              "13","14","15","16","17","18")
leg.txt <-  c("Artic/Alpine","Boreal/Alpine","Ext cold/wet","Ext cold/mesic","Cold mesic",
              "Cool tmp/xeric","Cool tmp/moist","Warm tmp/mesic ","Warm tmp/xeric",
              "Sub-trop.","Hot/dry","Hot/arid","Ext hot/arid","Ext hot/xeric","Tropical")
# convert arraty to raster grid 
plot.gd <- fun_a2r(input.arr=cz.arr)
#
plot(plot.gd,  zlim=c(1,18),ylim=c(-65,90),
 main=paste("Global Environmental Zones (", substr(tas_fname,start=9,stop=32),")",sep=""),
 xlab="Longitude", ylab="Latitude", col=cz.color,
 smallplot=c(0.82,0.87, 0.2, 0.8), horizontal=F,
       axis.args=list(at=leg.at, labels=leg.txt, cex.axis=0.8),
       legend.args=list(text= "    Zone Name" , side=3, line=1., cex=1.))
#add coastline
lines(coastlines, lwd=0.8, add=T)
#
#close pdf device 
dev.off()
#ouput png files
png.filename <- paste( "./cz_png/",substr(tas_fname,start=9,stop=32),"_climate_zone.png",sep="") 
png(file=png.filename,  width =1200, height = 600, units="px")
#
plot(plot.gd,  zlim=c(1,18),ylim=c(-65,90),
 main=paste("Global Environmental Zones (", substr(tas_fname,start=9,stop=32),")",sep=""),
 xlab="Longitude", ylab="Latitude", col=cz.color,
 smallplot=c(0.82,0.87, 0.2, 0.8), horizontal=F,
       axis.args=list(at=leg.at, labels=leg.txt, cex.axis=0.8),
       legend.args=list(text= "    Zone Name" , side=3, line=1., cex=1.))
#add coastline
lines(coastlines, lwd=0.8, add=T)
#close png device
dev.off()


} #end ld_go



} # end of iruns 




#go regid DATASET to one degree  
ld_go<-FALSE
if (ld_go) {

#import Aridity map from the dataset:
#https://datashare.ed.ac.uk/handle/10283/3089
ard_fname = c("Aridity.tif")
ard_map <- raster(x=ard_fname)
nc.filename <- paste("Aridity.nc",sep="")
writeRaster(x=ard_map, filename=nc.filename,
            overwrite=TRUE, format="CDF", varname="Aridity_index", varunit="1/10000",
            longname="Aridity_index", xname="lon", yname="lat")

  new.grid <- raster(ncol=288, nrow=192)
  extent(new.grid) <- extent(-180, 180 ,-90,90)
  new.grid <- resample(ard_map, new.grid, method="bilinear")

#usee ncdf4 built-in function to creat raster file
nc.filename <- paste("Aridity_one_degree.nc",sep="")
writeRaster(x=new.grid, filename=nc.filename,
            overwrite=TRUE, format="CDF", varname="Aridity_index", varunit="1/10000",
            longname="Aridity_index", xname="lon", yname="lat")


#import global environmental zone The Global Environmental Stratification:
#https://datashare.ed.ac.uk/handle/10283/3089
gdd_fname = c("GDD_zero.tif")
gdd_map <- raster(x=gdd_fname)
nc.filename <- paste("GDD_zero.nc",sep="")
writeRaster(x=gdd_map, filename=nc.filename,
            overwrite=TRUE, format="CDF", varname="GDD_index", varunit="oC",
            longname="Growing-Degree-Days_above_zero_degree", xname="lon", yname="lat")

#  new.grid <- raster(ncol=100, nrow=100)
#  extent(new.grid) <- extent(121, 122 ,24.3,25.3)
  new.grid <- raster(ncol=288, nrow=192)
  extent(new.grid) <- extent(-180, 180 ,-90,90)
  new.grid <- resample(gdd_map, new.grid, method="bilinear")

#usee ncdf4 built-in function to creat raster file
nc.filename <- paste("GDD_one_degree.nc",sep="")
writeRaster(x=new.grid, filename=nc.filename,
            overwrite=TRUE, format="CDF", varname="GDD_index", varunit="oC",
            longname="Growing-Degree-Days_above_zero_degree", xname="lon", yname="lat")

} #end ld_go




ld_go <- FALSE
# plot the raster grid with coastlines
if (ld_go) {
# create pdf device 
pdf (file="bias-plot.pdf",  width =12, height = 6) 

leg.at  <-  c("-10","-5", "0","5","10")
leg.txt <-  c("-10","-5", "0","5","10")
# convert arraty to raster grid 
plot.gd <- fun_a2r(input.arr=bias)
#
plot(plot.gd/365,  zlim=c(-10,10),ylim=c(-65,90),
 main="Daily mean bias of Growing Degree-Days (oC/day)",
 xlab="Longitude", ylab="Latitude", col=my.color,
 smallplot=c(0.85,0.9, 0.2, 0.8), horizontal=F,
       axis.args=list(at=leg.at, labels=leg.txt, cex.axis=0.8),
       legend.args=list(text= "(oC)" , side=3, line=1., cex=1.))
#add coastline
lines(coastlines, lwd=0.8, add=T)
#
#close pdf device 
dev.off()
} #end ld_go

#g_map <- fun_read_nc("Gens_Global_one_degree.nc")
#reverse the y axis for the calculation and adjust lon index
#g_map$Gens_index <- g_map$Gens_index[,192:1]
#tmp <- g_map$Gens_index
#g_map$Gens_index[1:144,] <- tmp[145:288,]
#g_map$Gens_index[145:288,] <- tmp[1:144,]

#tmp <- g_map$lon
#g_map$lon[1:144] <- tmp[145:288]
#g_map$lon[145:288] <- tmp[1:144]

# set soil Ph grid
#g.grid <- raster(ncol=288, nrow=192)
#extent(g.grid) <- extent(0, 360 ,-90,90)
#assign values to raster obj

#values(g.grid) <- c(g_map$Gens_index[,192:1])

#writeRaster(x=g.grid, filename="Gens_Global_one_degree_zero.nc",
#            overwrite=TRUE, format="CDF", varname="Gens_index", varunit="-",
#            longname="Global Environmental Stratification", xname="lon", yname="lat")
#
#
#} #end _ld

#import global Climatic zone from the tif data:
#https://datashare.ed.ac.uk/handle/10283/3089
#cz_fname = c("/lfs/home/ychen/TaiESM/gen_v3/GEnSv3/GEns_CZ_half_degree.tif")
#cz_fname = c("/lfs/home/ychen/TaiESM/GEnZ_point_one_degree.tif")

#cz_map <- raster(x=cz_fname)

#  extent(new.grid) <- extent(121, 122 ,24.3,25.3)
#  new.grid <- raster(ncol=288, nrow=192)
#  extent(new.grid) <- extent(-180, 180 ,-90,90)
#  new.grid <- resample(cz_map, new.grid, method='ngb')


#usee ncdf4 built-in function to creat raster file
#nc.filename <- paste("Climate_Zone_Global_point_one_degree.nc",sep="")
#writeRaster(x=cz_map, filename=nc.filename,
#            overwrite=TRUE, format="CDF", varname="CZ_index", varunit="-",
#            longname="Climate Zone Index", xname="lon", yname="lat")


#cz_map <- fun_read_nc("Climate_Zone_Global_half_degree.nc")
#reverse the y axis for the calculation and adjust lon index
#cz_map$CZ_index <- cz_map$CZ_index[,192:1]
#tmp <- cz_map$CZ_index
#cz_map$CZ_index[1:144,] <- tmp[145:288,]
#cz_map$CZ_index[145:288,] <- tmp[1:144,]

#tmp <- cz_map$lon
#cz_map$lon[1:144] <- tmp[145:288]
#cz_map$lon[145:288] <- tmp[1:144]

# set soil Ph grid
#cz.grid <- raster(ncol=288, nrow=192)
#extent(cz.grid) <- extent(0, 360 ,-90,90)
#assign values to raster obj

#values(cz.grid) <- c(cz_map$CZ_index[,192:1])

#writeRaster(x=cz.grid, filename="Climate_Zone_Global_one_degree_zero.nc",
#            overwrite=TRUE, format="CDF", varname="CZ_index", varunit="-",
#            longname="Climate Zone Index", xname="lon", yname="lat")
#




ld_plot <- FALSE
if (ld_plot) { 

library("fields")
pdf_fname = c( paste(substr(tas_fname,start=9, stop= (str_n-3)), ".pdf",sep=""))  
pdf(file= paste("./pdf_plot/", pdf_fname, sep="") , width = 9, height = 12 ) 
mask$LANDFRAC_PFT[mask$LANDFRAC_PFT < 0.5] <- 0
#
mask$LANDFRAC_PFT[mask$LANDFRAC_PFT >= 0.5] <- 1 
#

par(mfrow = c(3,1) , mai=c(0.5,0.1,0.5,0.1))
# plot precipitation 
image.plot( apply(pr_day$pr, 1:2, mean)*86400*365 * mask$LANDFRAC_PFT , zlim=c(0,3000), col=tim.colors(),
main="long-term mean annual precipitation (mm /yr)")   
mtext(text=pr_fname,side=3,line=-1, col="white")
# plot temperature 
image.plot( apply(tas_day$tas, 1:2, mean) * mask$LANDFRAC_PFT , zlim=c(270,310), col=tim.colors() ,
main="long-term mean temperature (K)" )
mtext(text=tas_fname,side=3,line=-1)

# plot soil HP
image.plot(  ph_map$sub_soil_PH * mask$LANDFRAC_PFT , zlim=c(3,10), col=tim.colors() ,
main="Sub Soil PH map (-)" )
mtext(text="/ph_sub/S_PH_H2O_one_degree.nc",side=3,line=-1)

dev.off()
}


#}

ld_go <- FALSE
if(ld_go) {
# get all tas files list 
tas_files <- list.files(path="./tas_day/", pattern="tas_day*" ) 
n_files <- length(tas_files) 
#
for (i in 1:n_files) {
#
tas_fname <- tas_files[i]
pr_fname <- paste("pr", substr(tas_fname,start=4,stop=nchar(tas_fname)), sep="") 
#  
print(tas_fname)
print(pr_fname)
#
#}
#}
#call function 
#fun_chech_plot(pr_fname=pr_fname, tas_fname=tas_fname)
#
} 
}
 
