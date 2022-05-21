#check tas data for 10 yrs GDD calculation  
# 
#

#fun_chech_plot <- function( 
#                  tas_fname = c("pr_day_TaiESM1_historical_r1i1p1f1_gn_20100101-20141231.nc"),
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
#coastlines <- readOGR("/lfs/home/ychen/GIS/Coastline/ne_110m_coastline/ne_110m_coastline.shp")

# load the function 
library("raster")
source("./src_function_ncdf4.R")


pr_files <- c("pr_day_TaiESM1_historical_r1i1p1f1_gn_19500101-19591231.nc",
"pr_day_TaiESM1_historical_r1i1p1f1_gn_19600101-19691231.nc",
"pr_day_TaiESM1_historical_r1i1p1f1_gn_19700101-19791231.nc",
"pr_day_TaiESM1_historical_r1i1p1f1_gn_19800101-19891231.nc",
"pr_day_TaiESM1_historical_r1i1p1f1_gn_19900101-19991231.nc")


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
                               

#tas_files <- c("pr_day_TaiESM1_ssp585_2010_2020.nc")


cenario = c("hist")


#run_years <- c ("2010_2020", "2020_2030", "2030_2040", "2040_2050", "2050_2060",
#                "2060_2070", "2070_2080", "2080_2090", "2090_2100")

nruns = 5 
#growing degree days
nx <- 288 
ny <- 192 


#for historical run
prcp.arr <- array(0, dim=c(nx,ny))  


for (irun in 1:nruns) {

#for ssp runs
#pr_fname <- paste("pr_day_TaiESM1_",cenario,"_",run_years[irun],".nc",sep="") 

# for historical run
pr_fname <- pr_files[irun] 


sub_dir <- paste("/",cenario,"/",sep="")

# load the on degree dataset mask:land_mask, tas:near surafce temperature, ard: aridity index  
mask    <- fun_read_nc("../mask/land_frac.nc")
pr_day <- fun_read_nc(paste("../pr_day/",sub_dir, pr_fname, sep="") )

# land mask manipulation
mask$LANDFRAC_PFT[mask$LANDFRAC_PFT <= 0.5] <- NA
mask$LANDFRAC_PFT[mask$LANDFRAC_PFT > 0.5] <- 1.0
land_mask <- mask$LANDFRAC_PFT

# dimensions 
ndays <- length(pr_day$time)
nyrs <- 50 
#nyrs <- ndays/365
#n

print(paste("N-years:",nyrs,sep=""))

#tmp <- g_map$lon
#g_map$lon[1:144] <- tmp[145:288]
#g_map$lon[145:288] <- tmp[1:144]

#get original (Metzger 1950-2000 climatology mean annual precipitation at one-degree.

prcp.nc <- fun_read_nc("ANN_PRECP_one_degree_ngb.nc")
prcp.org  <- prcp.nc$Ann_precp[,ny:1]
tmp <- prcp.org
prcp.org[1:(nx/2),] <- tmp[(nx/2+1):nx,]
prcp.org[(nx/2 +1):nx,] <- tmp[1:(nx/2),]
#apply mask to the dataset
prcp.org <- prcp.org*land_mask

 
# varibales/arries allocation 
# start to calculate the growing degree days
ld_cal<-TRUE
if (ld_cal) {


pr_day <- fun_read_nc(paste("../pr_day/",sub_dir,pr_fname, sep="") )


print(paste("work on years:",pr_fname,sep=""))
#
for (it in 1:ndays) {
   print(paste("working on day:", it, "/",ndays,".",sep=""))
   for (ix in 1:nx) {
   for (iy in 1:ny) {
            prcp.arr[ix,iy]  <-  pr_day$pr[ix,iy,it] + prcp.arr[ix,iy]
   } # iy
   } # ix 
} # it


# take annual average from n*10 years data  
#prcp.arr <- prcp.arr/ as.numeric(nyrs)

}# end of ld_cal

} # end iruns for historical case

#take annual average & unit conversion to mm/yr  (the original unit is kg/m2/s) 
prcp.arr <- (prcp.arr*86400.)/ as.numeric(nyrs)


# bias remov for > 4500 mm
prcp.arr[prcp.arr>4500] <- 4500
prcp.org[prcp.org>4500] <- 4500


#apply the mask to the dataset
prcp.arr <- prcp.arr*land_mask

# no unit conversion
bias.pr <- prcp.arr - prcp.org

save(bias.pr, file="historial_bias_precp.rda")

#load(file="historial_bias.rda")

save(prcp.org, file="historial_precp_Metzger.rda")
save(prcp.arr, file="historial_precp_TaiESM.rda")

# doing the bias correction 
#prcp.arr <- prcp.arr - bias 


