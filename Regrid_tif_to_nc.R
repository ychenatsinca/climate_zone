#go regid DATASET to one degree  
ld_go <- TRUE 
if (ld_go) {

library("raster")

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
  new.grid <- resample(ard_map, new.grid, method="ngb")

#usee ncdf4 built-in function to creat raster file
nc.filename <- paste("Aridity_one_degree_ngb.nc",sep="")
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
  new.grid <- resample(gdd_map, new.grid, method="ngb")

#usee ncdf4 built-in function to creat raster file
nc.filename <- paste("GDD_one_degree_ngb.nc",sep="")
writeRaster(x=new.grid, filename=nc.filename,
            overwrite=TRUE, format="CDF", varname="GDD_index", varunit="oC",
            longname="Growing-Degree-Days_above_zero_degree", xname="lon", yname="lat")

} #end ld_go


#import global environmental zone The Global Environmental Stratification:
#https://datashare.ed.ac.uk/handle/10283/3089
precp_fname = c("Annual_precipitation.tif")
precp_map <- raster(x=precp_fname)
nc.filename <- paste("annual_precipitation.nc",sep="")
writeRaster(x=precp_map, filename=nc.filename,
            overwrite=TRUE, format="CDF", varname="Ann_precp", varunit="mm",
            longname="Annual precipitaion", xname="lon", yname="lat")

#  new.grid <- raster(ncol=100, nrow=100)
#  extent(new.grid) <- extent(121, 122 ,24.3,25.3)
  new.grid <- raster(ncol=288, nrow=192)
  extent(new.grid) <- extent(-180, 180 ,-90,90)
  new.grid <- resample(precp_map, new.grid, method="ngb")

#usee ncdf4 built-in function to creat raster file
nc.filename <- paste("ANN_PRECP_one_degree_ngb.nc",sep="")
writeRaster(x=new.grid, filename=nc.filename,
            overwrite=TRUE, format="CDF", varname="Ann_precp", varunit="mm",
            longname="Annual precipitation", xname="lon", yname="lat")



