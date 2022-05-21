# 
# import libraries

library(raster)
source("./src_function_ncdf4.R")

mask    <- fun_read_nc("../mask/land_frac.nc")
# land mask manipulation
mask$LANDFRAC_PFT[mask$LANDFRAC_PFT <= 0.5] <- NA
mask$LANDFRAC_PFT[mask$LANDFRAC_PFT > 0.5] <- 1.0
land_mask <- mask$LANDFRAC_PFT


cz_metzger <- raster("Gens_Global_one_degree_zero.nc")

cz_taiesm <- raster("TaiESM1_historical_r1i1p_climate_zone.nc")

#raster to polygon

cz_taiesm_poly <- rasterToPolygons(cz_taiesm, dissolve=TRUE)


# plot the result

plot(cz_metzger, col=rainbow(128))

lines(cz_taiesm_poly, lwd=0.8, col="black")


# validation caclulation

cz_m_arr <- raster::as.array(cz_metzger)
cz_t_arr <- raster::as.array(cz_taiesm)


# load land mask


cz_m <- cz_m_arr[,,1]
cz_t <- cz_t_arr[,,1]

