
# for unstacking radiometry image, sm will be in band suffix, for checking we use the red band
outnames_red <- gsub("RADIOMETRY_sm\\.tif", "RED0_sm.tif", outnames)
outnames_blue <- gsub("RADIOMETRY_sm\\.tif", "BLUE_sm.tif", outnames)
outnames_nir <- gsub("RADIOMETRY_sm\\.tif", "NIR0_sm.tif", outnames)
outnames_swir <- gsub("RADIOMETRY_sm\\.tif", "SWIR_sm.tif", outnames)
outnames_ndvi <- gsub("RADIOMETRY_sm\\.tif", "NDVI_sm.tif", outnames)
x2 <- c(outnames_red, outnames_blue, outnames_nir, outnames_swir, outnames_ndvi)

d <- subset(x2, file.exists(x2) == T)
dd <- subset(x2, file.exists(x2) == F)




if (overwrite == T & length(d) > 0){
  file.remove(d)
  outnames <- d
} else if (overwrite == F & length(dd) > 0){
  outnames <- dd
} else if (overwrite == F & length(dd) == 0){
  return(print("all files exist, function stopped"))
}


