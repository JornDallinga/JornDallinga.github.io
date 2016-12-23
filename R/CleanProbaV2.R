cleanProbaV2 <- function(f_data, QC_val, fill=255, as.is=FALSE, overwrite = F, ...){
  
  # derive QC filename from data file name, check
  f_QC <- gsub("_[A-Z]+\\.tif", "_SM.tif", f_data)
  if(!file.exists(f_QC)) {
    warning(f_QC, "not processed, no quality flag (SM) file found!")
  } else {
    cat("reading data...")
    
    # as.is = T for datasets without scaling factor
    data <- brick(readGDAL(f_data, as.is=as.is, silent=TRUE))
    cat(inMemory(data))
    cat("reading SM...")
    QC <- raster(f_QC)
    
    clean <- function(x, y) {
      x[!y %in% QC_val] <- NA
      if(!is.null(fill)) {
        x[x %in% fill] <- NA
      }
      return(x)
    }
    cat("start overlay...")
    if(nlayers(data)>1) {
      
      b <- raster::overlay(x = data, y = QC, fun = clean, unstack=T, progress="bar", overwrite = overwrite, ...)
      
      if (hasArg(filename)) {
        cat("writing...")
        writeRaster(b, ... , overwrite = overwrite, bylayer=T, suffix=c("RED0_sm", "NIR0_sm", "BLUE_sm", "SWIR_sm"), progress="bar")
        file.remove(filename)
        gc()
      }
    } else {
      # single leyer, e.g. NDVI
      raster::overlay(x = data, y = QC, fun = clean, overwrite = overwrite, ...)
    }
  }
}
