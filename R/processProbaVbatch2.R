processProbaVbatch2 <- function(x, pattern = patterns, tiles=NULL, start_date=NULL, end_date=NULL, QC_val = QC_val, fill=NULL, as.is=FALSE, outdir, ncores=1, overwrite=FALSE) {
  #x <- l0_dir
  if (!is.character(x)) {
    stop('x needs to be of class character')
  }
  
  if(length(x) == 1) {
    
    info <- getProbaVinfo(x, pattern=pattern)
    
    # x <- list.files(path=x, pattern=pattern, full.names=TRUE,  recursive = T, include.dirs = F, no.. = T)
  }
  if (!is.null(tiles)) {
    x <- subset(info, info$tile %in% tiles)
    # x <- x[info$tile %in% tiles]
  }
  if (!is.null(end_date) & !is.null(start_date)) {
    x <- subset(x, x$date >= start_date & x$date <= end_date)
    
  }

  x <- paste0(l0_dir,'/',x$fpath)
  
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  
  if (pattern == "NDVI.tif$"){
    type <- "FLT4S"
  } else {
    type <- dataType(raster(x[1]))
  }
  
  outnames <- file.path(outdir, gsub("\\.tif", "_sm.tif", basename(x)))
  outnames <- gsub("RADIOMETRY_sm\\.tif", "RED0_sm.tif", outnames)
  
  if(!overwrite){
    x <- x[!file.exists(outnames)]
    outnames <- outnames[!file.exists(outnames)]
  }
  
  outnames <- gsub("_RED0_sm\\.tif", ".tif", outnames)
  
  cat("Processing", length(x), "files. Names: ", length(outnames), "\n")
  
  if (ncores > 1){
    registerDoParallel(ncores)
  } else registerDoSEQ()
  
  xprocessed <- foreach(i=x, o=outnames, .combine = c, .multicombine = T, .inorder = F, .packages = c("raster", "rgdal"), .verbose = T ) %dopar% {
    cat("...out:", o)
    r <- cleanProbaV(i, filename=o, QC_val = QC_val, fill=fill, datatype = type, as.is = as.is, overwrite = overwrite )
    print(r)
    o
  }
  
  registerDoSEQ()
  
  if (length(xprocessed) == 0){
    cat(length(xprocessed), " files processed, files allready exists or are out of range")
  } else {
    cat(length(xprocessed), " files processed")
  }
  
  
  # delete if files exists
  f_exist <- subset(outnames, file.exists(outnames) == T)
  if (length(f_exist) > 0 & pattern != "NDVI.tif$"){
    cat("\n","deleting temp files")
    file.remove(f_exist)
    cat(length(f_exist), " files removed")
  }
  
  return(xprocessed)
  
  # old...
  #     mcmapply(FUN=cleanProbaV, f_data=x, filename = outnames,
  #          MoreArgs = list(QC_val = QC_val, fill=fill, datatype = type, read_gdal=read_gdal), mc.cores=mc.cores)
  
}