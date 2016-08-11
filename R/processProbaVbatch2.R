processProbaVbatch2 <- function(x, pattern = patterns, tiles=NULL, start_d=NULL, QC_val = QC_val, fill=NULL, as.is=FALSE, outdir, ncores=1, overwrite=FALSE) {
  # x <- l0_dir
  if (class(pattern) == 'list') pattern <- unlist(pattern)
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
  if (!is.null(start_d)) {
    x <- subset(x, x$date >= start_d)
    
  }
  
  x <- paste0(l0_dir,'/',x$fpath)
  
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  type <- dataType(raster(x[1]))
  
  outnames <- file.path(outdir, gsub("\\.tif", "_sm.tif", basename(x)))
  # for unstacking radiometry image, sm will be in band suffix, for checking we use the red band
  outnames <- gsub("RADIOMETRY_sm\\.tif", "RED0_sm.tif", outnames)
  #x2 <- list.files(outdir, full.names = T)
  #if(!overwrite){
  #  x <- x[!file.exists(outnames)]
  #  outnames <- outnames[!file.exists(outnames)]
  #}
  outnames <- gsub("_RED0_sm\\.tif", ".tif", outnames)
  
  
  cat("Processing", length(x), "files. Names: ", length(outnames), "\n")
  
  
  if (ncores > 1){
    registerDoParallel(ncores)
  } else registerDoSEQ()
  
  xprocessed <- foreach(i=x, o=outnames, .combine = c, .multicombine = T, .inorder = F, .packages = c("raster", "rgdal"), .verbose = T ) %dopar% {
    cat("...out:", o)
    r <- cleanProbaV(i, filename=o, QC_val = QC_val, fill=fill, datatype = type, as.is = as.is )
    print(r)
    o
  }
  
  registerDoSEQ()
  cat(length(xprocessed), " files processed")
  return(xprocessed)
  
  # old...
  #     mcmapply(FUN=cleanProbaV, f_data=x, filename = outnames,
  #          MoreArgs = list(QC_val = QC_val, fill=fill, datatype = type, read_gdal=read_gdal), mc.cores=mc.cores)
  
}