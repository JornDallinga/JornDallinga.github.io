timeStackProbaV2 <- function(x, pattern, order_chrono=TRUE, tile=NULL, quick=FALSE, end_date=NULL, ...){
  
  df_info <- getProbaVinfo(x, pattern)
  if(order_chrono){
    df_info <- df_info[order(df_info$date),]
  }
  
  if (!is.null(tile)) df_info  <- df_info[df_info$tile==tile, ]
  if (!is.null(end_date)) df_info  <- df_info[as.numeric(df_info$date) <= end_date,]
  
  
  s <- raster::stack(file.path(fdir, df_info$fpath), quick=quick)
  
  #cat("build brick ... ")
  #s <- brick(s)
  names(s) <- row.names(df_info)
  
  s <- setZ(x=s, z=format(df_info$date, "%Y%j"))
  
  if(hasArg(filename)) {
    cat("writing...")
    out <- writeRaster(s, progress="bar", ... )
    return(out)
  }
  
  return(s)
}