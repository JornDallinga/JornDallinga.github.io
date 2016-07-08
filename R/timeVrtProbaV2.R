timeVrtProbaV2 <- function(x, pattern, stacked_bands=NULL, vrt_name, order_chrono=TRUE, tile=NULL, end_date=NULL, start_date = NULL, return_raster=TRUE){
  
  df_info <- getProbaVinfo(x, pattern)
  if(order_chrono){
    df_info <- df_info[order(df_info$date),]
  }
  
  if (!is.null(tile)) df_info  <- df_info[df_info$tile==tile, ]
  if (!is.null(end_date) & !is.null(start_date)){
    df_info  <- df_info[(df_info$date) <= end_date & (df_info$date) >= start_date,]
  } else if (is.null(end_date)) {
    end_date <- max(df_info$date)
  } else if (is.null(start_date)){
    start_date <- min(df_info$date)
  }
  
  if (!is.null(end_date) & !is.null(start_date)) df_info  <- df_info[(df_info$date) <= end_date & (df_info$date) >= start_date,]
  
  
  if(!is.null(stacked_bands))  {
    cat("\n Builng sub vrts, layers:", length( df_info$fpath[df_info$band %in% stacked_bands]), "\n")
    for (s in df_info$fpath[df_info$band %in% stacked_bands]){
      
      for (b in 1:4){
        s_out <- gsub(s, pattern = "RADIOMETRY", replacement = c("RED0", "NIR0", "BLUE", "SWIR")[b])
        gdalUtils::gdalbuildvrt(s, extension(s_out, "vrt"), b=b, overwrite = T, verbose=F)
      }
      
    }
    
    df_info <- getProbaVinfo(x, "(SM.tif$|NDVI.tif$|RED0|NIR0|BLUE|SWIR)")
    if (!is.null(tile)) df_info  <- df_info[df_info$tile==tile, ]
    if (!is.null(end_date)) df_info  <- df_info[as.numeric(df_info$date) <= end_date,]
  }
  
  cat("\n building main vrt, layers:", nrow(df_info))
  # new
  gdalUtils::gdalbuildvrt(paste(x,df_info$fpath, sep = ""), vrt_name, separate = T, overwrite = T, verbose=F)
  # old (does not work)
  # gdalUtils::gdalbuildvrt(df_info$fpath, vrt_name, separate = T, overwrite = T, verbose=F)
  
  if(return_raster) {
    b <- brick(vrt_name)
    names(b) <- rownames(df_info)
    b <- setZ(x=b, z=format(df_info$date, "%Y%j"))
    return(b)
  }
  
  return(df_info)
}
