cf_bands = c(1)
thresholds=c(-80, Inf)
span = 0.3

thresholds <- matrix(thresholds, nrow=2)

df_probav_sm <- getProbaVinfo(probav_sm_dir, pattern =  'BLUE_sm.tif$', tiles = tiles)
s_info <- df_probav_sm
#s_info <- getProbaVinfo(names(x))
bands <- s_info[s_info$date == s_info$date[1], 'band']
dates <- s_info[s_info$band == bands[1], 'date']
ydays <- s_info[s_info$band == bands[1], 'yday']


fun1 <- function(x){
  # smooth loess and getHarmMetrics
  m <- matrix(x, nrow= length(bands), ncol=length(dates))
  #qcb <- smoothLoess(m, dates = dates, thresholds=NULL, res_type = "QC", span=0.3)
  if (!all(is.na(m[1,]))) {
    res <- try({
      # smooth loess on all cf bands, then combine
      qc <- foreach(bn = 1:length(cf_bands), .combine='&') %do% {
        qcb <-   smoothLoess(m[cf_bands[bn],], dates = dates, threshold = thresholds[,bn],
                             res_type = "QC", span=span)
      }
    })  
    
    if(class(res) == 'try-error') {
      res <- rep(NA_integer_, length(dates))
    }
  } else {
    res <- rep(NA_integer_, length(dates))
  }
  
  return(res)
}

b_vrt <- timeVrtProbaV2(probav_sm_dir, pattern = bands_sel, vrt_name = vrt_name, tile = tiles, return_raster = T, start_date = "2015-10-21", end_date = "2016-03-01", te = c(xmin, ymin, xmax, ymax))
out_name <- paste0(getwd(), "/test.envi")# "/home/pi/PROBA_V/ProbaV_JD/rsdata/probav/metrics/test.envi"

out1 <- mcCalc(x=b_vrt, fun=fun1, minrows = 15, mc.cores = 10, logfile=logfile, out_name = out_name, overwrite = T, mc.preschedule = FALSE)

out <- brick(out_name)

