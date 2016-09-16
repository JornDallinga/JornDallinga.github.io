cloud_filter <- function(x){
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