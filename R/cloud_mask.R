cf_bands = c(1)
thresholds=c(-80, Inf, -120, 120)
thresholds=c(-80, Inf)
thresholds= NULL
span = 0.3

thresholds <- matrix(thresholds, nrow=2)

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



fun2 <- function(x){
  # smooth loess and getHarmMetrics
  m <- matrix(x, nrow= length(bands), ncol=length(dates))
  qcb <- smoothLoess(m, dates = dates, threshold = thresholds, res_type = "QC", span=0.3)

  return(qcb)
}






out_name <- "/home/pi/PROBA_V/ProbaV_JD/rsdata/probav/metrics/test.envi"
out1 <- mcCalc(x=b_vrt, fun=fun1, minrows = 15, mc.cores = mc.cores, logfile=logfile, out_name = out_name, overwrite = T, mc.preschedule = FALSE)

out <- brick(paste0(getwd(), "/rsdata/probav/metrics/test1.envi"))


out$Band.1@data
values(out$Band.4)

sub <- subset(out,10)

for (i in 1:nlayers(out)){
  sub <- subset(out,i)
  #cat("subset:", i)
  if (2 %in% values(sub)){
    xx <- i 
    print(paste(i, " :has temporal outliers", sep = ""))
  }
  
}

####
blue_filter <- subset(out,10)
blue_o <- subset(b_vrt,10)


    