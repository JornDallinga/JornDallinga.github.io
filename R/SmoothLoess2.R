smoothLoess2 <- function(tsx, QC_good=NULL, dates=NULL, threshold=c(-50, Inf), res_type=c("distance", "sd_distance", "all", "filled", "omit", "QC"), ...) {
  if(is.null(QC_good)) {
    QC_good <- as.numeric(!is.na(tsx))
  } else {
    QC_good <- as.numeric(QC_good)
  }
  
  x <- as.numeric(tsx)
  x[QC_good==0] <- NA
  
  if (is.null(dates)){
    dates <- index(tsx)
  }
  dates <- as.numeric(dates)
  loe <-  loess(formula = x ~ dates, na.action = "na.omit", ...)
  #loe <-  loess(formula = x ~ dates, na.action = "na.omit")#, span=0.3)
  loe_pred <- predict(loe, dates)
  #   if (class(x)=="integer") {
  #     loe_pred <- round(loe_pred, 0)
  #   }
  
  distance <-  (loe_pred - x)
  
  if (!is.null(threshold)){
    QC_good[distance < threshold[1] & !is.na(distance)] <- 2
    QC_good[distance > threshold[2] & !is.na(distance)] <- 2
  }
  # prepare output
  if(class(tsx)=="zoo") {
    
    tsx <- zoo(cbind(x = as.numeric(tsx), QC_good, filled=loe_pred), index(tsx))
    # names(tsx) <- c(name_x, "QC_good", paste0(name_x, "_filled"))
    return(tsx)
    
  } else {
    x_omit <- x
    x_omit[QC_good != 1] <- NA
    res <- switch(res_type,
                  all = data.frame( x=as.numeric(tsx), QC_good=QC_good, filled=loe_pred, distance=round(distance)),
                  filled = loe_pred,
                  omit = x_omit,
                  QC = QC_good,
                  distance = distance,
                  sd_distance = (distance / sd(x, na.rm = T)))
    return(res)
  }
}


#plot(dates,loe_pred, type='l')
#points(dates, x)