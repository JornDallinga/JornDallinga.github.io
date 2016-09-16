# Example script
Sys.info()['sysname']
.Platform$OS.type
version$os ## or R.version$os

# add additional libary path
.libPaths( c( .libPaths(), "~/R/x86_64-redhat-linux-gnu-library/3.2") )

#### --------- setup -------------
if (!require(ggvis)) install.packages('ggvis', dependencies = T)
if (!require(dplyr)) install.packages('dplyr')
if (!require(gdalUtils)) install.packages('gdalUtils')


# devtools might require additional packages. If fails, run in bash (Centos):
#sudo yum -y install libcurl libcurl-devel

# for devtools, CentOS requires the following:
#sudo yum install libcur*
#sudo yum install libxml*
#sudo yum install openssl*

if (!require(devtools)) install.packages('devtools')


# for rgdal, CentOS requires the following:
# sudo yum install gdal-devel
# sudo yum install proj-devel
# sudo yum install proj-nad
# sudo yum install proj-epsg

if (!require(rgdal)) install.packages('rgdal')

### for ranger, CentOS requires the following:
# In terminal: R -> install.packages('ranger')
if (!require(ranger)) install.packages('ranger')

## probaV
#if (!require(lubridate)) install.packages('lubridate')
#if (!require(probaV)) install_github('johanez/probaV', dependencies = T)
if (!require(probaV)) install.packages('probaV')
#devtools::install_local("probaV", depend = T) # set WD to the folder location

# install_github('johanez/probaV', dependencies = T)

library(rgdal)
library(ranger)
library(raster)
library(ggvis)
library(dplyr)
library(devtools)
library(gdalUtils)
library(probaV)
library(tools)
library(knitr)
library(doParallel)
library(foreach)
library(zoo)

setwd("/home/pi/PROBA_V/ProbaV_JD")
data_path <- "/DATA/GEOTIFF/PROBAV_L3_S5_TOC_100M"

# below the fixed link file in order to load the ProbaV package from Johannes
source("R/timeVrtProbaV2.R")
source("R/timeStackProbaV2.R")
source("R/processProbaVbatch2.R")
source("R/getHarmMetricsSpatial2.R")
source("R/mapDistance2Loess2.R")
source("R/CleanProbaV2.R")
source("R/getHarmMetricsSpatial_JE.R")

#### ----------- Preprocessing  -------------------------------------------------
## ---- check  downloaded data
## Only GeoTIFF is accepted
## The files should be stored in the folder structure used by vito
## (One foler per date contains files for all tiles).
l0_dir <-  file.path(data_path)
df_probav_down <- getProbaVinfo(l0_dir, pattern = ".tif$")
df_probav_down %>% ggvis(x=~tile, fill=~band) %>% layer_bars()
#df_probav_down %>% ggvis(x=~X, y=~Y )

## ---- clean data ---- #
# apply SM mask and split radiometry tif into single layers
QC_val <- getProbaVQClist()$clear_all

patterns <- c('RADIOMETRY.tif$') # "NDVI.tif$" 'RADIOMETRY.tif$', 
#patterns <- list('RADIOMETRY.tif$', 'NDVI.tif$')
tiles <- c("X18Y02") #..., "X21Y06")

df_in <- getProbaVinfo(l0_dir, pattern = patterns, tiles = tiles)
df_in %>% ggvis(x=~tile, fill=~band) %>% layer_bars()
nrow(df_in)

# check cores
detectCores(all.tests = FALSE, logical = TRUE)
# parallel with foreach
# start_d = df_in$date[nrow(df_in)],
# similar for NDVI
# processProbaVbatch2(l0_dir, 
#                     pattern = patterns, tiles = tiles, start_date = "2014-03-06", end_date = "2015-12-06",
#                     QC_val = QC_val, outdir = file.path(paste0(getwd(),"/rsdata/probav/sm2", collapse ="")),
#                     ncores = (detectCores(all.tests = FALSE, logical = TRUE)-1),
#                     overwrite=F)






# ----- cehck input --- #
tn <- 1
tiles <- c("X18Y02")
probav_sm_dir <- file.path(paste0(getwd(),"/rsdata/probav/sm2/", collapse =""))
df_probav_sm <- getProbaVinfo(probav_sm_dir, pattern =  '_sm.tif$', tiles = tiles[tn])
glimpse(df_probav_sm)

# ----- parameters ----#
bands <-  df_probav_sm[df_probav_sm$date == df_probav_sm$date[1], 'band']
dates <-  df_probav_sm[df_probav_sm$band == bands[1], 'date']
minrows = 15
mc.cores = detectCores(all.tests = FALSE, logical = TRUE)-1
#logfile <- paste0("~/PROBA_V/rsdata/lcafrica/logs/metrics_tmp_", tiles[tn], ".log")
logfile <- file.path(getwd(), paste0("rsdata/probav/logs/", tiles[tn], ".log"))

vrt_name <- file.path(getwd(), paste0("rsdata/probav/sm2/", tiles[tn], "_",paste0(bands, collapse = "_"), ".vrt"))
out_name <- file.path(getwd(), paste0("rsdata/probav/metrics/",tiles[tn],"_harm_lm2_loess_03_scaled.envi"))
#rasterOptions(maxmemory = 2e+08, chunksize = 2e+08, todisk = F, progress = "window",
#              tmpdir = file.path(paste0(getwd(),"/rsdata/probav/temp", collapse ="")))

rasterOptions(todisk = F, progress = "text",
              tmpdir = file.path(paste0(getwd(),"/rsdata/probav/temp", collapse ="")))

bands_select <- '(SWIR)' # e.g. '(BLUE|SWIR|NDVI)' or '(BLUE|SWIR)' or 'NDVI'
bands_sel <- paste(bands_select,'_sm.tif$', sep = "")

b_vrt <- timeVrtProbaV2(probav_sm_dir, pattern = bands_sel, vrt_name = vrt_name, tile = tiles, return_raster = T, start_date = "2014-10-12", end_date = "2015-10-26")
df_probav_sm <- timeVrtProbaV2(probav_sm_dir, pattern = bands_sel, vrt_name = vrt_name, tile = tiles[tn], return_raster = F, start_date ="2014-10-12", end_date = "2015-10-26")

xmin <- 4.177083 # 4.177083
xmax <- 5.582837 # 5.582837
ymin <- 50.55804 # 50.55804
ymax <- 52.03919 # 52.03919

e <- extent(c(xmin,xmax,ymin,ymax))
cr <- crop(x = b_vrt, y = e)
b_vrt <- cr

cf_bands = c(1)
thresholds=c(-120, 120)
thresholds <- matrix(thresholds, nrow=2)
#
s_info <- df_probav_sm
#s_info <- getProbaVinfo(names(x))
bands <- s_info[s_info$date == s_info$date[1], 'band']
dates <- s_info[s_info$band == bands[1], 'date']
ydays <- s_info[s_info$band == bands[1], 'yday']
#


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

out_name <- "/home/pi/PROBA_V/ProbaV_JD/rsdata/probav/metrics/test.envi"
mcCalc(x=b_vrt, fun=fun1, minrows = 15, mc.cores = mc.cores, logfile=logfile, out_name = out_name, overwrite = T, mc.preschedule = FALSE)
