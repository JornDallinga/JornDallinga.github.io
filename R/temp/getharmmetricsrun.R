library(ranger)
library(raster)
library(ggvis)
library(rgdal)
library(dplyr)
library(devtools)
library(gdalUtils)
library(probaV)
library(tools)
library(parallel)
library(zoo)
library(RCurl)
library(stringr)

tiles <- c("X17Y06")
tn <- 1
probav_sm_dir <- "/userdata/sm2/"
outdir <- file.path("/userdata/sm2")
# create output name on the metrics
out_name <- file.path("/home/JD/R_Projects/JornDallinga.github.io", paste0("rsdata/probav/metrics/",tiles[tn],"_harm_lm2_loess_03_scaled1.envi"))

bands_select <- '(BLUE|SWIR|NDVI)' 
bands_sel <- paste(bands_select,'_sm.tif$', sep = "")
vrt_name <- file.path(paste0(outdir,"/",tiles, "_",paste0(bands_select, collapse = "_"), ".vrt"))

xmin <- -2.064549  
xmax <- -1.074974 
ymin <- 8.840184
ymax <- 10.39336

mc.cores <- 3
minrows <- 5
logfile <- file.path("/home/JD/R_Projects/JornDallinga.github.io", paste0("rsdata/probav/logs/", tiles, ".log"))
rasterOptions(todisk = F,
              tmpdir = file.path("/userdata/temp", collapse =""))

# Create virtual stack
b_vrt <- timeVrtProbaV(probav_sm_dir, pattern = bands_sel, vrt_name = vrt_name, tile = tiles, return_raster = T, start_date = "2014-01-21", end_date = "2016-09-01", te = c(xmin, ymin, xmax, ymax))

df_probav_sm <- timeVrtProbaV(probav_sm_dir, pattern = bands_sel, vrt_name = vrt_name, tile = tiles[1], return_raster = F, start_date ="2014-01-21", end_date = "2016-09-01")

bands <-  df_probav_sm[df_probav_sm$date == df_probav_sm$date[1], 'band']
dates <-  df_probav_sm[df_probav_sm$band == bands[1], 'date']

b_metrics <- getHarmMetricsSpatial(x = b_vrt, minrows = minrows, mc.cores = mc.cores,
                                   logfile=logfile,
                                   overwrite=T, span=0.3,
                                   cf_bands = c(1,2), thresholds=c(-80, Inf, -120, 120),
                                   filename = out_name, df_probav_sm = df_probav_sm, 
                                   order = 1, datatype="INT2S", scale_f = c(10,100,10))