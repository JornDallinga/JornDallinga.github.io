library(stringr)
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

# below the fixed link file in order to load the ProbaV package from Johannes
source("R/timeVrtProbaV2.R")
source("R/timeStackProbaV2.R")
source("R/processProbaVbatch2.R")
source("R/getHarmMetricsSpatial2.R")
source("R/mapDistance2Loess2.R")
source("R/CleanProbaV2.R")
source("R/getHarmMetricsSpatial_JE.R")



##
# set your data path
# old path
#data_path <- "/DATA/GEOTIFF/PROBAV_L3_S5_TOC_100M"
data_path <- "/data/MTDA/TIFFDERIVED/PROBAV_L3_S5_TOC_100M"

dir_file <- readRDS("temp/dir_list")

start_date <- "20140116"
end_date <- "20161016"
x <- dir_file
g <- subset(x, str_sub(x,-8,-1) >= start_date & str_sub(x,-8,-1) <= end_date)

QC_val <- getProbaVQClist()$clear_all

patterns <- c('RADIOMETRY.tif$') # "NDVI.tif$" 'RADIOMETRY.tif$', 
tiles <- c("X17Y06") #..., "X21Y06")
#outdir = file.path(paste0(getwd(),"/rsdata/probav/sm2", collapse =""))
outdir <- file.path("/userdata/sm2")

rasterOptions(todisk = F,
              tmpdir = file.path("/userdata/temp", collapse =""), maxmemory = 2e+08, chunksize = 2e+08)


processProbaVbatch2(g,
                    pattern = patterns, tiles = tiles, start_date = "2014-01-16", end_date = "2016-10-16",
                    QC_val = QC_val, outdir = outdir,
                    ncores = 5,
                    overwrite=F)