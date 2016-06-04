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
if (!require(lubridate)) install.packages('lubridate')
if (!require(probaV)) install_github('johanez/probaV', dependencies = T)
if (!require(probaV)) install.packages('probaV')
devtools::install_local("probaV", depend = T) # set WD to the folder location

# install_github('johanez/probaV', dependencies = T)

require(ranger)
require(raster)
require(ggvis)
require(rgdal)
require(dplyr)
require(devtools)
require(gdalUtils)
require(probaV)


##
# set your data path
data_path <- "/home/pi/PROBA_V"


#### ----------- Preprocessing  -------------------------------------------------
## ---- check  downloaded data
## Only GeoTIFF is accepted
## The files should be stored in the folder structure used by vito
## (One foler per date contains files for all tiles).
l0_dir <-  file.path(data_path, "rsdata/probav/download")
df_probav_down <- getProbaVinfo(l0_dir, pattern = ".tif$")
df_probav_down %>% ggvis(x=~tile, fill=~band) %>% layer_bars()

## ---- clean data ---- #
# apply SM mask and split radiometry tif into single layers
QC_val <- getProbaVQClist()$clear_all
patterns <- c("RADIOMETRY.tif$")
tiles <- c("X16Y06") #..., "X21Y06")
tiles <- c("X00Y01") #..., "X21Y06")

df_in <- getProbaVinfo(l0_dir, pattern = patterns[1], tiles = tiles)
#df_in %>% ggvis(x=~tile, fill=~band) %>% layer_bars()
nrow(df_in)

# check cores
detectCores(all.tests = FALSE, logical = TRUE)
# parallel with foreach
# similar for NDVI
processProbaVbatch(file.path(data_path, "rsdata/probav/download"), pattern = patterns[1], tiles = tiles,
                   QC_val = QC_val, outdir = file.path(data_path, "rsdata/probav/sm"),
                   ncores = (detectCores(all.tests = FALSE, logical = TRUE)-1), overwrite=F)

# check result for red
df_sm <- getProbaVinfo(file.path(data_path, "rsdata/probav/sm"), pattern = "RED0_sm.tif$")
#df_sm %>% ggvis(x=~tile, fill=~band) %>% layer_bars()


#### ----------- Extract ts metrics  ------------------------------------------------------
# creates vrt stack, apllies cloud filter and erices metrics -> output metrics brick
# ----- parameters ----#
tiles <- c("X16Y06")
tn <- 1
minrows = 15
mc.cores = 3
logfile <- paste0("~/PROBA_V/rsdata/lcafrica/logs/metrics_tmp_", tiles[tn], ".log")
probav_sm_dir <- file.path(data_path, "rsdata/probav/sm/")
vrt_name <- file.path(data_path, paste0("rsdata/probav/sm/", tiles[tn], "_",paste0(bands, collapse = "_"), ".vrt"))
out_name <- file.path(data_path, paste0("rsdata/probav/metrics/",tiles[tn],"_harm_lm2_loess_03_scaled.envi"))
rasterOptions(maxmemory = 2e+08, chunksize = 2e+08, todisk = F, progress = "text")

# ----- cehck input --- #
df_probav_sm  <- getProbaVinfo(probav_sm_dir, pattern =  '(BLUE|SWIR|NDVI)_sm.tif$', tiles = tiles[tn])
glimpse(df_probav_sm)

bands <-  df_probav_sm[df_probav_sm$date == df_probav_sm$date[1], 'band']
dates <-  df_probav_sm[df_probav_sm$band == bands[1], 'date']

# --- buld a vrt ---#
#(its faster than raster stack!)
gdalinfo(version = T)

if (file.exists(vrt_name)) {
  b_vrt <- brick(vrt_name)
} else {
  b_vrt <- timeVrtProbaV2(x = probav_sm_dir, pattern = '(BLUE|SWIR|NDVI)_sm.tif$', vrt_name = vrt_name2, tile = tiles[tn], return_raster = T)
}

names(b_vrt) <- basename(df_probav_sm$fpath)
print(b_vrt)

#plotRGB(b_vrt, 9, 8, 7, stretch='lin')

# --- get metrics ---  #
cat(sprintf("\nlayers: %i  | bands: %s  | blocks: %i  | cores: %i\n",
            nrow(df_probav_sm), paste0(bands, collapse = " "),
            blockSize(b_vrt, minrows = minrows)$n, mc.cores))


b_metrics <- getHarmMetricsSpatial(b_vrt, minrows = minrows, mc.cores = mc.cores, logfile=logfile,
                                   overwrite=T, span=0.3, datatype="INT2S", scale_f = c(10,100,10),
                                   cf_bands = c(1,3), thresholds=c(-80, Inf, -120, 120),
                                   filename = out_name)


print(b_metrics)

# not that at thsi poitn you coudl extract spatial metrics
# from the temporal metrics brick using the python scripts.
#
