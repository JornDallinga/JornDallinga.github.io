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
data_path <- "/DATA/GEOTIFF/PROBAV_L3_S5_TOC_100M"
#data_path <- getwd()
#getwd()

# ----------------------------------------- Download data --------------------------------------------
# get coords
x <- 10.00
y <- 8.00

# get tile number
t <- probaVTileFromCoords(x, y)

# Lets try wget
u_name <- readline("Type the username:")
p_word <- readline("Type the password:")

dirloc <- paste(getwd(), '/2015/', sep = "")
wget_string <- paste('wget -A \'*X19Y06*\' -P ', dirloc, ' -r --user=', u_name,' --password=', p_word, ' http://www.vito-eodata.be/PDF/datapool/Free_Data/PROBA-V_300m/S1_TOC_-_300_m/2015/7/13/PV_S1_TOC-20150713_333M_V001/?mode=tif', sep="")
wget_string
system(wget_string)





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

patterns <- c('NDVI.tif$') # "NDVI.tif$" 'RADIOMETRY.tif$', 
#patterns <- list('RADIOMETRY.tif$', 'NDVI.tif$')
tiles <- c("X18Y02") #..., "X21Y06")

df_in <- getProbaVinfo(l0_dir, pattern = patterns, tiles = tiles)
df_in %>% ggvis(x=~tile, fill=~band) %>% layer_bars()
nrow(df_in)

# check cores
detectCores(all.tests = FALSE, logical = TRUE)
# parallel with foreach
#start_d = df_in$date[nrow(df_in)],
# similar for NDVI
processProbaVbatch2(l0_dir, 
                    pattern = patterns, tiles = tiles, start_d = "2014-01-25",
                    QC_val = QC_val, outdir = file.path(paste0(getwd(),"/rsdata/probav/sm2", collapse ="")),
                    ncores = (detectCores(all.tests = FALSE, logical = TRUE)-1),
                    overwrite=F)



# check result for red
df_sm <- getProbaVinfo(file.path(paste0(getwd(),"/rsdata/probav/sm2", collapse ="")), pattern = "sm.tif$")
df_sm %>% ggvis(x=~tile, fill=~band) %>% layer_bars()


#### ----------- Extract ts metrics  ------------------------------------------------------
# creates vrt stack, apllies cloud filter and erices metrics -> output metrics brick

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


# --- buld a vrt ---#
#(its faster than raster stack!)
gdalinfo(version = T)


# virtual raster

## Select the bands to use in sequential functions
bands_select <- '(BLUE|SWIR|NDVI)' # e.g. '(BLUE|SWIR|NDVI)' or '(BLUE|SWIR)' or 'NDVI'
bands_sel <- paste(bands_select,'_sm.tif$', sep = "")

if (file.exists(vrt_name)) {
  b_vrt <- brick(vrt_name)
  df_probav_sm <- timeVrtProbaV2(probav_sm_dir, pattern = bands_sel, vrt_name = vrt_name, tile = tiles[tn], return_raster = F, start_date = "2015-08-15", end_date = "2015-10-26")
} else {
  b_vrt <- timeVrtProbaV2(probav_sm_dir, pattern = bands_sel, vrt_name = vrt_name, tile = tiles, return_raster = T, start_date = "2014-02-10", end_date = "2015-10-26")
  df_probav_sm <- timeVrtProbaV2(probav_sm_dir, pattern = bands_sel, vrt_name = vrt_name, tile = tiles[tn], return_raster = F, start_date ="2014-02-10", end_date = "2015-10-26")
}
#b_vrt <- timeStackProbaV2(probav_sm_dir, pattern = '(BLUE|SWIR|NDVI)_sm.tif$', tile = tiles[tn], end_date = "2015-10-26")


# temp idea to reduce extent
plot(b_vrt$PROBAV_S5_TOC_X18Y02_20151016_100M_V001_NDVI_sm.tif)
ext <- drawExtent()
cr <- crop(x = b_vrt, y = ext)
b_vrt <- cr
plot(b_vrt)

names(b_vrt) <- basename(df_probav_sm$fpath)
names(b_vrt) <- paste(probav_sm_dir, names(b_vrt), sep= "")
print(b_vrt)

#plotRGB(b_vrt, 9, 8, 7, stretch='lin')

# --- get metrics ---  #
cat(sprintf("\nlayers: %i  | bands: %s  | blocks: %i  | cores: %i\n",
            nrow(df_probav_sm), paste0(bands, collapse = " "),
            blockSize(b_vrt, minrows = minrows)$n, mc.cores))

## time searies testing $$
#z <- zoo(c(b_vrt[10]), getZ(b_vrt))

# get single cell with dates preserved
z <- zoo(c(b_vrt$PROBAV_S5_TOC_X18Y02_20150301_100M_V001_NDVI_sm.tif[2]), getZ(b_vrt$PROBAV_S5_TOC_X18Y02_20150301_100M_V001_NDVI_sm.tif))

f <- smoothLoess(tsx = z, QC_good=NULL, dates=dates,thresholds=c(-80, Inf, -120, 120) , res_type=c("all"), span=0.3)
plot(f)
plot(f$x)


d <- getHarmMetrics(f$x,QC_good = f$QC_good ,dates = dates, sig = .95, order = 1)
round(d, digits = 3)

b_metrics <- getHarmMetricsSpatial_JE(x = b_vrt, minrows = minrows, mc.cores = mc.cores, logfile=logfile,
                                      overwrite=T, span=0.3, datatype="INT2S", scale_f = c(10,100,10),
                                      cf_bands = c(1,3), thresholds=c(-80, Inf, -120, 120),
                                      filename = out_name, probav_sm_dir = probav_sm_dir, order = 1)

b_metrics1 <- getHarmMetricsSpatial_Or(x = b_vrt, minrows = minrows, mc.cores = mc.cores, logfile=logfile,
                                      overwrite=T, span=0.3, datatype="FLT4S", scale_f = c(10,100,10),
                                      cf_bands = c(1,3), thresholds=c(-80, Inf, -120, 120), filename = out_name )


# datatype="FLT4S"
# datatype="INT2S"
r <- raster("/DATA/GEOTIFF/PROBAV_L3_S5_TOC_NDVI_100M/20150301/PROBAV_S5_TOC_20150301_100M_NDVI_V001/PROBAV_S5_TOC_X18Y02_20150301_100M_NDVI_V001_NDVI.tif")



summary(b_metrics)
Metrics <- readGDAL("/home/pi/PROBA_V/ProbaV_JD/rsdata/probav/metrics/X18Y02_harm_lm2_loess_03_scaled.envi") 


# old
#b_metrics <- getHarmMetricsSpatial_JE(b_vrt, minrows = minrows, mc.cores = mc.cores, logfile=logfile,
#                                      overwrite=T, span=0.3, datatype="INT2S", scale_f = c(10,100,10),
#                                      cf_bands = c(1,3), thresholds=c(-80, Inf, -120, 120),
#                                     filename = out_name)
#
# b_metrics2 <- getHarmMetrics(x = b_vrt, QC_good = QC_val)
#
# t <- smoothLoess(tsx = b_vrt, QC_good = NULL, dates = NULL, threshold = c(-50, Inf),
#                 res_type = "all")


print(b_metrics)
plot(b_metrics)

# not that at thsi poitn you coudl extract spatial metrics
# from the temporal metrics brick using the python scripts.
#



processProbaVbatch_0(l0_dir, pattern = patterns, tiles = tiles, start_d = "2015-10-25",
                     QC_val = QC_val, outdir = file.path(paste0(getwd(),"/rsdata/probav/sm2", collapse ="")),
                     ncores = 3, overwrite=T)




r <- "/DATA/GEOTIFF/PROBAV_L3_S5_TOC_100M/20151021/PROBAV_S5_TOC_20151021_100M_V001/PROBAV_S5_TOC_X18Y02_20151021_100M_V001_NDVI.tif"
# r <- "/DATA/GEOTIFF/PROBAV_L3_S5_TOC_100M/20151021/PROBAV_S5_TOC_20151021_100M_V001/PROBAV_S5_TOC_X18Y02_20151021_100M_V001_RADIOMETRY.tif"
filename <- "/home/pi/PROBA_V/ProbaV_JD/rsdata/probav/sm2/PROBAV_S5_TOC_X18Y02_20151021_100M_V001-test1.tif"
type <- dataType(raster(r[1]))

g <- cleanProbaV2(r, filename=filename, QC_val = QC_val, fill=255, as.is = F, overwrite = T)

r1 <- raster(r)
rr <- raster(filename)




#### ------------- extract train data --------------------------------
# example: cleaned dataset from Tsendbazar 2015
pnt_tsed <- readOGR(file.path(data_path, "/auxdata/lc_ref/tsendbazar2015"), "pnt_ref_africa_tsendbazar", stringsAsFactors = F)

# extract per tile ------------------#
tiles <- c("X16Y06") # ..., "X17Y06", "X18Y06", "X19Y06")

registerDoParallel(min(4, length(tiles)))
df_covs_tsed <- foreach(tile=tiles, .combine=rbind, .inorder = T) %dopar% {
  print(tile)
  b_metrics <- brick(paste0(data_path, "/rsdata/probav/metrics/", tile,"_harm_lm2_loess_03_scaled.envi"))
  print("loaded")
  df_metrics_tile  <- extract(b_metrics, pnt_tsed, cellnumbers=T, df=T)
  df_metrics_tile <- cbind(tile=rep(tile, nrow(df_metrics_tile)), df_metrics_tile)
  df_metrics_tile  <- na.exclude(df_metrics_tile)
  # for spatial segmentation
  #b_clumps <- raster(paste0(data_path, "/rsdata/probav/test/seg/", tile,"_clumps60.img"), RAT=F)
  #df_shape <- read.csv(paste0(data_path, "/rsdata/probav/test/seg/", tile,"_clumps60_shape.csv"))
  #df_stats <- read.csv(paste0(data_path, "/rsdata/probav/test/seg/", tile,"_clumps60_stats.csv"))
  #df_stats_tile  <- df_stats[b_clumps[df_metrics_tile$cells]+1,]
  #df_shape_tile  <- df_shape[b_clumps[df_metrics_tile$cells],]
  #df_covs_tile <- cbind(df_metrics_tile, df_stats_tile[,-1], df_shape_tile[,-1])
  df_covs_tile <- df_metrics_tile
  print(names(df_covs_tile))
  print( nrow(df_covs_tile))
  rm(df_shape, df_stats, b_clumps, b_metrics)
  df_covs_tile
}

# create a df with the trainign classes & metrics
df_ref_tsed <- pnt_tsed@data[df_covs_tsed$ID,]
df_model_tsed <- cbind(G9_cl1=df_ref_tsed$G9_cl1, df_covs_tsed)
# good to save this!

#### ------------- ranger model ------------------------------------
# note that ranger doesn't run parallel on windows
require(ranger)
tiles <- c("X16Y06") #..., "X17Y06", "X18Y06", "X19Y06")

# exclude some classes
df_model_tsed <- subset(df_model_tsed, G9_cl1 <= 5)
# exclude NAs
cc <- complete.cases(df_model_tsed)
table(df_model_tsed$G9_cl1[cc])
names(df_model_tsed)

df_model_tsed$LC <- factor(df_model_tsed$G9_cl1, labels = c("Forest","Shrubland", "Grassland", "Cropland", "Bare"))#, "Wetland", "Urban",  "Water"))
table(df_model_tsed$LC[cc])
glimpse(df_model_tsed[cc, -(2:4)])

cat("------- ranger ---------")
ra_tsed <- ranger(LC ~., df_model_tsed[cc, -(1:4)], num.trees=500, write.forest=T,
                  probability = F, num.threads=10, verbose=T, importance = "impurity")
saveRDS(ra_tsed, "data/models/ra_tzed_merge5_x16.rds")
print(ra_tsed)
#plot(ra_tsed)

ra_tsed <-readRDS("data/models/ra_tzed_merge5_x16.rds")
ra_tsed$confusion.matrix

#### ------------- predict -------------------------------------------
# using ranger
model <- readRDS("data/models/ra_tsed_x16.rds")
tiles <- c("X16Y06")

for (tile in tiles){
  print(paste0("--------------", tile, "-------------------"))
  # -------------------------------data----------------------------------#
  b_metrics <- brick(paste0(data_path, "/rsdata/probav/metrics/", tile,"_harm_lm2_loess_03_scaled.envi"))
  # the follwoing lines are neede to extract manually spatial segments
  # b_clumps <- raster(paste0(data_path, "/rsdata/probav/test/seg/", tile,"_clumps60.img"), RAT=F)
  #  print('shape')
  #  df_shape <- read.csv(paste0(data_path, "/rsdata/probav/test/seg/", tile,"_clumps60_shape.csv"))
  #  print('stats')
  #  df_stats <- read.csv(paste0(data_path, "/rsdata/probav/test/seg/", tile,"_clumps60_stats.csv"))
  #  df_clumps <- cbind(df_stats[-1,-1], df_shape[,-1])
  
  # -------------------------------predict----------------------------------#
  print("---predict--------------")
  
  out_name <- paste0(data_path, "/rsdata/probav/results/tsesd/pred_tsed_", tile,  ".tif")
  
  # this is apralle over mcCalc, additionally ranegr predict can use multiple threads
  # with larger datasets RAM becomes the bottleneck. Therefore it can eb better to use less
  # cores for calc and more for ranger
  
  pred_tsed <- mcPredictSpatial(model,  b_metrics, b_clumps=NULL, df_clumps = NULL, type='response',
                                mc.cores = 5, ranger_threads = 1, minrows = 12, logfile = logfile,
                                datatype ="INT1U", of ="GTiff", out_name = out_name)
  
  print(pred_tsed)
}


