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
if (!require(gdalUtils)) install.packages('RCurl')


# devtools might require additional packages. If fails, run in bash (Centos):
# for devtools, CentOS requires the following:

#sudo yum -y install libcurl libcurl-devel
#sudo yum install libcur*
#sudo yum install libxml*
#sudo yum install openssl*

if (!require(devtools)) install.packages('devtools')

# rgdal could fail to install on centos. install the following rgdal required applications for a successful instalation of rgdal
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
#if (!require(probaV)) install_github('johanez/probaV', dependencies = T)
if (!require(probaV)) install_github('JornDallinga/probaV', dependencies = T)
if (!require(probaV)) install.packages('probaV')
#devtools::install_local("probaV", depend = T) # set WD to the folder location

# install_github('johanez/probaV', dependencies = T)
if (!require(knitrBootstrap)) install.packages('knitrBootstrap')
if (!require(zoo)) install.packages('zoo')
if (!require(stringr)) install.packages('stringr')
if (!require(stringr)) install.packages('repmis')



library(stringr)
library(rgdal)
library(ranger)
library(raster)
library(ggvis)
library(dplyr)
library(devtools)
library(probaV)
library(tools)
library(knitr)
library(doParallel)
library(foreach)
library(zoo)
library(gdalUtils)

remove.packages(gdalUtils,"~/R/x86_64-redhat-linux-gnu-library/3.2" )

# below the fixed link file in order to load the ProbaV package from Johannes
source("R/timeVrtProbaV2.R")
source("R/timeStackProbaV2.R")
source("R/processProbaVbatch2.R")
source("R/getHarmMetricsSpatial2.R")
source("R/mapDistance2Loess2.R")
source("R/CleanProbaV2.R")
source("R/getHarmMetricsSpatial_JE.R")
source("R/SmoothLoess2.R")


# Run script
system("R < /home/JD/R_Projects/JornDallinga.github.io/R/Run_ProcessBath2.R --no-save")

##
# set your data path
# old path
#data_path <- "/DATA/GEOTIFF/PROBAV_L3_S5_TOC_100M"
data_path <- "/data/MTDA/TIFFDERIVED/PROBAV_L3_S5_TOC_100M"
list.files(data_path)
#data_path <- getwd()
#getwd()

# ----------------------------------------- Download data --------------------------------------------
# get coords
x <- -11.00
y <- 9.00

# get tile number
t <- probaVTileFromCoords(x, y)
t
# Lets try wget
u_name <- readline("Type the username:")
p_word <- readline("Type the password:")

# needed tiles X16Y06....X19Y06

dirloc <- paste(getwd(), '/rsdata/probav/download/2014/', sep = "")
dirloc 
wget_string <- paste('wget -A \'*X19Y06*\' -P ', dirloc, ' --no-directories -r --user=', u_name,' --password=', p_word, ' http://www.vito-eodata.be/PDF/datapool/Free_Data/PROBA-V_300m/S1_TOC_-_300_m/2015/7/13/PV_S1_TOC-20150713_333M_V001/?mode=tif', sep="")
wget_string <- paste('wget -A \'*X16Y06*\' -P ', dirloc, ' --no-directories -r --user=', u_name,' --password=', p_word, ' http://www.vito-eodata.be/PDF/datapool/Free_Data/PROBA-V_100m/S5_TOC_100_m/2016/2/?mode=tif', sep="")

#http://www.vito-eodata.be/PDF/datapool/Free_Data/PROBA-V_300m/S1_TOC_-_300_m/2015/7/13?coord=-11.0,9.0,-11.1,9.1&mode=tif

#--no-directories
wget_string
system(wget_string)


g<- raster('/home/pi/PROBA_V/ProbaV_JD/rsdata/probav/download/2014/PROBAV_S5_TOC_X19Y06_20160211_100M_V002_NDVI.tif')


#### ----------- Preprocessing  -------------------------------------------------
## ---- check  downloaded data
## Only GeoTIFF is accepted
## The files should be stored in the folder structure used by vito
library(repmis)
g <- source_data("https://github.com/JornDallinga/JornDallinga.github.io/blob/master/temp/dir_list.Rdata?raw=True")

githubURL <- "https://github.com/JornDallinga/JornDallinga.github.io/blob/master/temp/dir_list"
x <- getURL(githubURL)
out <- readRDS(textConnection(x))

download.file(githubURL,"temp/dir_list", method="curl")
dir_file <- readRDS("temp/dir_list")

githubURL <- "https://github.com/JornDallinga/JornDallinga.github.io/blob/master/temp/dir_list.RData"
load(url(githubURL))
head(df)

## (One foler per date contains files for all tiles).
dir_file <- readRDS("temp/dir_list")

tiles <- c("X20Y01")
datalist = list()
for (i in 1:length(dir_file)){
  dat <- getProbaVinfo(dir_file[i], pattern = ".tif$", tiles = "X18Y02")
  datalist[[i]] <- dat # add it to your list
}

df_probav_down <- do.call(rbind, datalist)

# df_probav_down <- getProbaVinfo(fd2, pattern = ".tif$", tiles = "X18Y02")
df_probav_down %>% ggvis(x=~tile, fill=~band) %>% layer_bars()
# df_probav_down %>% ggvis(x=~X, y=~Y )


## ---- clean data ---- #
# apply SM mask and split radiometry tif into single layers
QC_val <- getProbaVQClist()$clear_all

patterns <- c('NDVI.tif$') # "NDVI.tif$" 'RADIOMETRY.tif$', 
tiles <- c("X20Y01") #..., "X21Y06")X18Y02

df = list()
for (i in 1:length(dir_file)){
  dat <- getProbaVinfo(dir_file[i], pattern = patterns, tiles = tiles)
  df[[i]] <- dat # add it to your list
}

df_in <- do.call(rbind, df)

#-----------------
#l0_dir <- data_path
df_in <- getProbaVinfo(l0_dir, pattern = patterns, tiles = tiles)
df_in %>% ggvis(x=~tile, fill=~band) %>% layer_bars()
nrow(df_in)

# set directory to process and store files
outdir <- file.path("/userdata/sm2")

# check cores
detectCores(all.tests = FALSE, logical = TRUE)
# parallel with foreach
# similar for NDVI
processProbaVbatch2(g, 
                    pattern = patterns, tiles = tiles, start_date = "2016-06-11", end_date = "2016-06-11",
                    QC_val = QC_val, outdir = outdir,
                    ncores = (detectCores(all.tests = FALSE, logical = TRUE)-1),
                    overwrite=F)

start_date <- "20160611"
end_date <- "20160611"
x <- dir_file
g <- subset(x, str_sub(x,-8,-1) >= start_date & str_sub(x,-8,-1) <= end_date)
outdir <- file.path("/userdata/sm2")

processProbaVbatch2(g,
                    pattern = patterns, tiles = tiles, start_date = "2016-06-11", end_date = "2016-06-11",
                    QC_val = QC_val, outdir = outdir,
                    ncores = 5,
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
              tmpdir = file.path("/rsdata/probav/temp", collapse =""), maxmemory = 2e+08, chunksize = 2e+08)




# --- buld a vrt ---#
#(its faster than raster stack!)
gdalinfo(version = T)


# virtual raster

## Select the bands to use in sequential functions
bands_select <- '(BLUE|SWIR|NDVI)' # e.g. '(BLUE|SWIR|NDVI)' or '(BLUE|SWIR)' or 'NDVI'
bands_select <- '(NDVI)' # e.g. '(BLUE|SWIR|NDVI)' or '(BLUE|SWIR)' or 'NDVI'

bands_sel <- paste(bands_select,'_sm.tif$', sep = "")

b_vrt <- timeVrtProbaV2(probav_sm_dir, pattern = bands_sel, vrt_name = vrt_name, tile = tiles, return_raster = T, start_date = "2014-03-06", end_date = "2016-12-06", te = c(5.996773,51.80702,6.468209,52.43639))
df_probav_sm <- timeVrtProbaV2(probav_sm_dir, pattern = bands_sel, vrt_name = vrt_name, tile = tiles[tn], return_raster = F, start_date = "2014-03-06", end_date = "2015-12-06")



#---------------------------start plotting ------------------------------------------#
# plot RGB
bands_select <- '(BLUE|SWIR|NIR0|RED0)' # e.g. '(BLUE|SWIR|NDVI)' or '(BLUE|SWIR)' or 'NDVI'
bands_select <- '(BLUE|SWIR|NIR0)' 
bands_sel <- paste(bands_select,'_sm.tif$', sep = "")

# dir with clouds
probav_sm_dir <- "/home/pi/PROBA_V/ProbaV_JD/rsdata/probav/sm_Withclouds/"
# dir without clouds
probav_sm_dir <- "/home/pi/PROBA_V/ProbaV_JD/rsdata/probav/sm2/"

b_vrt <- timeVrtProbaV2(probav_sm_dir, pattern = bands_sel, vrt_name = vrt_name, tile = tiles, return_raster = T, start_date = "2015-04-15", end_date = "2015-04-17")
df_probav_sm <- timeVrtProbaV2(probav_sm_dir, pattern = bands_sel, vrt_name = vrt_name, tile = tiles[tn], return_raster = F, start_date ="2015-04-15", end_date = "2015-04-17")

# set par back to normal
par(mfrow=c(1,1))
plotRGB(b_vrt,3, 2, 1, stretch='lin')


# with clouds
plot(b_vrt)
b_vrt_cloud <- b_vrt
plotRGB(b_vrt_cloud, 3, 2, 1, stretch='lin')


# cloud mask SM
plot(b_vrt)
r <- b_vrt
r2 <- r
r2[is.na(r)] <- 9999
r2[r2 < 9999] <- NA

cl <- brick("/home/pi/PROBA_V/ProbaV_JD/rsdata/probav/cloudfilter/Clouds.tif")
cl_ma <- raster("/home/pi/PROBA_V/ProbaV_JD/rsdata/probav/cloudfilter/cloud_mask_SM.tif")
cl_fi <- raster("/home/pi/PROBA_V/ProbaV_JD/rsdata/probav/cloudfilter/cloudfilter.tif")

cl_ma[cl_fi == 1] <- 1
breakpoints <- c(0,2,9999)
colors <- c("darkred","red")
plot(cl_ma,breaks=breakpoints,col=colors, add = T, legend = F)

plotRGB(cl, 3, 2, 1, stretch='lin')
plot(no_cl, add = T, col = 'red')
plot(cl_fi, add = T, col = 'darkred')

#plotRGB(r, 3, 4, 1, stretch='lin')
r3 <- subset(r2,1)
plot(r3, col = 'red', add = T, legend = F)

writeRaster(blue_filter_c, filename =  "/home/pi/PROBA_V/ProbaV_JD/rsdata/probav/cloudfilter/cloudfilter.tif")

# temporal outliers
blue_filter <- subset(out,10)
blue_filter <- b_vrt
blue_filter_c <- blue_filter
blue_filter_c[blue_filter != 2] <- NA 
blue_filter_c[blue_filter == 2] <- 1
plot(blue_filter_c, col = 'darkred', add = T, legend = F)

#plotRGB(b_vrt)
e <- drawExtent()

# or
# temp idea to reduce extent
#xmin <- 9.191336 # 4.177083 
#xmax <- 9.218927 # 5.582837 
#ymin <- 45.53769 # 50.55804
#ymax <- 45.55274 # 52.03919

xmin <- 5.996773 # 4.177083 
xmax <- 6.468209 # 5.582837 
ymin <- 51.80702 # 50.55804
ymax <- 52.43639 # 52.03919

e <- extent(c(xmin,xmax,ymin,ymax))

cr <- crop(x = b_vrt, y = e)
b_vrt <- cr
plot(b_vrt)

names(b_vrt) <- basename(df_probav_sm$fpath)
names(b_vrt) <- paste(probav_sm_dir, names(b_vrt), sep= "")
print(b_vrt)

plotRGB(b_vrt, 9, 8, 7, stretch='lin')


# --- get metrics ---  #
cat(sprintf("\nlayers: %i  | bands: %s  | blocks: %i  | cores: %i\n",
            nrow(df_probav_sm), paste0(bands, collapse = " "),
            blockSize(b_vrt, minrows = minrows)$n, mc.cores))

## time searies testing $$
#z <- zoo(c(b_vrt[10]), getZ(b_vrt))

# get single cell with dates preserved

z <- zoo(c(b_vrt[50000]), getZ(b_vrt))
plot(z)


f <- smoothLoess(tsx = z, QC_good=NULL,  res_type=c("all"), span=0.3)
ff <- smoothLoess(tsx = z, QC_good=NULL, dates=dates,thresholds=c(-80, Inf, -120, 120) , res_type= c("distance", "sd_distance", "all", "filled", "omit", "QC"), span=0.3)



#m <- matrix(x[1], nrow= length(bands), ncol=length(dates))


test <- calc(x = b_vrt, fun = smoothLoess, QC_good=NULL, dates=dates,thresholds=c(-80, Inf, -120, 120) , res_type=c("QC"), span=0.3 )

plot(f)
plot(f$x)


d <- getHarmMetrics(f$x,QC_good = f$QC_good ,dates = dates, sig = .95, order = 1)
round(d, digits = 3)

# No scale
b_metrics <- getHarmMetricsSpatial_JE(x = b_vrt, minrows = minrows, mc.cores = mc.cores, logfile=logfile,
                                      overwrite=T, span=0.3, scale_f = NULL,
                                      cf_bands = c(1,2), thresholds=c(-80, Inf, -120, 120),
                                      filename = out_name, df_probav_sm = df_probav_sm, order = 1, datatype="INT2S")




# Scaled
b_metrics <- getHarmMetricsSpatial_JE(x = b_vrt, minrows = minrows, mc.cores = mc.cores, logfile=logfile,
                                      overwrite=T, span=0.3, scale_f = c(10,100,10),
                                      cf_bands = c(1,2), thresholds=c(-80, Inf, -120, 120),
                                      filename = out_name, probav_sm_dir = probav_sm_dir, order = 1, datatype="INT2S")



# when running the fun using calc it works
tt <- calc(x = b_vrt, fun = fun)
dataType(tt) # "FLT4S"


# scale_f = c(10,100,10)
# thresholds=c(-80, Inf, -120, 120)
# datatype="FLT4S"
# datatype="INT2S"
r <- raster("/DATA/GEOTIFF/PROBAV_L3_S5_TOC_NDVI_100M/20150301/PROBAV_S5_TOC_20150301_100M_NDVI_V001/PROBAV_S5_TOC_X18Y02_20150301_100M_NDVI_V001_NDVI.tif")



summary(b_metrics)
b_metrics <- brick("/home/pi/PROBA_V/ProbaV_JD/rsdata/probav/metrics/X18Y02_harm_lm2_loess_03_scaled.envi") 


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
pnt_tsed <- readOGR(file.path(getwd(), "rsdata/ref_data"), "ref_Glc2ViirsStepNmoGeowikiGlob_point_africa", stringsAsFactors = F)
pnt_JD <- readOGR(file.path(getwd(), "rsdata/ref_data"), "Ref_dataJD", stringsAsFactors = F)
names(pnt_JD) <- c("id", "Description", "ID_nr", "x", "y", "z", "m")
pnt_JD[,-(4:7)]
pts=as.data.frame(pnt_JD)
coordinates(pts) <- ~x+y
projection(pts) <- proj4string(pnt_JD)
pnt_JD <- pts

# extract per tile ------------------#
tiles <- c("X18Y02") # ..., "X17Y06", "X18Y06", "X19Y06")

registerDoParallel(min(4, length(tiles)))
df_covs_tsed <- foreach(tile=tiles, .combine=rbind, .inorder = T) %dopar% {
  print(tile)
  b_metrics <- brick(paste0(getwd(), "/rsdata/probav/metrics/", tile,"_harm_lm2_loess_03_scaled.envi"))
  print("loaded")
  df_metrics_tile  <- extract(b_metrics, pnt_JD, cellnumbers=T, df=T)
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
  #rm(df_shape, df_stats, b_clumps, b_metrics)
  df_covs_tile
}

# create a df with the trainign classes & metrics
df_ref_tsed <- pnt_tsed@data[df_covs_tsed$ID,]
df_model_tsed <- cbind(G9_cl1=df_ref_tsed$G9_cl1, df_covs_tsed)
# good to save this!

# JD
df_ref_JD <- pnt_JD@data[df_covs_tsed$ID,]
df_model_JD <- cbind(ID_nr=df_ref_tsed$ID_nr, df_covs_tsed)
# good to save this!

#### ------------- ranger model ------------------------------------
# note that ranger doesn't run parallel on windows
require(ranger)
tiles <- c("X18Y02") #..., "X17Y06", "X18Y06", "X19Y06")

# exclude some classes
df_model_tsed <- subset(df_model_tsed, G9_cl1 <= 5)
# exclude NAs
cc <- complete.cases(df_model_JD)
table(df_model_JD$ID_nr[cc])
names(df_model_JD)

df_model_JD$LC <- factor(df_model_JD$ID_nr, labels = c("Forest","Agriculture", "Bare", "Water"))#, "Wetland", "Urban",  "Water")) c("Forest","Shrubland", "Grassland", "Cropland", "Bare")
table(df_model_JD$LC[cc])
glimpse(df_model_JD[cc, -(2:4)])

cat("------- ranger ---------")
ra_JD <- ranger(LC ~., df_model_JD[cc, -(1:4)], num.trees=500, write.forest=T,
                  probability = F, num.threads=10, verbose=T, importance = "impurity")
saveRDS(ra_JD, "data/models/ra_JD_merge5_x16.rds")
print(ra_JD)
#plot(ra_tsed)

ra_JD <-readRDS("data/models/ra_JD_merge5_x16.rds")
ra_JD$confusion.matrix

#### ------------- predict -------------------------------------------
# using ranger
model <- readRDS("data/models/ra_JD_merge5_x16.rds")
tiles <- c("X18Y02")

for (tile in tiles){
  print(paste0("--------------", tile, "-------------------"))
  # -------------------------------data----------------------------------#
  b_metrics <- brick(paste0(getwd(), "/rsdata/probav/metrics/", tile,"_harm_lm2_loess_03_scaled.envi"))
  # the follwoing lines are neede to extract manually spatial segments
  # b_clumps <- raster(paste0(data_path, "/rsdata/probav/test/seg/", tile,"_clumps60.img"), RAT=F)
  #  print('shape')
  #  df_shape <- read.csv(paste0(data_path, "/rsdata/probav/test/seg/", tile,"_clumps60_shape.csv"))
  #  print('stats')
  #  df_stats <- read.csv(paste0(data_path, "/rsdata/probav/test/seg/", tile,"_clumps60_stats.csv"))
  #  df_clumps <- cbind(df_stats[-1,-1], df_shape[,-1])
  
  # -------------------------------predict----------------------------------#
  print("---predict--------------")
  
  out_name <- paste0(getwd(), "/rsdata/probav/results/JD/pred_tsed_", tile,  ".tif")
  
  # this is apralle over mcCalc, additionally ranegr predict can use multiple threads
  # with larger datasets RAM becomes the bottleneck. Therefore it can eb better to use less
  # cores for calc and more for ranger
  
  pred_JD <- mcPredictSpatial(model,  b_metrics, b_clumps=NULL, df_clumps = NULL, type='response',
                                mc.cores = 5, ranger_threads = 1, minrows = 12, logfile = logfile,
                                datatype ="INT1U", of ="GTiff", out_name = out_name)
  
  print(pred_JD)
}

check <- raster("rsdata/probav/results/JD/pred_tsed_X18Y02.tif")
