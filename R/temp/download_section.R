
## Data download

This example script uses data existing on the cloud toolbox and follows an existing directory structure. Downloading ProvaV data will not create that directory structure yet. Thus, this script will work when following the ProbaV directory structure on the cloud-toolbox/MEP, but not yet with downloading the data manually to a certain directory. But if you would like to know how to download the ProbaV data, below follows an example. 

*It is preferred to requests the datasets from vito through the MEP.*
  *Thus, the data download section can be skipped if you allready have the data!*
  
  ```{r}
# Select the location for which you wish to download the ProbaV  tiles for. 
x <- 10.00 # Longitude
y <- 8.00 # Latitude

# In order to download the correct tile numbers, we use coordinates to convert to the tile number
tile <- probaVTileFromCoords(x, y)
tile_str <- paste("\'*",tile,"*\'", sep ="")

```


To download data from the ProbaV vito website, we need to be a registered user (free). If you don't already have an account, feel free to make one by following this [link](http://www.vito-eodata.be/PDF/portal/Application.html#Home). Assign the username and password to a variable, which you can enter below.

```{r, results='hide'}
u_name <- readline("Type the username:")
p_word <- readline("Type the password:")
```
```{r}
# enter the file directory for the download location
dirloc <- paste(getwd(), '/2015/', sep = "")
# Create file directory if it does not exist yet
dir.create(file.path(dirloc), showWarnings = FALSE)
```


Use wget to download the data. wget has be passed directly in the terminal. Therefore, we pass the string to the system command. In this example, the link in the string will have to be adjusted manually to your request. The dates at the end of the http address below, allow you to download for a specific year, month and/or day. If you wish to download all ProbaV data for a specific year (e.g. 2016), then change the dates to '2016/?/?mode=tif'. The data will be downloaded into the directory set in the previous code (dirloc). For more examples on how to download ProbaV data on different operating systems, please follow this [link](http://www.vito-eodata.be/PDF/image/Data_pool_manual.pdf).

```{r}
wget_string <- paste('wget -A ', tile_str ,' .tif' , ' -P ', dirloc, ' --no-directories -r --user=', u_name,' --password=', p_word, ' http://www.vito-eodata.be/PDF/datapool/Free_Data/ProbaV_100m/S5_TOC_100_m/2016/2/1/?mode=tif', sep="")

# check for output of the download string
print(wget_string)
```
```{r, eval=FALSE}
# pass the string to the terminal directly from R.
system(wget_string)
```

