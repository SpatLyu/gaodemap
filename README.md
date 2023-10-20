gaodemap
========

R interface of gaode map api，just like ggmap but get map from gaode api instead of google or openstreet.

## Installation
```
library(devtools)
install_github('icejean/gaodemap')

If you get a message like this:
Downloading GitHub repo icejean/gaodemap@master
Error in utils::download.file(url, path, method = download_method(), quiet = quiet,  : 
  cannot open URL 'https://api.github.com/repos/icejean/gaodemap/tarball/master'
  
Just copy and paste the URL to the browser and download the ziped source file,
icejean-gaodemap-8de1771.tar.gz, for exmaple,and install it from source:
install.packages("D:/R/Rsources/icejean-gaodemap-8de1771.tar.gz",repos = NULL,type="source",INSTALL_opts = "--no-multiarch")

```

## Usage

Apply an application from [lbs.amap.com](https://lbs.amap.com/api/webservice/guide/create-project/get-key). Then register you key here.
```
library(gaodemap)
options(gaode.key = 'fill your key here XXX')
```


### Function: getLocation
Get location from coordinates data.
```
lon = matrix(c(117.93780, 24.55730, 117.93291, 24.57745, 117.23530, 24.64210, 117.05890, 24.74860), byrow=T, ncol=2)
### json 
location_json = getLocation(lon[1,], output='json')

### xml
location_xml = getLocation(lon[1, ], output='xml')

## formatted
location = getLocation(lon, formatted = T) 
```

### Function: getCoordinate
Given a address, return the corresponding coordinates
```
getCoordinate('北京大学') # json
getCoordinate('北京大学', output='xml') # xml
getCoordinate('北京大学', formatted = T) # character
getCoordinate(c('北京大学', '清华大学'), formatted = T) # matrix
```


### Function: getGaoDeMap

```
p <- getGaoDeMap(c(lon=116.354431, lat=39.942333))
library(ggmap)
ggmap(p)
```

### Function: geoconv

Convert your coordinate data to gaodemap's coordinate system. Document: https://lbs.amap.com/api/webservice/guide/api/convert

## Example

```
library(gaodemap)
library(ggplot2)
options(gaode.key='xxx')
ruc_map = getGaoDeMap('中国人民大学', zoom=12)
ruc_coordinate = getCoordinate('中国人民大学', formatted = T)
ruc_coordinate = data.frame(t(ruc_coordinate))
ggmap::ggmap(ruc_map) +
  geom_point(aes(x=longtitude, y=latitude), data=ruc_coordinate, col='red', size=5)
```
There's a full example R script in the ./test directory showing the usage of all 8 functions.
Please read it for detail.