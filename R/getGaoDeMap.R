#' Get static map from GaoDe API
#' 
#' Take in location and return a ggmap object.
#' 
#' @param location a vector a or matrix contains longtitude and latitude of the center of the map, or a character refers to the address
#' @param city city of location
#' @param width width of the map
#' @param height height of the map
#' @param zoom map zoom, an integer from 1 (continent) to 17 (building), default value 10 (city)
#' @param scale multiplicative factor for the number of pixels returned. possible values are 2 or anything else.
#' @param color "color" or "bw", color or black-and-white
#' @param messaging logical. whether to print the download messages.
#' @param coordtype coordiantion type.
#' @param zoomplus map zoom plus, a real number for fine turning zoom in further ggplot commands
#' @return A ggmap object. a map image as a 2d-array of colors as hexadecimal strings representing pixel fill values.
#' @export getGaoDeMap
#' @importFrom png readPNG
#' @importFrom RgoogleMaps XY2LatLon
#' @importFrom ggmap ggmap
#' @importFrom utils URLencode
#' @examples
#' 
#' \dontrun{
#' library(ggmap)  
#' ## Beijing
#' p <- getGaoDeMap(c(116.39565, 39.92999))
#' ggmap(p)
#' 
#' p <- getGaoDeMap('beijing') # the same
#' ggmap(p)
#' 
#' ## black-and-white
#' p <- getGaoDeMap(color='bw')
#' ggmap(p)
#' 
#' ## do not print messages
#' p <- getGaoDeMap(messaging = F)
#' }
getGaoDeMap = function(location, city=NULL, width=400, height = 400, zoom=10, 
                       scale=2, color = "color",messaging = TRUE, map_ak = '',zoomplus=0){
  #iconv(URLdecode(target),"UTF-8","GBK")
  location<-URLencode(iconv(location,"GBK","UTF-8"))
  if (!is.null(city)) city<-URLencode(iconv(city,"GBK","UTF-8"))
  
  if (map_ak == '' && is.null(getOption('gaode.key'))){
    stop(Notification)
  }else{
    map_ak = ifelse(map_ak == '', getOption('gaode.key'), map_ak)
  }
  ## location
  if (is.character(location) && length(location) == 1){
    location_cor = getCoordinate(location, city=city, formatted=T)
  } else if (length(location == 2)){
    location_cor = location
  } else{
    stop('Wrong address!')
  }
  lon = location_cor[1];
  lat = location_cor[2];
  
  ## set url
  url_head = "https://restapi.amap.com/v3/staticmap?"
  url = paste0(url_head, "size=", width, "*", height, "&location=",
               lon, ",", lat, "&zoom=", zoom,"&key=",map_ak)
  if (scale == 2) url = paste0(url, "&scale=2")
  
  #cat(url)
  #cat("\n")
  
  wd<-getwd()
  destfile = paste0(wd,"/",lon, ";", lat, ".png")
  
  download.file(url, destfile = destfile, 
                quiet = !messaging, mode = "wb")
  if (messaging) message(paste0("Map from URL : ", url))
  
  ## read image and transform to ggmap obejct 
  map = readPNG(destfile)
  # format file
  if(color == "color"){
    map <- apply(map, 2, rgb)
  } else if(color == "bw"){
    mapd <- dim(map)
    map <- gray(.30 * map[,,1] + .59 * map[,,2] + .11 * map[,,3])
    dim(map) <- mapd[1:2]
  }
  class(map) <- c("ggmap","raster")
  
  # map spatial info
  ll <- XY2LatLon(
    list(lat = lat, lon = lon, zoom = zoom+zoomplus),
    -width/2 + 0.5,
    -height/2 - 0.5
  )
  ur <- XY2LatLon(
    list(lat = lat, lon = lon, zoom = zoom+zoomplus),
    width/2 + 0.5,
    height/2 - 0.5
  )
  
  attr(map, "bb") <- data.frame(
    ll.lat = ll[1], ll.lon = ll[2],
    ur.lat = ur[1], ur.lon = ur[2]
  )
  
  #delete the temp file
  unlink(destfile)
  
  # transpose
  out <- t(map)
  out
}


#' Change the zoom seting of a ggmap object.
#' 
#' Take in a ggmap object and set it's zoom(ll,ur coordinate).
#' 
#' @param map ggmap object to change zoom
#' @param location a vector a or matrix contains longtitude and latitude of the center of the map.
#' @param width width of the map
#' @param height height of the map
#' @param zoom map zoom, an integer from 3 (continent) to 21 (building), default value 10 (city)
#' @return A ggmap object. 
#' @export changeZoom
#' @importFrom RgoogleMaps XY2LatLon
#' @importFrom ggmap ggmap

changeZoom<- function(map,location,width,height,zoom){
  lon<-location[1]
  lat<-location[2]
  
  # map spatial info
  ll <- XY2LatLon(
    list(lat = lat, lon = lon, zoom = zoom),
    -width/2 + 0.5,
    -height/2 - 0.5
  )
  ur <- XY2LatLon(
    list(lat = lat, lon = lon, zoom = zoom),
    width/2 + 0.5,
    height/2 - 0.5
  )
  
  attr(map, "bb") <- data.frame(
    ll.lat = ll[1], ll.lon = ll[2],
    ur.lat = ur[1], ur.lon = ur[2]
  )
  return (map)
}


