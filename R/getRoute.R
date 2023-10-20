#' Get route from query
#' Take in a original location and destination, return the direction
#' @param origin the original location
#' @param destination the destination
#' @param mode 'driving'(default), 'walking', or 'transit','bicycling','truck'.
#' @param city the city of of original location and destination. 
#' @param strategy integer from 0 to 20 for path selecting, default 9 for driving mode.
#' @details Go to https://lbs.amap.com/api/webservice/guide/api/direction to read more.
#' @param size for truck mode.
#' @return a data frame contains longtitude and latitude of the route.
#' @export getRoute
#' @importFrom RCurl getForm
#' @importFrom XML htmlTreeParse　xpathSApply xmlValue
#' @importFrom rjson fromJSON
#' 
#' @examples
#' \dontrun{
#' bjMap = getBaiduMap('北京', color='bw')
#' df = getRoute('首都国际机场', '北京南苑机场', region = '北京')
#' ggmap(bjMap) + geom_path(data = df, aes(lon, lat), alpha = 0.5, col = 'red')
#' }
#' 

getRoute = function(origin, destination, mode='driving', city=NULL,
                    strategy = 9, size=2,
                    output = 'xml',
                    map_ak=''){
  
  if(!is.null(city))city<-URLencode(iconv(city,"GBK","UTF-8"))
  
  rawData <- getRouteXML(origin, destination, mode,city, strategy, size, output, map_ak)
  if(mode=="bicycling" || mode=="truck") return(json2df(rawData))
  else return(xml2df(rawData))
}

getRouteXML = function(origin, destination, mode, city,
                       strategy , size,
                       output ,
                       map_ak){
  if (map_ak == '' && is.null(getOption('gaode.key'))){
    stop(Notification)
  }else{
    map_ak = ifelse(map_ak == '', getOption('gaode.key'), map_ak)
  }
  
  ## get xml data
  if(mode=="driving"){
    serverAddress = 'https://restapi.amap.com/v3/direction/driving?'
  }else if(mode=="walking"){
    serverAddress = 'https://restapi.amap.com/v3/direction/walking?'
  }else if(mode=="transit"){
    serverAddress = 'https://restapi.amap.com/v3/direction/transit/integrated?'
    if(strategy>5) strategy=0
  }else if(mode=="bicycling"){
    serverAddress = 'https://restapi.amap.com/v4/direction/bicycling?'
  }else if(mode=="truck"){
    serverAddress = 'https://restapi.amap.com/v4/direction/truck?'
  }else{
    stop("Not supported mode!")
  }
  
  # rawData = getForm(serverAddress,  
  #                   origin = origin, destination = destination, 
  #                   strategy = strategy, city=city, size=size,
  #                   key = map_ak, output = "xml")
  url = paste0(serverAddress,"key=", map_ak, "&origin=",origin,"&destination=",destination,
               "&strategy=",strategy,"&city=",city,"&size=",size,"&output=",output)
  cat(url);cat("\n")
  rawData =getURL(url)
  
  return(rawData)
}

xml2df = function(rawData){
  ## extract longitude and latitude
  tree = htmlTreeParse(rawData, useInternal = TRUE)
  path <- xpathSApply(tree, "//polyline",  xmlValue)
  split_path = function(x){
    xVec = strsplit(x, ';')[[1]]
    xMat = sapply(xVec, function(x) as.numeric(strsplit(x, ',')[[1]]))
    xDf = data.frame(t(xMat), row.names = NULL)
    colnames(xDf) = c('lon', 'lat')
    return(xDf)
  }
  coor_list = lapply(path, split_path)
  ## return a dataframe
  coors = do.call(rbind, coor_list)
  return(coors)
}

json2df =function(rawData){
  rs<-fromJSON(rawData)
  
  paths<-rs$data$paths[[1]]$steps
  if(is.null(paths))
    paths<-rs$data[[1]]$paths[[1]]$steps
  
  path<-list()
  length(path)<-length(paths)
  for(i in 1:length(paths)){
    path[i]<-paths[[i]]$polyline
  }
  split_path = function(x){
    xVec = strsplit(x, ';')[[1]]
    xMat = sapply(xVec, function(x) as.numeric(strsplit(x, ',')[[1]]))
    xDf = data.frame(t(xMat), row.names = NULL)
    colnames(xDf) = c('lon', 'lat')
    return(xDf)
  }
  coor_list = lapply(path, split_path)
  ## return a dataframe
  coors = do.call(rbind, coor_list)
  return(coors)
}

