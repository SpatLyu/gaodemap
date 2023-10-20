getCoordinate.core = function(address,city=NULL, 
                              output='json', formatted = F,
                              map_ak = ''){
  ### address
  if (any(grepl(' |#', address))) warning('address should not have blank character!')
  address = gsub(' |#', '', address)
  
  url_head = paste0('https://restapi.amap.com/v3/geocode/geo?address=', address)
  if (!is.null(city)) url_head = paste0(url_head, "&city=", city)
  url = paste0(url_head, "&output=", output, "&key=", map_ak)
  
  ### result
  result = tryCatch(getURL(url),error = function(e) {getURL(url, timeout = 200)})
  
  #cat(result)
  #cat("\n")
  
  if(length(address)==1)
    address<-iconv(URLdecode(address),"UTF-8","GBK")
  else{
    for(i in 1:length(address))
      address[i]<-iconv(URLdecode(address[i]),"UTF-8","GBK")
  }
  names(result) = address
  
  ### transform data from json/xml
  trans = function(x, out = output){
    if (out == 'xml') {
      rs <- gsub('.*?<location>([\\.,0-9]*)</location>.*', '\\1', x)
      coor<-unlist(str_split(rs,"[,]"))
    }else if (out == 'json'){
      rs <- gsub('.*?"location":"([\\.,0-9]*).*', '\\1', x)
      coor<-unlist(str_split(rs,"[,]"))
    }
    long = as.numeric(coor[1]); lat = as.numeric(coor[2])
    return(c("longtitude" = long, "latitude" = lat))
  }
  if (formatted) {
    if (length(result) > 1) {
      result = t(sapply(result, trans))
    } else {
      result = trans(result)
    }
  }
  
  ### final
  return(result)
}
#' Get coordiante from address. 
#' Take in address and return the coordinate
#' @param address address
#' @param city the city of the address, optional
#' @param output should be "json" or "xml", the type of the result
#' @param formatted logical value, return the coordinates or the original results
#' @param limit integer value.If the length of address exceeded limit, function will run in parallel
#' @param map_ak access key of GaoDe map service, you can set it through options(gaode.key='xxx')
#' @return A vector contains the  corresponding coordiante. If "formatted=TRUE", return the numeric coordinates, otherwise return json or xml type result, depents on the argument "output". If the length of address is larger than 1, the result is a matrix.
#' @importFrom stringr str_split
#' @importFrom RCurl getURL
#' @importFrom utils URLencode
#' @importFrom utils URLdecode
#' @export getCoordinate
#' @examples
#' \dontrun{ 
#' ## json output
#' getCoordinate('北京大学')
#' 
#' ## xml output
#' getCoordinate('北京大学', output='xml')
#' 
#' ## formatted
#' getCoordinate('北京大学', formatted = T)
#' 
#' ## vectorization, return a matrix
#' getCoordinate(c('北京大学', '清华大学'), formatted = T)
#' }
getCoordinate=function(address, city=NULL, output='json', formatted = F,limit=600, map_ak=''){
  #change encoding form GBK to UTF-8
  if(length(address)==1)
    address<-URLencode(iconv(address,"GBK","UTF-8"))
  else{
    for(i in 1:length(address))
      address[i]<-URLencode(iconv(address[i],"GBK","UTF-8"))
  }
  if (!is.null(city)) city<-URLencode(iconv(city,"GBK","UTF-8"))
  
  #cat(address)
  
  if (map_ak == '' && is.null(getOption('gaode.key'))){
    stop(Notification)
  }else{
    map_ak = ifelse(map_ak == '', getOption('gaode.key'), map_ak)
  }

  if(length(address)<limit){
    res<-getCoordinate.core(address, city, output , formatted, map_ak)
  }else if(require(parallel)){
    cl <- makeCluster(getOption("cl.cores", detectCores()*0.8))
    res<-parLapply(cl,X = address,fun = function(x){
      getCoordinate.core(x, city, output , formatted, map_ak)
    })
    res<-do.call('rbind',res)
    stopCluster(cl)
  }else{
    warning('can not run in parallel mode without package parallel')
    res<-getCoordinate.core(address, city, output , formatted, map_ak)
  } 
  res
}
