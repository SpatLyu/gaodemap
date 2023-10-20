#' Convert geocode
#' Take in geocode from the other source to GaoDe's geocode
#' @param geocde geocode from the other source
#' @param coordsys takes strings from "gps";"mapbar";"baidu";"autonavi"(no change). See more in details. 
#' @details Go to https://lbs.amap.com/api/webservice/guide/api/convert to read more.
#' @importFrom rjson fromJSON
#' @importFrom RCurl getURL
#' @importFrom stringr str_split
#' @export geoconv
geoconv = function(geocode, coordsys='gps', map_ak=''){
  if (map_ak == '' && is.null(getOption('gaode.key'))){
    stop(Notification)
  }else{
    map_ak = ifelse(map_ak == '', getOption('gaode.key'), map_ak)
  }
  if (class(geocode) %in% c('data.frame', 'matrix')){
    geocode = as.matrix(geocode)
    code = apply(geocode, 1, function(x) paste0(x[1], ',', x[2]))
    code_url = paste0(code, ';', collapse = '')
    code_url = substr(code_url, 1, nchar(code_url)-1)
  } else if(length(geocode) == 2){
    code_url = paste0(geocode[1], ',', geocode[2])
  } else{
    stop('Wrong geocodes!')
  }
  
  url_header = 'https://restapi.amap.com/v3/assistant/coordinate/convert?locations='
  url = paste0(url_header, code_url, '&coordsys=', coordsys, '&key=', map_ak, '&output=json',
               collapse='')
  #cat(url);cat("\n")
  result <- fromJSON(getURL(url))
  locations<-str_split(result$locations,";")
  locations<-as.vector(unlist(locations))
  locations<-str_split(locations,",")
  locations<-matrix(as.numeric(unlist(locations)),byrow=T, ncol=2)
  return (locations)
}

