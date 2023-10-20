#' Get edge coordinates of a given district.
#' Take in a district name and return a dataframe of coordinates of the edge of the district.
#' @param target the name of the district.
#' @param id the id of the district
#' @details Go to https://lbs.amap.com/api/webservice/guide/api/district to read more.
#' @importFrom rjson fromJSON
#' @importFrom RCurl getURL
#' @importFrom stringr str_split
#' @export getRegionCoordinates

getRegionCoordinates <- function(target, ak, id="1") {
  #iconv(URLdecode(target),"UTF-8","GBK")
  target<-URLencode(iconv(target,"GBK","UTF-8"))
  
  url <-
    paste(
      "https://restapi.amap.com/v3/config/district?keywords=",
      target,
      "&subdistrict=0&extensions=all&key=",
      ak,
      sep = ""
    )
  
  border<-getURL(url)
  rs <- fromJSON(border)
  borders<-unlist(str_split(unlist(rs$districts)["polyline"],"[|]"))
  for(i in 1:length(borders)){
    #i<-1
    coords<-unlist(str_split(borders[i],";"))
    coords2<-matrix(data=NA, nrow = length(coords), ncol = 2, byrow = TRUE, dimnames = NULL)
    for(j in 1:length(coords)){
      #j<-1
      coor<-unlist(str_split(coords[j],","))
      coords2[j,1]<-as.numeric(coor[1])
      coords2[j,2]<-as.numeric(coor[2])
    }
    coords2<-as.data.frame(coords2)
    names(coords2)<-c("lon","lat")
    coords2$order<-rownames(coords2)
    coords2$hole<-FALSE
    coords2$piece<-i
    coords2$id<-id
    coords2$group<-paste(id,".",i,sep="")
    if(i==1){coords3<-coords2}
    else{ coords3<-rbind(coords3,coords2)}
  }
  return (coords3)
}

