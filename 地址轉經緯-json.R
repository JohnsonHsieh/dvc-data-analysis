library(jsonlite)

#Google map 地址轉經緯度
geoPoint = function(address, verbose=FALSE) {
  #若未輸入地址, return錯誤
  if(verbose) cat(address,"\n")
  root = "http://maps.google.com/maps/api/geocode/"
  #Google編碼為UTF8, 中文要轉碼後帶入URL才會正確
  # address = iconv(address,"big5","utf8")
  #POI API型態(XML or JSON)
  # return.call = "xml"
  return.call = "json"
  sensor = "false"
  language = "zh-TW"
  #產生URL
  url_gen = paste(root, return.call, "?address=", address, "&sensor=", sensor, "&language=", language, sep = "")
  #擷取網頁原始碼
  html_code = URLencode(url_gen) %>% fromJSON
  #若status為OK抓取資料, 若不為OK return status
  if(html_code$status=="OK"){
    location <- html_code$results$geometry$location
    loc_type <- html_code$results$geometry$location_type
    address <- html_code$results$formatted_address
    long_name <- html_code$results$address_components[[1]]$long_name
    return(cbind(location, loc_type, address, vname=paste0(long_name[4],long_name[3])))
  } else {
    return(paste("Status:", html_code$status, sep = " "))
  }
}