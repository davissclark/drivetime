getTimeZone <- function(lat, lng, day = "2017-08-23 00:00", key = "AIzaSyC7Z1DIvtio8Fpm2s3ntBmChiKKFwhnZ8g") {
  seconds <- as.numeric(ymd_hm(day) - ymd_hm("1970-1-1 00:00"))
  url <- URLencode(sprintf("https://maps.googleapis.com/maps/api/timezone/json?location=%s,%s&timestamp=%s&key=%s", lat, lng, seconds, key))
  fromJSON(getURL(url))
}
