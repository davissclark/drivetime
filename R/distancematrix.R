#' Get drive time from Distance Matrix API.
#'
#' @param start A start address or lat,lng coordinate set.
#' @param end An end address or lat,lng coordinate set.
#' @param model A distance matrix API traffic model.
#' @param day The day on which to base drive time estimates.
#' @param time The time of day on which to base drive time estimates.
#' @param latlng Binary indicating whether start/end are lat,lng coordinates.
#' @param key A rgeistered Google Maps Distance Matrix API key.
#' @return A Google Maps drive time estimate in minutes.
getGoogleDist <- function(start, end, model, day, time,
                          latlng = FALSE,
                          key = "AIzaSyCp3l9A5PKPVupslJqOFmO55kg50JYd85Q") {

  departure = sprintf("%s %s", day, time)

  slat <- as.numeric(unlist(strsplit(start, split = ",")))[1]
  slng <- as.numeric(unlist(strsplit(start, split = ",")))[2]

  tzinfo <- getTimeZone(slat, slng, departure)

  seconds <- as.numeric(ymd_hm(departure) - ymd_hm("1970-1-1 00:00")) * 24 * 60 * 60
  seconds <- seconds - (tzinfo$dstOffset + tzinfo$rawOffset)

  if (latlng == FALSE) {
    start <- encode_address(start)
    end <- encode_address(end)
  }
  url <- sprintf("https://maps.googleapis.com/maps/api/distancematrix/json?mode=driving&origins=%s&destinations=%s&departure_time=%s&traffic_model=%s&key=%s", start, end, seconds, model, key)
  fromJSON(getURL(url))
}
