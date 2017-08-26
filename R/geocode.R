#' Get lat / lng coordinates from the Google Maps Geocode API.
#'
#' @param address An address string
#' @param key A rgeistered Google Maps Geocode API key.
#' @return A set of latitude / longitude coordinates associated with a requested address.
geocode <- function(address, key = "AIzaSyCp3l9A5PKPVupslJqOFmO55kg50JYd85Q") {
  address <- gsub("[\\s]", "+", address, perl = TRUE)
  url <- sprintf("https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=%s", address, key)
  r <- rjson::fromJSON(getURL(url))
  if (r$status == "OK") {
    lat <- r$results[[1]]$geometry$location$lat
    lng <- r$results[[1]]$geometry$location$lng
    paste(lat, lng, sep = ",")
  }
}

#' Get address from the Google Maps Geocode API.
#'
#' @param lat A latitude coordinate
#' @param lng A longitude coordinate
#' @param key A rgeistered Google Maps Geocode API key.
#' @return An address associated with a requested set of location coordinates.
rev.geocode <- function(lat, lng, key = "AIzaSyCp3l9A5PKPVupslJqOFmO55kg50JYd85Q") {
  latlng <- paste(lat, lng, sep = ",")
  url <- sprintf("https://maps.googleapis.com/maps/api/geocode/json?latlng=%s&key=%s", latlng, key)
  r <- rjson::fromJSON(getURL(url))
  if (r$status == "OK") {
    re <- r$results[[1]]
  }
  list(city.long = r$results[[1]]$address_components[[3]]$long_name,
       city.short = r$results[[1]]$address_components[[3]]$short_name,
       county.long = r$results[[1]]$address_components[[4]]$long_name,
       county.short = r$results[[1]]$address_components[[4]]$short_name,
       state.long = r$results[[1]]$address_components[[5]]$long_name,
       state.short = r$results[[1]]$address_components[[5]]$short_name,
       address = r$results[[1]]$formatted_address)
}
