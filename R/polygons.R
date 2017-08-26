find.max.lat <- function(lat, lng, model, minutes, day, time) {
  origin <- paste(lat, lng, sep = ",")
  north <- lat + 1
  repeat {
    d <- getGoogleDist(origin,
                       paste(north, lng, sep = ","),
                       model = model,
                       day, time,
                       latlng = TRUE)$rows[[1]]$elements[[1]]$duration_in_traffic$value / 60
    north <- north - .01
    if (length(d) == 0) {
      if (north < lat) {
        break
      }
      next
    } else if (d < minutes) {
      break
    }
  }
  c(lng, north)
}

find.min.lat <- function(lat, lng, model, minutes, day, time) {
  origin <- paste(lat, lng, sep = ",")
  south <- lat - 1
  repeat {
    d <- getGoogleDist(origin,
                       paste(south, lng, sep = ","),
                       model = model,
                       day, time,
                       latlng = TRUE)$rows[[1]]$elements[[1]]$duration_in_traffic$value / 60
    south <- south + .01
    if (length(d) == 0) {
      if (south > lat) {
        break
      }
      next
    } else if (d < minutes) {
      break
    }
  }
  c(lng, south)
}

find.max.lng <- function(lat, lng, model, minutes, day, time) {
  origin <- paste(lat, lng, sep = ",")
  east <- lng + 1
  repeat {
    d <- getGoogleDist(origin,
                       paste(lat, east, sep = ","),
                       model = model,
                       day, time,
                       latlng = TRUE)$rows[[1]]$elements[[1]]$duration_in_traffic$value / 60
    east <- east - .01
    if (length(d) == 0) {
      if (east < lng) {
        break
      }
      next
    } else if (d < minutes) {
      break
    }
  }
  c(east, lat)
}

find.min.lng <- function(lat, lng, model, minutes, day, time) {
  origin <- paste(lat, lng, sep = ",")
  west <- lng - 1
  repeat {
    d <- getGoogleDist(origin,
                       paste(lat, west, sep = ","),
                       model = model,
                       day, time,
                       latlng = TRUE)$rows[[1]]$elements[[1]]$duration_in_traffic$value / 60
    west <- west + .01
    if (length(d) == 0) {
      if (west > lng) {
        break
      }
      next
    } else if (d < minutes) {
      break
    }
  }
  c(west, lat)
}

# Two-column numeric matrix; the first column is longitude and the second is
# latitude. Polygons are separated by rows of (NA, NA). It is not possible to
# represent multi-polygons nor polygons with holes using this method; use
# SpatialPolygons instead.
