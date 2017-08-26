encode_address <- function(address) {
  address <- gsub(",", " ", address)
  URLencode(gsub("\\s+", "+", gsub("(#[0-9a-zA-Z]{1,})", "", address, perl = TRUE)))
}

normalizeZips <- function(zips) {
  ifelse(nchar(zips) == 3,
         paste0("00", zips),
         ifelse(nchar(zips) == 4,
                paste0("0", zips),
                ifelse(nchar(zips) > 5,
                       substr(zips, 1, 5),
                       as.character(zips))
         )
  )
}

flatten_zip <- function(l) {
  df <- data.frame(
    branch = vector(length = length(l)),
    branchaddress = vector(length = length(l)),
    address = vector(length = length(l)),
    gend = vector(length = length(l)),
    minutes = vector(length = length(l)),
    miles = vector(length = length(l))
  )
  for(i in 1:length(l)) {
    v <- c("branch", "branchaddress", "address", "gend", "minutes", "miles")
    sapply(v, function(x) df[i, x] <<- l[[i]][[x]])
  }
  df
}
