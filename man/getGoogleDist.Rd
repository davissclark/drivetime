% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distancematrix.R
\name{getGoogleDist}
\alias{getGoogleDist}
\title{Get drive time from Distance Matrix API.}
\usage{
getGoogleDist(start, end, model, day, time, latlng = FALSE,
  key = "AIzaSyCp3l9A5PKPVupslJqOFmO55kg50JYd85Q")
}
\arguments{
\item{start}{A start address or lat,lng coordinate set.}

\item{end}{An end address or lat,lng coordinate set.}

\item{model}{A distance matrix API traffic model.}

\item{day}{The day on which to base drive time estimates.}

\item{time}{The time of day on which to base drive time estimates.}

\item{latlng}{Binary indicating whether start/end are lat,lng coordinates.}

\item{key}{A rgeistered Google Maps Distance Matrix API key.}
}
\value{
A Google Maps drive time estimate in minutes.
}
\description{
Get drive time from Distance Matrix API.
}
