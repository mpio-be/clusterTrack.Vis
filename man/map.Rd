% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map.R
\name{map}
\alias{map}
\title{Map a ctdf object}
\usage{
map(ctdf, prop = 0.9)
}
\arguments{
\item{ctdf}{A \code{ctdf} object.}
}
\description{
Visualize clustered track data on an interactive map.
}
\examples{
require(clusterTrack.Vis)
data(pesa56511)
ctdf = as_ctdf(pesa56511, time = "locationDate") |> cluster_track()
map(ctdf)
}
