
.sitepoly <- function(x, method = "mcp", p = 0.9) {

  clusterTrack:::.mcp(x, p)

}

#' Extended summary of a ctdf
#'
#' Extends the clusterTrack::summary(ctdf)
#'
#' @param ctdf A `ctdf` object.
#' @export
#' @examples
#' data(pesa56511)
#' ctdf = as_ctdf(pesa56511, time = "locationDate") |> cluster_track()
#' 
#' summarise_ctdf(ctdf)

summarise_ctdf <- function(ctdf, site_poly = "mcp", prop = 0.9) {

  x = summary(ctdf) |>
    st_as_sf() |>
    st_transform(crs = 4326) |>
    data.table()
  
  polys = ctdf[cluster > 0, .(
    site_poly = .sitepoly(location, method = site_poly, p = prop) |>
      st_transform(crs = 4326)
  ), by = cluster]

  polys[, site_poly_center := st_centroid(polys$site_poly)]

  o = merge(x, polys, by = "cluster")
  o[, cluster := factor(cluster)]
  o



}