
#' @import mapview leaflet glue ggplot2
NULL

#' Map a ctdf object
#'
#' Visualize clustered track data on an interactive map.
#'
#' @param ctdf A `ctdf` object.
#' @export
#' @examples
#' data(pesa56511)
#' ctdf  = as_ctdf(pesa56511, time = "locationDate", s_srs = 4326, t_srs = "+proj=eqearth") |>cluster_track()
#' map(ctdf)

map <- function(ctdf) {

  .check_ctdf(ctdf)

  if (Sys.info()[["sysname"]] == "Linux")  mapviewOptions(fgb = FALSE) # 'cause Chrome sucks. 

  x            = copy(ctdf)
  clus         = st_as_sf(x[cluster>0])  
  nonclus      = st_as_sf(x[cluster ==0])  
  x[, cluster := factor(cluster)]

  cluster_centroids = data.table(clus)
  cluster_centroids =
    cluster_centroids[, .(
      start    = min(timestamp),
      stop     = max(timestamp),
      tenure   = difftime(max(timestamp), min(timestamp), units = "days") |> as.numeric(),
      geometry = st_union(location) |> st_convex_hull() |> st_centroid(),
      segment  = unique(.segment),
      N        = .N
    ), by = cluster]
  cluster_centroids[tenure < 1, Tenure := glue_data(.SD, "{round(tenure*24)}[h]")]
  cluster_centroids[tenure > 1, Tenure := glue_data(.SD, "{round(tenure,1)}[d]")]
  
  cluster_centroids[, lab := glue_data(
    .SD,
    "tenure:{Tenure}                     <br>
    start:{format(start, '%d-%b-%y %Hh')} <br>
    stop:{format(stop, '%d-%b-%y %Hh')}   <br>
    segment:{segment}                    <br>
    N:{N}"
  )]
    
  cluster_centroids = 
    cluster_centroids |>
    st_as_sf() |>
    st_transform(4326)
    

  tr = as_ctdf_track(x) |> setDT()
  tr[, let(segement = factor(.segment) )]
  tr = st_as_sf(tr)

  polys = ctdf[cluster > 0, .(hull = .mcp (location, p = 1) ), by = cluster] |>
    st_as_sf()


  o = 
    mapview(map.types = c("CartoDB", "Esri.WorldImagery")) +
    mapview(polys, zcol = "cluster", layer.name = "cluster", alpha = 0.2) +
    if(nrow(nonclus)>0) mapview(nonclus, color = "#7e7f81cc", cex = 3,  legend = FALSE) +
    mapview(tr, legend = FALSE, color = "#7e7f81cc") +
    mapview(clus, zcol = "cluster", layer.name = "cluster")

  clust_ico = awesomeIcons(
    icon       = NULL,
    text       = as.character(cluster_centroids$cluster),
    iconColor  = "black",
    library    = "fa",
    markerColor = "white"
  )

  o@map |>
    addAwesomeMarkers(
      data = cluster_centroids,
      icon = clust_ico, 
      popup = ~lab
    )


}
