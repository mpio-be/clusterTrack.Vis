
#' @export
#' @examples
#' data(lbdo66867)
#' ctdf = as_ctdf(lbdo66867,time = "locationDate", crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf)
#' clust = cluster_track(ctdf)
#' map(clust)

map <- function(clust) {

  if (Sys.info()[["sysname"]] == "Linux")  mapviewOptions(fgb = FALSE)

  x = copy(clust)
  clus   = st_as_sf(x[cluster>0])  
  nonclus = st_as_sf(x[cluster ==0])  
  x[, cluster := factor(cluster)]
  
  cluster_centroids = data.table(clus)
  cluster_centroids =
    cluster_centroids[, .(
      start = min(timestamp),
      stop  = max(timestamp),
      tenure   = difftime(max(timestamp), min(timestamp), units = "days"),
      geometry = st_union(location) |> st_convex_hull() |> st_centroid(),
      segment  = unique(.segment),
      N        = .N

    ), by = cluster]
  cluster_centroids[, lab := glue_data(
    .SD,
    "tenure:{round(tenure,1)}[d]      <br>
    start:{format(start, '%d-%b-%y')} <br>
    stop:{format(stop, '%d-%b-%y')}   <br>
    segment:{segment}                 <br>
    N:{N}"
  )]
    
  cluster_centroids = 
    cluster_centroids |>
    st_as_sf() |>
    st_transform(4326)
    
    


  tr = as_ctdf_track(x) |> setDT()
  tr[, let(segement = factor(.segment), filter = factor(.filter))]
  tr[(!.filter), lwd := 5]
  tr[(.filter),  lwd := 3]
  tr = st_as_sf(tr)
  
  trf = dplyr::filter(tr, .filter) 
  trnf = dplyr::filter(tr, !.filter) 



  o = mapview(map.types = c("CartoDB", "Esri.WorldImagery")) +
    mapview(nonclus, color = "#7e7f81cc", cex = 1, legend = FALSE) +
    mapview(trf, legend = FALSE, color = "#7e7f81cc") +
    mapview(trnf, legend = FALSE, color = "#df3112cc", lwd = 3) +
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
