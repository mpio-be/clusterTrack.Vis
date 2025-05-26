
#' @export
#' @examples
#' data(lbdo66867)
#' ctdf = as_ctdf(lbdo66867,time = "locationDate", crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf, 48, 20, 1000000, 10)
#' clust = cluster_track(ctdf)


map <- function(clust) {

  # mapviewOptions(fgb = FALSE)
  m = mapview(map.types = c("CartoDB", "Esri.WorldImagery"))

  x = copy(clust)
  x[, cluster := factor(cluster)]
  p = st_as_sf(x[!is.na(cluster)])  
  


  tr = as_ctdf_track(x) |> setDT()
  tr[, let(segement = factor(.segment), filter = factor(.filter))]
  tr[(!.filter), lwd := 5]
  tr[(.filter), lwd := 3]
  tr = st_as_sf(tr)
  


  m +
  mapview(tr, zcol = "filter" ) +
  mapview(p, zcol  = "cluster")

  






}
