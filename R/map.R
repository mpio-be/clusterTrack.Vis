
#' @export
#' @examples
#' data(lbdo66867)
#' ctdf = as_ctdf(lbdo66867,time = "locationDate", crs = 4326, project_to = "+proj=eqearth")
#' slice_ctdf(ctdf, 48, 20, 1000000, 10)
#' clust = cluster_track(ctdf)
#' map(clust)

map <- function(clust) {

  if (Sys.info()[["sysname"]] == "Linux")  mapviewOptions(fgb = FALSE)

  m = mapview(map.types = c("CartoDB", "Esri.WorldImagery"))

  x = copy(clust)
  x[, cluster := factor(cluster)]
  p = st_as_sf(x[!is.na(cluster)])  
  
  tr = as_ctdf_track(x) |> setDT()
  tr[, let(segement = factor(.segment), filter = factor(.filter))]
  tr[(!.filter), lwd := 5]
  tr[(.filter), lwd := 3]
  tr = st_as_sf(tr)
  
  z = dplyr::filter(tr, .filter) |>  st_transform(4236) |> st_coordinates() 

  
  o =
    m +
    mapview(dplyr::filter(tr, !.filter), zcol = "filter", legend = FALSE) +
    mapview(p, zcol  = "cluster", layer.name = "cluster")
    

  o@map |>
  addAntpath(lng = z[,1],lat = z[,2],   color = "#7e7f81cc", weight = 2)



}
