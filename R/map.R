
#' Map a ctdf object
#'
#' Visualize clustered track data on an interactive map.
#'
#' @param ctdf A `ctdf` object.
#' @export
#' @examples
#' require(clusterTrack.Vis)
#' data(pesa56511)
#' ctdf = as_ctdf(pesa56511, time = "locationDate") |> cluster_track()
#' map(ctdf)

map <- function(ctdf, prop = 0.9) {

  clusterTrack:::.check_ctdf(ctdf)

  mapviewOptions(fgb = FALSE) 

  move_track = st_as_sf(ctdf[cluster ==0])  
  site_track = st_as_sf(ctdf[cluster != 0])  
  
  sites = summarise_ctdf(ctdf, prop = 0.9)
  sites[tenure < 1, Tenure := glue_data(.SD, "{round(tenure*24)}[h]")]
  sites[tenure > 1, Tenure := glue_data(.SD, "{round(tenure,1)}[d]")]
  sites[, lab := glue_data(
    .SD,
    "tenure:{Tenure}                      <br>
    start:{format(start, '%d-%b-%y %Hh')} <br>
    stop:{format(stop, '%d-%b-%y %Hh')}   <br>
    segments:{segments}                   <br>
    N:{N}"
  )]
    

  all_track = as_ctdf_track(ctdf) |> st_transform(crs = 4326) |> setDT()
  all_track[, let(segement = factor(.segment) )]
  all_track = st_as_sf(all_track)

  polys = sites[, .(cluster, site_poly)] |>st_as_sf()
  spsites = sites[, .(cluster, lab, geometry)] |>st_as_sf()

  o = 
    mapview(map.types = c("CartoDB", "Esri.WorldImagery")) +
    mapview(polys, zcol = "cluster", layer.name = "cluster", alpha = 0.2) +
    if(nrow(move_track)>0) mapview(move_track, color = "#7e7f81cc", cex = 3,  legend = FALSE) +
    mapview(all_track, legend = FALSE, color = "#7e7f81cc") +
    mapview(site_track, zcol = "cluster", layer.name = "cluster", legend = FALSE)

  clust_ico = awesomeIcons(
    icon       = NULL,
    text       = as.character(sites$cluster),
    iconColor  = "black",
    library    = "fa",
    markerColor = "white"
  )

  o@map |>
    addAwesomeMarkers(
      data = spsites,
      icon = clust_ico, 
      popup = ~lab
    )


}
