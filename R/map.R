
info_box <- function(items) {

  html_list = 
    sapply(items, function(item) glue::glue("<li>{item}</li>")) |>
    glue_collapse()
  html_list = glue::glue("<ul>{html_list}</ul>")


  htmltools::HTML(
    glue::glue(
    '<div class="modal" id="infobox" tabindex="-1" role="dialog">
        <div class="modal-dialog" role="document">
          <div class="modal-content">
            <div class="modal-body">
              {html_list}
            </div>
          </div>
        </div>
      </div>'
    )
  )
}

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

  if(nrow(ctdf[!is.na(cluster)])==0) stop("this ctdf does not have any clusters!")


  N = glue("<b>N</b>:<br>
            - fixes:{nrow(ctdf)} <br>
            - segments: {max(ctdf$.segment, na.rm = TRUE)} <br>
            - clusters: {max(ctdf$cluster)}")
  
  cluster_par = attr(ctdf, "cluster_params")
  cluster_par = glue(" - {names(cluster_par)} = {cluster_par}") |> paste(collapse = "<br>")
  cluster_par = glue("<b>Parameters</b>:<br>{cluster_par}")
  
  pack = glue("<i>clusterTrack v.{ packageVersion('clusterTrack')}</i>")

  nfo = c(N, cluster_par, pack)

  CD = st_as_sf(ctdf) |> st_transform(4326)
  CD = mutate(CD,
    pt_lab = glue("time: {format(timestamp, '%d-%b-%y %H:%M')} <br/> seg: {`.segment`}<b/r>")
  ) |>
    rowwise() |>
    mutate(pt_lab = htmltools::HTML(pt_lab) )

  move_points = dplyr::filter(CD, cluster ==0)  
  site_points = dplyr::filter(CD, cluster != 0)  
  
    
  sites = summarise_ctdf(ctdf, prop = prop)
  sites[tenure < 1, Tenure := glue_data(.SD, "{round(tenure*24)}[h]")]
  sites[tenure > 1, Tenure := glue_data(.SD, "{round(tenure,1)}[d]")]
  sites[, lab := glue_data(
    .SD,
    "tenure:{Tenure}                      <br/>
    start:{format(start, '%d-%b-%y %Hh')} <br/>
    stop:{format(stop, '%d-%b-%y %Hh')}   <br/>
    segments:{segments}                   <br/>
    N:{N}"
  )]

  all_track = as_ctdf_track(ctdf) |> st_transform(crs = 4326) |> setDT()
  all_track[, let(segement = factor(.segment) )]
  all_track = st_as_sf(all_track)

  polys  = sites[, .(cluster, site_poly)] |>st_as_sf()
  labels = sites[, .(cluster, lab, site_poly_center, stop)] |>st_as_sf()


  # map elements
    pal = colorFactor(viridis::viridis(unique(sites$cluster) |> length()), sites$cluster)


    clust_ico = awesomeIcons(
      icon       = NULL,
      text       = as.character(sites$cluster),
      iconColor  = "black",
      library    = "fa",
      markerColor = "white"
    )

  # build map
  leaflet() |>
    addTiles() |>
    addPolylines(
      data    = all_track,
      color   = "#7e7f81cc",
      weight  = 1.5,
      opacity = 0.5,
      group   = "All Track"
    ) |>
    addCircleMarkers(
      data        = move_points,
      label       = ~pt_lab,
      radius      = 4,
      color       =  "#7e7f81cc",
      stroke      = FALSE,
      opacity     = 0.3,
      fillOpacity = 1,
      group       = "Site Track"
    ) |>
    addPolygons(
      data        = polys,
      fillColor   = ~ pal(cluster),
      fillOpacity = 0.5,
      weight      = 0,
      group       = "Clusters"
    ) |>
    addCircleMarkers(
      data        = site_points,
      label       = ~pt_lab,
      radius      = 5,
      color       = ~ pal(cluster),
      stroke      = FALSE,
      fillOpacity = 1,
      group       = "Site Track"
    ) |>
    addAwesomeMarkers(
      data = labels,
      icon = clust_ico,
      popup = ~lab,
      group = "Cluster Icons",
      popupOptions = popupOptions(
        autoClose    = FALSE,
        keepInView   = TRUE,
        closeOnClick = FALSE
      )
    ) |>
    addTimeslider(
      data = labels,
      options = timesliderOptions(timeAttribute = "stop")
    ) |>
    addBootstrapDependency() |>
    addEasyButton(easyButton(
      icon    = '<i class="fa fa-dot-circle-o" style="color:red; font-weight:bold;"></i>',
      title   = "clusterTrack",
      onClick = JS("function(btn, map){ $('#infobox').modal('show'); }")
    )) |>
    htmlwidgets::appendContent(
      info_box(nfo)
    )






}
