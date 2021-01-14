emodnet_map_plot_2<-function (data, fill = NULL, title = NULL, subtitle = NULL, legend = NULL, 
                              crs = 3035, xlim = c(2426378.0132, 7093974.6215), ylim = c(1308101.2618, 
                                                                                         5446513.5222), direction = 1, option = "viridis", 
                              zoom = FALSE, ...) 
{
  stopifnot(class(data)[1] %in% c("sf", "RasterLayer", 
                                  "SpatialPolygonsDataFrame", "SpatialPointsDataFrame", 
                                  "SpatialLinesDataFrame"))
  emodnet_map_basic <- emodnet_map_basic(...) + ggplot2::ggtitle(label = title, 
                                                                 subtitle = subtitle)
  if (class(data)[1] == "RasterLayer") {
    message("Transforming RasterLayer to sf vector data")
    data <- sf::st_as_sf(raster::rasterToPolygons(data))
    fill <- sf::st_drop_geometry(data)[, 1]
  }
  if (class(data)[1] %in% c("SpatialPolygonsDataFrame", 
                            "SpatialPointsDataFrame", "SpatialLinesDataFrame")) {
    message("Transforming sp to sf")
    data <- sf::st_as_sf(data)
  }
  if (sf::st_geometry_type(data, FALSE) == "POINT") {
    emodnet_map_plot <- emodnet_map_basic + ggplot2::geom_sf(data = data, 
                                                             color = emodnet_colors()$yellow)
  }
  if (sf::st_geometry_type(data, FALSE) == "POLYGON") {
    emodnet_map_plot <- emodnet_map_basic + ggplot2::geom_sf(data = data, 
                                                             ggplot2::aes(fill = fill), size = 0.05,color=NA) + ggplot2::scale_fill_viridis_c(alpha = 0.8, 
                                                                                                                                     name = legend, direction = direction, option = option)
  }
  if (zoom == TRUE) {
    bbox <- sf::st_bbox(sf::st_transform(data, crs))
    xlim <- c(bbox$xmin, bbox$xmax)
    ylim <- c(bbox$ymin, bbox$ymax)
  }
  emodnet_map_plot <- emodnet_map_plot + ggplot2::coord_sf(crs = crs, 
                                                           xlim = xlim, ylim = ylim)
  return(emodnet_map_plot)
}