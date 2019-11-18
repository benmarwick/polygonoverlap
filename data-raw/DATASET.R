## code to prepare `DATASET` dataset goes here

bounding_box_polygon <- rgdal::readOGR("data-raw/MK_II_excv_outline.shp")
input_polygons <- rgdal::readOGR("data-raw/rock_poly.shp")
other_polygons <- rgdal::readOGR("data-raw/skele_poly.shp")

usethis::use_data(bounding_box_polygon, input_polygons, other_polygons, overwrite = TRUE)

lapply(list(bounding_box_polygon, input_polygons, other_polygons), class)
