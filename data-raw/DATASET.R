## code to prepare `DATASET` dataset goes here

bounding_box_polygon <- rgdal::readOGR("data-raw/MK_II_excv_outline.shp")
input_polygons <- rgdal::readOGR("data-raw/rock_poly.shp")
other_polygons <- rgdal::readOGR("data-raw/skele_poly.shp")

usethis::use_data(bounding_box_polygon, input_polygons, other_polygons, overwrite = TRUE)

lapply(list(bounding_box_polygon, input_polygons, other_polygons), class)

#------ testing

# library(mapview)
# library(mapedit)
# library(tidyverse)
#
# # create polygons by drawing them on a map:
# other_polygons_spdf <-
#   mapview() %>%
#   editMap()
#
# input_polygons_spdf <-
#   mapview() %>%
#   editMap()
#
# bounding_box_spdf <-
#   mapview() %>%
#   editMap()
#
# # save
# saveRDS(other_polygons_spdf, "other_polygons_spdf.rds")
# saveRDS(input_polygons_spdf, "input_polygons_spdf.rds")
# saveRDS(bounding_box_spdf, "bounding_box_spdf.rds")

library(here)

other_polygons_spdf <-  readr::read_rds(here("data-raw/other_polygons_spdf.rds"))
input_polygons_spdf <- readr::read_rds(here("data-raw/input_polygons_spdf.rds"))
bounding_box_spdf <-  readr::read_rds(here("data-raw/bounding_box_spdf.rds"))

# plot
plot(bounding_box_spdf$finished$geometry)
plot(other_polygons_spdf$finished$geometry,  add = TRUE, col = "green")
plot(input_polygons_spdf$finished$geometry, add = TRUE, col = "red")

# convert to spatial polygons data frame
bounding_box_spdf <- as(bounding_box_spdf$finished, "Spatial")
other_polygons_spdf <- as(other_polygons_spdf$finished, "Spatial")
input_polygons_spdf <- as(input_polygons_spdf$finished, "Spatial")

# plot
sp::plot(bounding_box_spdf)
sp::plot(other_polygons_spdf,  add = TRUE, col = "green")
sp::plot(input_polygons_spdf, add = TRUE, col = "red")

library(polygonoverlap)

# step 1
n <- 100000
input_polygons_randomly_shuffled_spdf <-
  shift_poly_to_random_points(bounding_box_spdf,
                              input_polygons_spdf,
                              n)

# step 2
areas_of_overlap_from_random_shuffle_spdf <-
  compute_overlap_area_of_polygons_randomly_shuffled(input_polygons_randomly_shuffled_spdf,
                                                     other_polygons_spdf)

# step 3
observed_polygon_overlap_spdf <-
  compute_overlap_area_of_polygons_observed(input_polygons_spdf,
                                            other_polygons_spdf)

# plot
ggplot(areas_of_overlap_from_random_shuffle_spdf,
       aes(area)) +
  geom_histogram() +
  labs(x = expression("Areas of intersection of our two sets of polygons (m"^2*")"),
       y = "Frequency") +
  ggtitle(paste0("Distribution of areas of polygon overlap produced by ",
                 n,
                 " random shuffles")) +
  geom_vline(xintercept = observed_polygon_overlap_spdf,
             col = "red")
