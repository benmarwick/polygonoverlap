#' Polygon Overlap Analysis using sf
#'
#' Modern implementation using sf package instead of sp/rgeos/rgdal
#'
#' This package computes the probability that an observed area of overlap
#' between two sets of polygons is due to chance.
#' @keywords internal
"_PACKAGE"

#' Shift polygons to random points within a bounding box
#'
#' @param bounding_box_polygon sf object representing the bounding box
#' @param input_polygons sf object with polygons to be randomly shifted
#' @param n number of random shuffles to perform
#'
#' @return A list of sf objects, each representing one random shuffle
#' @export
shift_poly_to_random_points <- function(bounding_box_polygon,
                                        input_polygons,
                                        n) {
  # Get bounding box extent
  bbox <- sf::st_bbox(bounding_box_polygon)

  # Calculate centroid of input polygons
  input_centroid <- sf::st_centroid(sf::st_union(input_polygons))
  input_coords <- sf::st_coordinates(input_centroid)

  # Store results
  result_list <- vector("list", n)

  # Perform n random shuffles
  for (i in 1:n) {
    # Generate random point within bounding box
    repeat {
      random_x <- stats::runif(1, bbox["xmin"], bbox["xmax"])
      random_y <- stats::runif(1, bbox["ymin"], bbox["ymax"])
      random_point <- sf::st_point(c(random_x, random_y))
      # Use the CRS from the bounding box
      random_point <- sf::st_sfc(random_point, crs = sf::st_crs(bounding_box_polygon))

      # Check if point is within bounding box polygon
      # sparse = FALSE ensures we get a logical matrix/vector
      if (sf::st_intersects(random_point, bounding_box_polygon, sparse = FALSE)[1]) {
        break
      }
    }

    # Calculate translation vector
    dx <- random_x - input_coords[1]
    dy <- random_y - input_coords[2]

    # Shift all input polygons
    # Note: simple arithmetic on geometries works because sf loads the S3 methods
    shifted_polygons <- sf::st_geometry(input_polygons) + c(dx, dy)
    shifted_polygons <- sf::st_set_crs(shifted_polygons, sf::st_crs(input_polygons))

    # Create sf object with original attributes
    result_list[[i]] <- sf::st_sf(
      geometry = shifted_polygons,
      sf::st_drop_geometry(input_polygons)
    )
  }

  return(result_list)
}


#' Compute overlap area for randomly shuffled polygons
#'
#' @param input_polygons_randomly_shuffled list of sf objects from shift_poly_to_random_points
#' @param other_polygons sf object representing fixed polygons
#'
#' @return data.frame with areas of overlap for each shuffle
#' @export
compute_overlap_area_of_polygons_randomly_shuffled <- function(
    input_polygons_randomly_shuffled,
    other_polygons) {

  # Dissolve other_polygons to avoid counting overlaps multiple times
  other_polygons_union <- sf::st_union(other_polygons)

  # Calculate overlap for each shuffle
  areas <- sapply(input_polygons_randomly_shuffled, function(shuffled) {
    # Union the shuffled polygons
    shuffled_union <- sf::st_union(shuffled)

    # Calculate intersection
    intersection <- sf::st_intersection(shuffled_union, other_polygons_union)

    # Calculate area (handling empty intersections)
    if (length(intersection) == 0 || sf::st_is_empty(intersection)) {
      return(0)
    } else {
      return(sf::st_area(intersection))
    }
  })

  # Convert to numeric (removes units class)
  areas <- as.numeric(areas)

  return(data.frame(area = areas))
}


#' Compute observed overlap area between two sets of polygons
#'
#' @param input_polygons sf object with first set of polygons
#' @param other_polygons sf object with second set of polygons
#'
#' @return numeric value representing the area of overlap
#' @export
compute_overlap_area_of_polygons_observed <- function(input_polygons,
                                                      other_polygons) {

  # Union both sets of polygons
  input_union <- sf::st_union(input_polygons)
  other_union <- sf::st_union(other_polygons)

  # Calculate intersection
  intersection <- sf::st_intersection(input_union, other_union)

  # Calculate area
  if (length(intersection) == 0 || sf::st_is_empty(intersection)) {
    return(0)
  } else {
    return(as.numeric(sf::st_area(intersection)))
  }
}


#' Plot polygons for visualization
#'
#' @param bounding_box_polygon sf object
#' @param input_polygons sf object
#' @param other_polygons sf object
#' @param title character string for plot title
#'
#' @export
plot_polygons <- function(bounding_box_polygon,
                          input_polygons,
                          other_polygons,
                          title = "Plot of input polygons") {

  # Use ggplot2 namespace explicitly
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = bounding_box_polygon, fill = NA, color = "black") +
    ggplot2::geom_sf(data = input_polygons, fill = NA, color = "green", linewidth = 1) +
    ggplot2::geom_sf(data = other_polygons, fill = NA, color = "red", linewidth = 1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = title) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}


#' Helper function to safely read shapefiles
#' @param path path to shapefile
#' @param verbose whether to print messages
#' @export
safe_read_shapefile <- function(path, verbose = FALSE) {

  tryCatch({
    # Try standard read
    shp <- sf::st_read(path, quiet = !verbose)

    # Check if geometry is valid
    if (!all(sf::st_is_valid(shp))) {
      if (verbose) message("Fixing invalid geometries...")
      shp <- sf::st_make_valid(shp)
    }

    return(shp)
  }, error = function(e) {
    if (verbose) {
      message("Standard read failed, trying alternative method...")
      message("Error was: ", e$message)
    }

    # Try reading with different options
    shp <- sf::st_read(path, quiet = !verbose,
                       options = c("ENCODING=UTF-8"))

    if (!all(sf::st_is_valid(shp))) {
      if (verbose) message("Fixing invalid geometries...")
      shp <- sf::st_make_valid(shp)
    }

    return(shp)
  })
}
