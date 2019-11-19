
#' Shift polygons to random points
#'
#' Shift polygons to random points in a bounding box
#'
#' @param bounding_box_polygon SpatialPolygonsDataFrame, a single polygon bounding box
#' @param input_polygons SpatialPolygonsDataFrame, many polygons
#' @param n integer, 100 to 1000 are usually good values
#'
#' @export


# Now we'll write a function that will take each rock in our rock shapefile
# and shift it to a randomly chosen point from our 1000 random points.
# That will result in a shapefile that contains a completely random
# shuffle of the rocks within the excavation area. We'll nest that
# function in another function that repeats that process of making a
# random-shuffle shapefile `r  n <- 1000; n` times. We'll end up with
# `r n` shapefiles of randomly rearranged rocks. This took a couple of
# minutes to run on my computer. Output should be called
# input_polygons_randomly_shuffled

shift_poly_to_random_points <-
  function(bounding_box_polygon, # SpatialPolygonsDataFrame
           input_polygons,      # SpatialPolygonsDataFrame
           n = 100) {

  excv <-   bounding_box_polygon
  rocks <- input_polygons
  rnd <- sp::spsample(excv, n, type = "random")

  # Loops to generate n shapefiles with randomly located rocks
  # n is the number of shapefiles to make
  # (assigned in inline code in the para above)
  # create a storage list for output of loop
  rocks_list <- vector("list", length = n)
  for (j in 1:n) {
    # first loop
    # create a storage list for output of loop
    rocks_rnd <- vector("list", length = length(rocks))
    # randomly relocate every rock in the shapefile
    for (i in 1:length(rocks)) {
      # second loop
      # this is where we move each rock, one by one
      # get a rock from our observed rocks to shift
      ri <- rocks[i, ]
      # we have to shift all of the coords relating to this rock...
      # get coords of vertices (outline of rock)
      cds <-
        slot(slot(slot(ri, "polygons")[[1]], "Polygons")[[1]], "coords")
      # get coords of labpt, labpt, bbox also (centrepoints and bounding box)
      l1 <-
        slot(slot(slot(ri, "polygons")[[1]], "Polygons")[[1]], "labpt")
      l2 <- slot(slot(ri,  "polygons")[[1]], "labpt")
      b1 <- slot(ri,  "bbox")
      # get a random point in the excavation area to shift to
      rn <- unname(rnd@coords[sample(1:length(rnd), 1), ])
      # shift all the vertices coords
      ofst <- cds[1, ] - rn
      newcds <- t(apply(cds, 1, function(x)
        x - ofst))
      # shift lapt1 (centrepoint)
      ofst <- l1 - rn
      newl1 <- l1 - ofst
      # shift lapt2 (centrepoint)
      ofst <- l2 - rn
      newl2 <- l2 - ofst
      # shift bbox (bounding box)
      ofst <- t(apply(b1, 2,  function(x)
        x - rn))
      newb1 <- b1 - ofst
      # put these shifted points back into the polygon object
      slot(slot(slot(ri, "polygons")[[1]], "Polygons")[[1]], "coords") <-
        newcds
      slot(slot(slot(ri, "polygons")[[1]], "Polygons")[[1]], "labpt") <-
        newl1
      slot(slot(ri,  "polygons")[[1]], "labpt") <- newl2
      slot(ri, "bbox") <- newb1
      # assign this rock to the list of rocks in the shapefile
      rocks_rnd[[i]] <- ri
    } # end rearrangemet of all the rocks in one shapefile
    # this is where we collect each of the n shapefiles and
    # put them in a list
    # make list of SpatialPolygons
    rocks_list[[j]] <- do.call(rbind, rocks_rnd)
  }

  return(rocks_list)
  }


#' Compute overlap area of polygons randomly shuffled
#'
#' Compute overlap area of polygons randomly shuffled
#'
#' @param input_polygons_randomly_shuffled SpatialPolygonsDataFrame, output from shift_poly_to_random_points function
#' @param other_polygons SpatialPolygonsDataFrame, many polygons
#'
#' @export

# Now we can write a function that will calculate the area of intersection
# (or overlap) between rock polygons and skeleton polygons for each
# of our `r n` random-shuffle shapefiles.

compute_overlap_area_of_polygons_randomly_shuffled <-
  function(input_polygons_randomly_shuffled,
           other_polygons) {
    rocks_list <- input_polygons_randomly_shuffled
    skeles <-  other_polygons
    skeles <- maptools::unionSpatialPolygons(skeles,
                                   ID=rep(1,
                                          times=length(skeles@polygons)))


    # make a list to store the output of the function
    int_area <- vector("list", length(length(rocks_list)))
    for (i in 1:length(rocks_list)) {
      # get polygons that are just the intersection of rocks and skeles
      x <- PBSmapping::joinPolys(
        PBSmapping::combinePolys(maptools::SpatialPolygons2PolySet(skeles)),
        PBSmapping::combinePolys(maptools::SpatialPolygons2PolySet(rocks_list[[i]])),
        "INT"
      )
      if(!is.null(x)){
      x <- suppressWarnings(maptools::PolySet2SpatialPolygons(x))
      # extract area of intersecting polygons
      areas <- sapply(slot(x, "polygons"),
                      function(x)
                        sapply(slot(x, "Polygons"), slot, "area"))
      # store output in list
      int_area[[i]] <- sum(areas)
      } else {
        int_area[[i]] <- 0
      }
    }
    random_areas <- data.frame(area = unlist(int_area))
    return(random_areas)
  }

#' Compute overlap area of polygons observed
#'
#' Compute overlap area of polygons observed
#'
#' @param input_polygons SpatialPolygonsDataFrame, many polygons
#' @param other_polygons SpatialPolygonsDataFrame, many polygons
#'
#' @export

# Now we need to calculate our observed amount of overlap of actual rocks on
# the skeletons so we can see how this compares to the distribution of random areas.

compute_overlap_area_of_polygons_observed <-
  function(input_polygons,
           other_polygons){

    rocks <- input_polygons
    skeles <-  other_polygons

x <- PBSmapping::joinPolys( PBSmapping::combinePolys(maptools::SpatialPolygons2PolySet(rocks)),
                            PBSmapping::combinePolys(maptools::SpatialPolygons2PolySet(skeles)),
                "INT" )
x <- suppressWarnings(maptools::PolySet2SpatialPolygons(x))
# calculate area of intersecting polygons
areas <- sapply(slot(x, "polygons"),
                function(x) sapply(slot(x, "Polygons"), slot, "area"))
obs_area <- sum(areas)

return(obs_area)
}
