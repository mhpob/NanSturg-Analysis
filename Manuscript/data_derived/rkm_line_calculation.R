library(dplyr); library(sf)

# Read and manipulate flowline data ---
flowline <- st_read('c:/users/darpa2/analysis/marshyhopeas-2019-22/data/raw/nanticoke_flowline.gpkg')


##  Merge different within-river secctions into one.
flowline <- flowline %>%

  # Combine different sections into one object
  group_by(gnis_name) %>%
  summarize(geom = st_combine(geom), .groups = 'keep') %>%

  # Merge within-object lines together
  summarize(geom = st_line_merge(geom)) %>%
  st_transform(32618)



# Break the line that makes up the flowline into points 1 km apart from each other ----
rkms <- flowline %>%
  st_line_sample(density = 1/1000) %>%
  st_zm() %>%
  st_as_sf() %>%
  mutate(body = flowline$gnis_name) %>%
  st_cast('POINT', ids = body) %>%
  rbind(lwgeom::st_endpoint(flowline) %>%
          st_as_sf() %>%
          mutate(body = flowline$gnis_name)) %>%
  st_transform(4326) %>%
  group_by(body) %>%
  mutate(rkm = (length(body)-1):0) %>%
  # Select every 5th RKM
  filter(rkm %in% seq(5, max(rkm), by = 5))


nan_poly <- st_read('manuscript/data/spatial/NHD_H_0208_HU4_GDB.gdb',
                    layer = 'wbdhu10',
                    query = "SELECT OGR_GEOM_WKT AS wkt
                        FROM wbdhu10
                        WHERE States LIKE 'D%'")

nan_poly <- st_read('manuscript/data/spatial/NHD_H_0208_HU4_GDB.gdb',
                    layer = 'nhdarea',
                    wkt_filter = nan_poly$wkt)

# Convert to meter-based CRS
nan_poly <- nan_poly %>%
  st_transform(32618)

rkms <- rkms %>%
  st_transform(32618) %>%
  st_intersection(nan_poly)



# Using for loop (slightly faster than purr; 6 v 10s)----
buff_pts <- rkms %>%
  st_buffer(5000) %>%
  st_cast('POINT')

buff_pts <- split(buff_pts, interaction(buff_pts$body, buff_pts$rkm), drop = T)


line_ind <- vector('list', length(buff_pts))
rkm_lines <- vector('list', length(buff_pts))
buff_lines <- vector('list', length(buff_pts))

for(i in seq_along(buff_pts)){
  # There are 30 vertices per quarter of the circle (this can be changed, see ?sf::st_buffer)
  ##  For a staight line through the center, we want to connect vertices
  ##    opposite from one another (vertex 1 and 61 are a pair; vertex 2 and 62; etc.)
  ##  We only need to calculate these one way (line connecting 1 to 61 only, no need for 61 to 1)
  buff_lines[[i]] <- mapply(
    function(a, b){
      st_cast(st_union(a, b), 'LINESTRING')
    },
    st_geometry(buff_pts[[i]][1:60,]),
    st_geometry(buff_pts[[i]][61:120,]),
    SIMPLIFY = F) %>%
    st_sfc(crs = 32618) %>%
    st_as_sf()

  rkm_poly <- nan_poly %>%
    st_intersection(st_buffer(
      filter(rkms,
             body == buff_pts[[i]]$body[1],
             rkm == buff_pts[[i]]$rkm[1]),
      2000))

  rkm_lines[[i]] <- buff_lines[[i]] %>%
    ## note for future self: rkm_poly only has one polygon -- if there are more
    ##  need to st_union first!!!
    st_intersection(rkm_poly) %>%
    st_cast('MULTILINESTRING') %>%
    st_cast('LINESTRING') %>%
    filter(st_intersects(.,
                         st_buffer(
                           filter(rkms,
                                  body == buff_pts[[i]]$body[1],
                                  rkm == buff_pts[[i]]$rkm[1]),
                           1), #buffer b/c of precision issues
                         sparse = F)) %>%
    slice(which.min(st_length(.)))


}

rkm_lines <- bind_rows(rkm_lines)
rkm_lines <- rkm_lines %>%
  select(body, rkm, x)



rkm_lines <- rkm_lines %>%
  st_transform(4326)
row.names(rkm_lines) <- NULL

# st_write(rkm_lines, 'manuscript/data_derived/rkm_lines.gpkg')



# The above, but using {purrr} (slightly slower than FOR; 10 v 6s) ----
library(tidyr); library(purrr)

rkm_lines <- rkms %>%

  # Drop extra columns
  select(body, rkm) %>%

  # "nest" (group) the points by body/rkm combination
  nest(geom = x) %>%

  mutate(

    # Create 5km buffer polygon around the rkm points
    ##  cast the polygon to points to get the vertices
    buffer = map(geom,
                 ~ st_cast(
                   st_buffer(.x, 5000),
                   'POINT'
                 )),

    # There are 30 vertices per quarter of the circle (this can be changed, see ?sf::st_buffer)
    ##  For a staight line through the center, we want to connect vertices
    ##    opposite from one another (vertex 1 and 61 are a pair; vertex 2 and 62; etc.)
    ##  We only need to calculate these one way (line connecting 1 to 61 only, no need for 61 to 1)
    line_st = map(buffer, ~ st_geometry(slice(., 1:60))),
    line_end = map(buffer, ~ st_geometry(slice(., 61:120))),

    # "buffer" column no longer needed; drop to free up memory
    buffer = NULL,

    # sf::st_union is not a "tidy" function; we want to combine points by row here,
    #   but sf::st_union provides a Cartesian product (all combinations of points).
    #   geotidy::st_union does what we want -- a row-wise union.
    line = map2(line_st, line_end,
                ~ geotidy::st_union(.x, .y)),

    # "line_st" and "line_end" no longer needed; drop to free up memory
    line_st = NULL,
    line_end = NULL,

    # cast the start/end points in "line" to a LINESTRING
    line = map(line, ~ st_cast(., 'LINESTRING')),

    # create simple feature collection with the correct CRS and convert to simple features
    line = map(line, ~ st_sfc(.x, crs = 32618)),
    line = map(line, ~ st_as_sf(.x)),

    # Crop Nanticoke polygon for smaller intersection
    rkm_crop = map(line, ~ st_crop(nan_poly, .x)),

    # Crop the lines by cropped Nanticoke polygon
    ## note for future self: nan_poly only had one polygon -- if there are more
    ##  need to st_union first!!!

    line = map2(line, rkm_crop, ~ st_intersection(.x, .y)),

    # After cropping, we can wind up with multiple (MULTILINESTING) or
    #   single (LINESTRING) line segments. To get them all to a LINESTRING, we need to cast
    #   up to MULTILINESTRING so they are all in the same format, then back down to
    #   LINESTRING
    line = map(line, ~ st_cast(., 'MULTILINESTRING')),
    line = map(line, ~ st_cast(., 'LINESTRING')),

    # Remove Z value (it's all zero anyway)
    line = map(line, ~ st_zm(.)),

    # Since some lines are made up of multiple line segments, select the one that
    #   intersects with the original RKM point. Buffering by 1 meter to get around
    #   precision issues.
    line = map2(line, geom, ~ filter(.x,
                                     st_intersects(.x,
                                                   st_buffer(.y, 1),
                                                   sparse = F)
                                     )
                ),

    # Select the shortest line segment that passes though the RKM point.
    line = map(line, ~ slice(.x, which.min(st_length(.x))))
  ) %>%

  # Extract the now-single line for each body/rkm combination
  unnest(line) %>%

  # Convert to simple features
  st_as_sf() %>%

  # Drop "geom" column rendered redundant after unnesting
  select(body, rkm, x) %>%

  # Transform back to lat/long
  st_transform(4326)
