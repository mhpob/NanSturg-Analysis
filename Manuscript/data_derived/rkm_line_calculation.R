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



# Calculate RKM of Creek mouths ----
## Find where the creeks meet the mainstem
mouths <- flowline %>%
  st_intersection() %>%
  filter(n.overlaps == 2)

## Find RKM of Creek mouths
nan_split <- flowline %>%
  filter(gnis_name == 'Nanticoke River') %>%

  # Split Nanticoke by mouth locations
  lwgeom::st_split(mouths) %>%

  # Pull out the split sections
  st_collection_extract('LINESTRING') %>%

  # Find their lengths in km
  st_length() %>%
  units::set_units(km)



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
  filter(rkm %in% seq(0, max(rkm), by = 5))


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




buff_pts <- rkms %>%
  st_buffer(5000) %>%
  st_cast('POINT')

buff_pts <- split(buff_pts, interaction(buff_pts$body, buff_pts$rkm))
buff_pts <- buff_pts[sapply(buff_pts, function(x) length(st_geometry(x)) > 0)]


line_ind <- vector('list', length(buff_pts))
rkm_lines <- vector('list', length(buff_pts))
buff_lines <- vector('list', length(buff_pts))

for(i in seq_along(buff_pts)){
  line_ind[[i]] <- buff_pts[[i]] %>%
    st_distance() %>%
    as.matrix() %>%
    apply(., 1, as.numeric)

  # Drop duplicated values and zeroes (half the distance matrix)
  line_ind[[i]][lower.tri(line_ind[[i]], diag = T)] <- NA

  # Select indices of lines that span the whole circle (10 km, less 0.1 for precision issues)
  line_ind[[i]] <- which(line_ind[[i]][, 1:(ncol(line_ind[[i]]) - 1)] >= 9999.9, arr.ind = T)

  buff_lines[[i]] <- mapply(
    function(a, b){
      st_cast(st_union(a, b), 'LINESTRING')
    }, st_geometry(buff_pts[[i]][line_ind[[i]][,1],]),
    st_geometry(buff_pts[[i]][line_ind[[i]][,2],]),
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

plot(st_geometry(nan_poly))
plot(rkm_lines, add = T, col = 'red', lwd = 2)

rkm_lines <- rkm_lines %>%
  st_transform(4326)



#Simpler? Starting from line 80
buff_pts <- rkms %>%
  select(body, rkm) %>%
  st_buffer(5000) %>%
  st_cast('POINT') %>%
  tidyr::nest(buffer = x)



k <- buff_pts %>%
  mutate(
    # Create distance matrix between all vertices of the 5km buffer
    dist_mat = lapply(buffer, st_distance),

    # Remove lower triangle and diagonal of the matrix (length from A -> B is
    #   equivalent to the length from B -> A, so only need to calculate half)
    dist_mat = lapply(dist_mat, function(.) ifelse(lower.tri(., diag = T), NA, .)),

    # Find indices of the points that are the full diameter apart (10 km), but allow
    #   some wiggle room for precisions' sake (10 km - 1 = 9999)
    indices = lapply(dist_mat, function(.) which(.[, 1:(ncol(.) - 1)] > 9999, arr.ind = T)),
    line1 = lapply(buffer, function(.){
      slice()
    })
  )



## Fail at map
# k <- buff_pts %>%
#   mutate(line_st = map(buffer, ~ slice(., 1:60)),
#          line_end = map(buffer, ~ slice(., 61:120)),
#          line_end = map(line_end, function(.) mutate(., mark = st_coordinates(.)[,2])),
#          line = map2(line_st, line_end,
#                      st_union),
#          line = map(line, function(.) distinct(., mark)))
#
# p <- mapply(function(start, end){
#   mapply(
#     function(a, b){
#       st_cast(st_union(a, b), 'LINESTRING')
#     },
#     st_geometry(start),
#     st_geometry(end),
#     SIMPLIFY = F)
# },
# k$line_st,
# k$line_end)