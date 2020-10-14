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




rkm_poly <- nan_poly %>%
  st_intersection(st_buffer(rkms, 5000))



buff_pts <- rkms %>%
  st_buffer(5000) %>%
  st_cast('POINT')

buff_pts <- split(buff_pts, interaction(buff_pts$body, buff_pts$rkm))
buff_pts <- buff_pts[sapply(buff_pts, function(x) length(st_geometry(x)) > 0)]

# line_ind <- lapply(buff_pts, function(.){
#   apply(
#     as.matrix(
#       st_distance(., .)
#     ),
#     1,
#     as.numeric)
# })

line_ind <- vector('list', length(buff_pts))
rkm_line <- vector('list', length(buff_pts))

for(i in seq_along(buff_pts)){
  line_ind[[i]] <- buff_pts[[i]] %>%
    st_distance() %>%
    as.matrix() %>%
    apply(., 1, as.numeric)

  line_ind[[i]][lower.tri(line_ind[[i]], diag = T)] <- NA

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
      1))

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

plot(st_geometry(nan_poly))
plot(test, add = T, col = 'red', lwd = 2)
