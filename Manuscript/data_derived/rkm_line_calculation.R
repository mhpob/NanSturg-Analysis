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

## Return RKMs
cumsum(nan_split)

# Units: [km]
# [1] 45.55766 58.47172 68.09327 80.51762
#     Marshy,  Broad,   Deep



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
  mutate(rkm = (length(body)-1):0)

# Select every 5th RKM
rkms <- rkms %>%
  filter(rkm %in% seq(0, max(rkm), by = 5))


# write.csv(rkms, 'data/raw/nanticoke_system_rkms.csv', row.names = F)

nan_poly <- st_read('manuscript/data/spatial/NHD_H_0208_HU4_GDB.gdb',
                    layer = 'wbdhu10',
                    query = "SELECT OGR_GEOM_WKT AS wkt
                        FROM wbdhu10
                        WHERE States LIKE 'D%'")

nan_poly <- st_read('manuscript/data/spatial/NHD_H_0208_HU4_GDB.gdb',
                    layer = 'nhdarea',
                    wkt_filter = nan_poly$wkt)

# Convert rkms to nan_poly crs
rkms <- rkms %>%
  st_transform(st_crs(nan_poly))


rkms <- rkms %>%
  st_transform(32618)

nan_poly <- nan_poly %>%
  st_transform(32618)

rkm5 <- rkms %>%
  ungroup() %>%
  filter(grepl('Nan', body), rkm == 5)


rkm5_poly <- nan_poly %>%
  st_intersection(st_buffer(rkm5, 5000))



buff_pts <- st_buffer(rkm5, 5000) %>%
  st_cast('POINT')

line_ind <- buff_pts %>%
  st_distance(buff_pts) %>%
  as.matrix() %>%
  apply(1, as.numeric)

line_ind[lower.tri(line_ind, diag = T)] <- NA
line_ind <- which(line_ind[, 1:(ncol(line_ind)-1)] >= 9999.9, arr.ind = T)

buff_lines <- mapply(
  function(a, b){
    st_cast(st_union(a, b), 'LINESTRING')
  },
  st_geometry(buff_pts[line_ind[,1],]),
  st_geometry(buff_pts[line_ind[,2],]), SIMPLIFY = F) %>%
  st_sfc(crs =32618)



plot(st_geometry(rkm5_poly), add = T)
plot(buff_lines, add = T, col = 'red', lwd = 5)


test <- buff_lines %>%
  st_as_sf() %>%
  st_intersection(rkm5_poly) %>%
  st_cast('MULTILINESTRING') %>%
  st_cast('LINESTRING') %>%
  filter(st_intersects(.,
                       st_buffer(rkm5, 1), #buffer b/c of precision issues
                       sparse = F)) %>%
  slice(which.min(st_length(.)))

plot(st_geometry(rkm5_poly))
plot(st_geometry(test), add = T, col = 'red')
plot(st_geometry(rkm5), add = T, col = 'blue')
