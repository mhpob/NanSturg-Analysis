library(dplyr); library(sf)

# Read and manipulate flowline data ---
flowline <- st_read('manuscript/data/nanticoke_flowline.gpkg')


##  Merge different within-river secctions into one.
flowline <- flowline %>%

  # Combine different sections into one object
  group_by(gnis_name) %>%
  summarize(geom = st_combine(geom)) %>%

  # Merge within-object lines together
  group_by(gnis_name) %>%
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
mouths <- data.frame(body = c('Marshyhope Creek', 'Broad Creek', 'Deep Creek'),
                     rkm_nan_mouth = as.numeric(cumsum(nan_split)[1:3]))

# Units: [km]
# [1] 45.55766 58.47172 68.09327 80.51762
#     Marshy,  Broad,   Deep



# Sites ----
##DNREC
dnrec <- read.csv('manuscript/data/detections/past receiver locations.csv') %>%
  arrange(body) %>%
  st_as_sf(coords = c('long', 'lat'),
           remove = F,
           crs = 4326) %>%
  st_transform(32618)


# Break the line that makes up the flowline into points 1 m apart from each other ----
flowline_pts <- flowline %>%
  st_segmentize(1) %>%
  st_cast('MULTIPOINT')


pts <- lapply(flowline_pts$gnis_name, function(.){
  st_nearest_points(flowline_pts[flowline_pts$gnis_name == .,],
                               dnrec[dnrec$body == .,])
})
pts <- lapply(pts, st_as_sf)

pts <- bind_rows(pts)

st_geometry(dnrec) <- st_geometry(pts)


# Cast flowline from MULTIPOINT into simplified LINESTRING ---
flowline_simp <- st_cast(flowline_pts, 'LINESTRING')

dnrec$rkm_body_mouth <- NA


for(i in 1:nrow(dnrec)){
  # Split flowline in half according to location of receiver
  flowline_split <- flowline_simp %>%
    filter(gnis_name == dnrec[i,]$body) %>%
    lwgeom::st_split(dnrec[i,]) %>%
    st_collection_extract('LINESTRING')

  # Find the length of the flowline between locations
  lengths <- flowline_split %>%
    st_length() %>%

    # convert to KM
    units::set_units(km) %>%
    # Choose the down-river section (flowlines are measured from up- to down-river)
    .[[2]]

  dnrec[i,]$rkm_body_mouth <- lengths
}


dnrec <- dnrec %>%
  left_join(mouths) %>%
  mutate(rkm_nan_mouth = ifelse(is.na(rkm_nan_mouth), 0, rkm_nan_mouth),
         rkm_gross = rkm_body_mouth + rkm_nan_mouth,
         error_m = as.numeric(st_length(.)),
         rkm_nan_mouth = NULL) %>%
  data.frame %>%
  select(-geometry)

write.csv(dnrec, 'manuscript/dnrec_rkm.csv', row.names = F)


## MDNR
dets <- data.table::fread('manuscript/data/detections/sturgeon_detections.gz')

mdnr <- dets %>%
  tibble() %>%
  distinct(station, lat, long) %>%
  arrange(-lat) %>%
  st_as_sf(coords = c('long', 'lat'),
           remove = F,
           crs = 4326) %>%
  st_transform(32618)


pts <-  st_nearest_points(flowline_pts[flowline_pts$gnis_name == 'Marshyhope Creek',],
                          mdnr) %>%
  st_as_sf

st_geometry(mdnr) <- st_geometry(pts)
mdnr$rkm_body_mouth <- units::set_units(1, 'km')

for(i in 1:nrow(mdnr)){
  # Split flowline in half according to location of receiver
  flowline_split <- flowline_simp %>%
    filter(gnis_name == 'Marshyhope Creek') %>%
    lwgeom::st_split(recs[i,]) %>%
    st_collection_extract('LINESTRING')

  # Find the length of the flowline between locations
  lengths <- flowline_split %>%
    st_length() %>%

    # convert to KM
    units::set_units(km) %>%
    # Choose the down-river section (flowlines are measured from up- to down-river)
    .[[2]]

  recs[i,]$rkm_body_mouth <- lengths
}

mdnr <- mdnr %>%
  mutate(rkm_nan_mouth = mouths[mouths$body == 'Marshyhope Creek',]$rkm_nan_mouth,
         rkm_gross = rkm_body_mouth + units::set_units(rkm_nan_mouth, 'km'),
         error_m = as.numeric(st_length(.))) %>%
  tibble() %>%
  select(-geometry, -rkm_nan_mouth)

write.csv(mdnr, 'manuscript/mddnr_rkm.csv', row.names = F)
