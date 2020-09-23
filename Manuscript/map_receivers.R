library(sf)

# From https://www.sciencebase.gov/catalog/item/5a58a3d2e4b00b291cd68276
shape <- read_sf('manuscript/data/NHD_0208_HU4.gdb',
                 layer = 'NHDReachCodeMaintenance')
reachcode <- shape[grepl('Nanti|Marshy.*k$', shape$GNIS_Name),]$ReachCode
perm_id <- shape[grepl('Nanti|Marshy.*k$', shape$GNIS_Name),]$Permanent_Identifier

huc12 <- read_sf('manuscript/data/NHD_0208_HU4.gdb',
                 layer = 'WBDHU12')

View(huc12)

nhd_area <- read_sf('manuscript/data/NHD_0208_HU4.gdb',
                    layer = 'NHDArea')
# Pull HUC12 areas in the Nanticoke, Marshyhipe, Deep Creek, or Broad Creek

shape <- st_intersection(huc12[grepl('-Nant|-Marshy|^(Up|Lo).*Deep C|-Broad', huc12$Name),],
                            nhd_area)
shape <- shape[!grepl('Tommy Wright|Saulsbury', shape$Name),]
plot(st_geometry(shape))



plot(st_geometry(nhd_area))

lines <- read_sf('manuscript/data/NHD_0208_HU4.gdb',
                 layer = st_layers('manuscript/data/NHD_0208_HU4.gdb')$name[17])

k <- shape[shape$Permanent_Identifier %in% perm_id, ]

crop_sh <- st_bbox(c(xmin = -75.966,
                  xmax = -75.551,
                  ymin = 38.218,
                  ymax = 38.726),
                crs = st_crs(4326)) %>%
  st_as_sfc() %>%
  st_as_sf() %>%
  st_transform(st_crs(nhd_area)) %>%
  st_intersection(nhd_area)




md_shape <- read_sf('manuscript/data/maryland_rivers_and_streams.gdb')
crop <- st_bbox(c(xmin = -75.966,
                  xmax = -75.551,
                  ymin = 38.218,
                  ymax = 38.726),
                crs = st_crs(4326)) %>%
  st_as_sfc() %>%
  st_transform(st_crs(md_shape))

marshy <- md_shape %>%
  st_intersection(crop) %>%
  dplyr::filter(LAYER == 'SHORE')
