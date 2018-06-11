library(dplyr); library(sf)

# Data loading ----
# Load NCBO bottom type data and pull out Marshyhope polygons
habitat <- read_sf(dsn ='C:/Users/secor/Downloads/2015 Atlantic Sturgeon Habitat Geodatabase and Report Nanticoke and Tributaries-2016-01-19/2015 Atlantic Sturgeon Habitat Geodatabase Nanticoke and Tributaries 01132016.gdb',
                          layer = 'RiverBed_Habitat_Polygons_CMECS_SC_01132016')
habitat <- habitat[habitat$Location == 'Marshyhope Creek, MD',]

# Reproject from UTM to longlat (epsg = 4326)
habitat <- st_transform(habitat, 4326)


# Load position data
all_pos <- read.csv(
  'p:/obrien/biotelemetry/nanticoke/vps results/positions/all-calc-positions.csv',
                    stringsAsFactors = F) %>%
  mutate(DATETIME = lubridate::ymd_hms(DATETIME)) %>%
  rename_all(tolower)

fish_pos <- all_pos %>%
  filter(grepl('^\\d', all_pos$transmitter)) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)


# Point-in-Polygon analysis ----
# Find which habitats detections fall into
pip <- st_join(fish_pos, habitat, join = st_intersects) %>%
  group_by(transmitter, Group_, SubGroup) %>%
  summarize(n())

# Area of bottom types in VPS array ----
VPShabitat <- habitat %>%
  # Crop bottom type polygons to the bounding box of fish detections
  st_crop(st_bbox(fish_pos)) %>%
  # Lengths and area of cropped polygons
  mutate(VPSlength = st_length(.),
         VPSarea = st_area(.)) %>%
  # Summarize by sediment type
  group_by(Group_, SubGroup) %>%
  summarize(grp.area = sum(VPSarea)) %>%
  ungroup() %>%
  mutate(prop.area = grp.area / sum(grp.area) * 100)
