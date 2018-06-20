library(dplyr); library(sf)

# Data loading ----
# Load NCBO bottom type data and pull out Marshyhope polygons
habitat <- read_sf(dsn ='C:/Users/secor/Downloads/2015 Atlantic Sturgeon Habitat Geodatabase and Report Nanticoke and Tributaries-2016-01-19/2015 Atlantic Sturgeon Habitat Geodatabase Nanticoke and Tributaries 01132016.gdb',
                          layer = 'RiverBed_Habitat_Polygons_CMECS_SC_01132016') %>%
  filter(Location == 'Marshyhope Creek, MD') %>%
  mutate(sed_type = case_when(SubGroup == '<Null>' ~ Group_,
                           T ~ SubGroup)) %>%
  # Reproject from UTM to longlat (epsg = 4326)
  st_transform(4326)


# Load position data
fish_pos <- read.csv(
  'p:/obrien/biotelemetry/nanticoke/vps results/positions/all-calc-positions.csv',
                    stringsAsFactors = F) %>%
  rename_all(tolower) %>%
  filter(grepl('^\\d', transmitter)) %>%
  mutate(datetime = lubridate::ymd_hms(datetime)) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)


# Point-in-Polygon analysis ----
# Find which habitats detections fall into
pip <- st_join(fish_pos, habitat, join = st_intersects) %>%
  group_by(transmitter, sed_type) %>%
  summarize(n = n()) %>%
  mutate(prop = n/sum(n))

# Area of bottom types in VPS array ----
VPShabitat <- habitat %>%
  # Crop bottom type polygons to the bounding box of fish detections
  st_crop(st_bbox(fish_pos)) %>%
  # Lengths and area of cropped polygons
  mutate(VPSlength = st_length(.),
         VPSarea = st_area(.)) %>%
  # Summarize by sediment type
  group_by(sed_type) %>%
  summarize(grp.area = sum(VPSarea)) %>%
  ungroup() %>%
  mutate(prop = as.numeric(grp.area / sum(grp.area)))


plot.data <- full_join(data.frame(VPShabitat), data.frame(pip)) %>%
  mutate(category = case_when(is.na(transmitter) ~ 'Habitat',
                              T ~ transmitter))
library(ggplot2)
ggplot() + geom_col(data = plot.data,
                    aes(x = sed_type,
                        y = prop, fill = category),
                    position = 'dodge') +
  labs(x = NULL, y = 'Proportion in Array', fill = NULL) +
  scale_fill_manual(values = c(colorRampPalette(c('lightgreen', 'darkgreen'))(8), 'black')) +
  theme_bw()
