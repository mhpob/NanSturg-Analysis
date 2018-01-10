library(dplyr); library(raster)

# Data loading ----
# Load NCBO bottom type data and pull out Marshyhope polygons
habitat <- rgdal::readOGR(dsn ='C:/Users/secor/Downloads/2015 Atlantic Sturgeon Habitat Geodatabase and Report Nanticoke and Tributaries-2016-01-19/2015 Atlantic Sturgeon Habitat Geodatabase Nanticoke and Tributaries 01132016.gdb',
                          layer = 'RiverBed_Habitat_Polygons_CMECS_SC_01132016')
habitat <- habitat[habitat$Location == 'Marshyhope Creek, MD',]

# Reproject from UTM to longlat
habitat <- spTransform(habitat, CRS = CRS('+proj=longlat +zone=18 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))


# Load position data
all_pos <- read.csv(
  'p:/obrien/biotelemetry/nanticoke/vps results/positions/all-calc-positions.csv',
                    stringsAsFactors = F) %>%
  mutate(DATETIME = lubridate::ymd_hms(DATETIME))
names(all_pos) <- tolower(names(all_pos))

fish_pos <- all_pos %>%
  filter(grepl('^\\d', all_pos$transmitter))


# Point-in-Polygon analysis ----
# Find which habitats detections fall into
pip <- over(SpatialPoints(coords = fish_pos[, c('lon', 'lat')],
                          proj4string = CRS(proj4string(habitat))),
            habitat) %>%
  cbind(fish_pos) %>%
  group_by(transmitter, Group_, SubGroup) %>%
  summarize(n())

# Area of bottom types in VPS array ----
# Create bounding box of fish detections
bounds <-  bbox(cbind(fish_pos$lon, fish_pos$lat))
bounds <- matrix(c(bounds[1, 1], bounds[2, 1],
                     bounds[1, 1], bounds[2, 2],
                     bounds[1, 2], bounds[2, 2],
                     bounds[1, 2], bounds[2, 1],
                     bounds[1, 1], bounds[2, 1]),
                     ncol = 2, byrow = T)

bounds <- SpatialPolygons(list(Polygons(list(Polygon(bounds)),
                                          'Fish detection bounding box')),
                            proj4string = CRS(proj4string(habitat)))

# Crop bottom type polygons to the bounding box
VPShabitat <- intersect(habitat, bounds)
VPShabitat$VPSlength <- SpatialLinesLengths(as(VPShabitat, 'SpatialLines')) * 1000
VPShabitat$VPSarea <- area(VPShabitat)
VPShabitat$VPSacres <- VPShabitat$VPSarea * 0.00024711

VPShabitat@data %>%
  group_by(Group_, SubGroup) %>%
  summarize(grp.area = sum(VPSarea)) %>%
  ungroup() %>%
  mutate(prop.area = grp.area / sum(grp.area) * 100)
