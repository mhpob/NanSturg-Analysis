library(ggplot2); library(dplyr); library(rgdal)

# Import positions ----
all_pos <- read.csv('p:/obrien/biotelemetry/nanticoke/vps results/positions/all-calc-positions.csv',
                stringsAsFactors = F) %>%
  mutate(DATETIME = lubridate::ymd_hms(DATETIME))
names(all_pos) <- tolower(names(all_pos))

# Select fish positions
fish_pos <- all_pos %>%
  filter(grepl('^\\d', all_pos$transmitter))

# Import receiver positions
rec_pos <- readxl::read_excel(
  'p:/obrien/biotelemetry/nanticoke/vps results/vps-nanticokeriver-01.xls',
  sheet = 'Stations', range = 'A3:J35') %>%
  filter(Args == 'Derived')

# Import bottom type polygons ----
habitat <- readOGR(dsn ='C:/Users/secor/Downloads/2015 Atlantic Sturgeon Habitat Geodatabase and Report Nanticoke and Tributaries-2016-01-19/2015 Atlantic Sturgeon Habitat Geodatabase Nanticoke and Tributaries 01132016.gdb',
                   layer = 'RiverBed_Habitat_Polygons_CMECS_SC_01132016')

# Select polygons in the Marshyhope
habitat <- habitat[habitat$Location == 'Marshyhope Creek, MD',]

# Reproject to lonlat
habitat <- spTransform(habitat, CRS = CRS('+proj=longlat +zone=18 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))


# Slight manipulation of underlying data
habitat@data$OBJECTID <- as.character(habitat@data$OBJECTID)
habitat@data$SubGroup <- ifelse(habitat@data$SubGroup == '<Null>', '',
                           levels(habitat@data$SubGroup)[habitat@data$SubGroup])

# Prepare to plot ----
hab.df <- fortify(habitat, region = 'OBJECTID')
# Bring back in other data
hab.df <- left_join(hab.df, habitat@data, by = c('id' = 'OBJECTID'))

# Pick only habitat polygons that are within the area of interest
# Not necessary, but removing the other bottom types makes the legend pretty
trim <- filter(hab.df,
            long >= -75.8145,
            long <= -75.8095,
            lat >= 38.6415,
            lat <= 38.649)
hab.df <- filter(hab.df, id %in% unique(trim$id))


# Plot ----
ggplot() +
  geom_polygon(data = hab.df, aes(x = long, y = lat, group = group,
                             fill = interaction(Group_, SubGroup)),
               color = 'black') +
  scale_fill_manual(values = c('orange1', 'orangered', 'yellow', 'lightblue', 'blue'),
                    labels = c('Mud', 'Muddy Sand', 'Sand', 'Gravelly Sand',
                               'Sandy Gravel')) +
  coord_map(xlim = c(-75.8145, -75.8095), ylim = c(38.6415, 38.649)) +
  geom_point(data = fish_pos, aes(x = lon, y = lat), alpha = 0.2) +
  geom_point(data = rec_pos, aes(x = Longitude, y = Latitude),
             pch = 24, lwd = 2, fill = 'green') +
  labs(x = 'Latitude', y = 'Longitude', fill = 'Bottom Type') +
  theme_bw() +
  theme(legend.justification = c(0,0), legend.position = c(0.05,0.05))
