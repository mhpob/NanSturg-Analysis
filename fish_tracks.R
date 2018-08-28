library(ggplot2); library(dplyr); library(sf)

# Import positions ----
all_pos <- read.csv(
  'p:/obrien/biotelemetry/nanticoke/vps results/positions/all-calc-positions.csv',
  stringsAsFactors = F) %>%
  mutate(DATETIME = lubridate::ymd_hms(DATETIME)) %>%
  rename_all(tolower)

fish_pos <- all_pos %>%
  filter(grepl('^\\d', all_pos$transmitter),
         hpe <= 10) %>%
  mutate(wk = lubridate::floor_date(datetime, unit = 'week')) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326, remove = F)


# Import bottom type polygons ----
habitat <- st_read(dsn ='C:/Users/secor/Downloads/2015 Atlantic Sturgeon Habitat Geodatabase and Report Nanticoke and Tributaries-2016-01-19/2015 Atlantic Sturgeon Habitat Geodatabase Nanticoke and Tributaries 01132016.gdb',
                   layer = 'RiverBed_Habitat_Polygons_CMECS_SC_01132016')

habitat <- habitat %>%
  # Select polygons in the Marshyhope
  filter(Location == 'Marshyhope Creek, MD') %>%
  # Slight manipulation of underlying data
  mutate(OBJECTID = as.character(OBJECTID),
         SubGroup = case_when(SubGroup == '<Null>' ~ '',
                              T ~ as.character(SubGroup))) %>%
  # Reproject to lonlat
  st_transform(4326) %>%
  st_crop(xmin = -75.8145, xmax = -75.8095, ymin = 38.6415, ymax = 38.649)

ggplot() +
  geom_sf(data = habitat, aes(fill = interaction(Group_, SubGroup)),
          color = 'black') +
  scale_fill_manual(values = c('orange1', 'orangered', 'yellow', 'lightblue',
                               'blue'),
                    labels = c('Mud', 'Muddy Sand', 'Sand', 'Gravelly Sand',
                               'Sandy Gravel')) +
  geom_path(data = fish_pos, aes(x = lon, y = lat)) +
  facet_wrap(~ transmitter, nrow = 2) +
  coord_sf(xlim = c(-75.8145, -75.8095), ylim = c(38.6422, 38.6486), expand = F) +
  scale_x_continuous(breaks = c(-75.81, -75.812, -75.814)) +
  labs(x = NULL, y = NULL, fill = 'Bottom Type') +
  theme_bw()


ggplot() +
  geom_sf(data = habitat, aes(fill = interaction(Group_, SubGroup)),
          color = 'black') +
  scale_fill_manual(values = c('orange1', 'orangered', 'yellow', 'lightblue',
                               'blue'),
                    labels = c('Mud', 'Muddy Sand', 'Sand', 'Gravelly Sand',
                               'Sandy Gravel')) +
  geom_path(data = fish_pos,
            aes(x = lon, y = lat, color = factor(wk)), lwd = .75) +
  facet_wrap(~transmitter, nrow = 2) +
  coord_sf(xlim = c(-75.8145, -75.8095), ylim = c(38.6422, 38.6486), expand = F) +
  scale_x_continuous(breaks = c(-75.81, -75.812, -75.814)) +
  labs(x = NULL, y = NULL, fill = 'Bottom Type', color = 'Week') +
  theme_bw()

