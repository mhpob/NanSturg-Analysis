library(dplyr); library(sf)

# Create box to crop using well-known text
#   Decided not to do it this way, but keeping it here in case I want to in the future

# crop_wkt <- st_bbox(c(ymin = 38.223568, xmin = -75.969909,
#                       ymax = 38.730472, xmax = -75.552283),
#                     crs = st_crs(4269)) %>%
#   st_as_sfc() %>%
#   st_as_text()


nan <- st_read('manuscript/data/spatial/NHD_H_0208_HU4_GDB.gdb',
               layer = 'wbdhu10',
               query = "SELECT OGR_GEOM_WKT AS wkt
                        FROM wbdhu10
                        WHERE States LIKE 'D%'")

nan <- st_read('manuscript/data/spatial/NHD_H_0208_HU4_GDB.gdb',
               layer = 'nhdarea',
               wkt_filter = nan$wkt)



# Receiver sites
dnrec <- read.csv('manuscript/data/detections/past receiver locations.csv') %>%
  filter(year < 2019 & year > 2014) %>%
  mutate(lat_nudge = lat + (year - 2016.5) / 300,
         year = as.factor(year))

mdnr <- data.table::fread('manuscript/data/detections/sturgeon_detections.gz') %>%
  mutate(year = lubridate::year(date.local)) %>%
  filter(year < 2019 & year > 2014) %>%
  distinct(station, lat, long, year) %>%
  mutate(long_nudge = long + (year - 2016.5) / 250,
         year = as.factor(year))

rkm_lines <- st_read('manuscript/data_derived/rkm_lines.gpkg')


# Locations of labels
labels <- data.frame(
  long = c(-75.83, -75.77, -75.6, -75.65, -75.577, -75.635, -75.8),
  lat = c(38.27, 38.63, 38.6, 38.55, 38.633, 38.64, 38.695),
  labs = c('Lower Nanticoke', 'Marshyhope Creek', 'Upper Nanticoke',
           'Broad Creek', 'Deep Creek', 'Seaford, DE', 'Federalsburg, MD')
)



# Plotting
library(ggplot2); library(patchwork); library(ragg)

lower <- ggplot() +
  geom_sf(data = nan) +
  geom_sf(data = rkm_lines) +
  geom_point(data = mdnr, aes(x = long_nudge, y = lat, color = year),
             size = 3) +
  geom_sf_label(data = rkm_lines, aes(label = rkm), nudge_x = 0.02,
                label.size = unit(0, 'line'), label.padding = unit(0.1, 'line')) +
  coord_sf(xlim = c(-75.97, -75.74), ylim = c(38.2467, 38.55), expand = F) +
  geom_text(data = labels, aes(x = long, y = lat, label = labs)) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.position = 'none')


upper <- ggplot() +
  geom_sf(data = nan) +
  geom_sf(data = rkm_lines) +

  geom_point(data = dnrec, aes(x = long, y = lat_nudge, color = year),
             size = 3) +

  geom_point(data = mdnr, aes(x = long_nudge, y = lat, color = year),
             size = 3) +
  geom_sf_label(data = rkm_lines, aes(label = rkm), nudge_x = 0.01,
                label.size = unit(0, 'line'), label.padding = unit(0.1, 'line')) +
  coord_sf(label_axes = '-NE-',
           xlim = c(-75.83, -75.55), ylim = c(38.52, 38.7), expand = F) +
  geom_text(data = labels, aes(x = long, y = lat, label = labs),
             check_overlap = T) +
  labs(x = NULL, y = NULL, color = 'Year') +
  theme_bw()





# Make a polygon using a bounding box
crop_box <- st_bbox(c(ymin = 36.76498, xmin = -77.08675,
                      ymax = 39.72379, xmax = -74.84402),
                    crs = st_crs(4326)) %>%
  # turn into a simple features collection
  st_as_sfc()


# Import map, selecting only features that touch the crop box.
#   Do this by turning the box into well-known text

inset_map <- st_read('manuscript/data/spatial/natural earth/ne_10m_coastline.shp',
                     # turn box into well-known text
                     wkt_filter = st_as_text(crop_box))

plot(inset_map$geometry)


# Use the coastline (a linestring) to cut up the bbox polygon
inset_map <- crop_box %>%
  lwgeom::st_split(inset_map) %>%
  # Separate into individual features
  st_collection_extract() %>%
  # just so happens that features 5 (upper Potomac) and 2 (everything else) have
  #   what we need
  .[-c(2, 5),]


# Create data frame to hold labels
river_labels1 <- data.frame(
  long = c(-76.832102, -76.902022),
  lat = c(36.99268, 37.612137),
  labs = c('James', 'York')
)
river_labels2 <- data.frame(
  long = -75.65,
  lat = 38.46,
  labs = 'Nanticoke'
)


inset <- ggplot() +
  geom_sf(data = inset_map, fill = 'gray') +
  geom_text(data = river_labels1, aes(x = long, y = lat, label = labs), angle = -45,
            size = 8 / .pt) +
  geom_text(data = river_labels2, aes(x = long, y = lat, label = labs), angle = 45,
            size = 7 / .pt) +
  coord_sf(expand = F) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))
        # panel.border = element_rect(color = 'black', fill = NA))



########
### Need to layer the inset

library(cowplot)
lower_inset <- lower + draw_plot(inset, -76.0113, 38.4, 0.2, 0.149)



agg_png('manuscript/figures/map2.png', res = 600,
        width = 7.5, height = 3.65, scaling = .75, units = 'in')

lower_inset + upper & theme(plot.margin = margin(0, 0, 0, 0))
# ggsave('manuscript/figures/map.png', out, dpi = 600,
#        width = 7.5, height = 3.65,
#        scale = 1.75)
# width = 4500, height = 2189)



dev.off()

