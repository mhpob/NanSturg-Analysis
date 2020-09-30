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


library(ggplot2)

lower <- ggplot() +
  geom_sf(data = nan) +
  coord_sf(xlim = c(-75.97, -75.74), ylim = c(38.2467, 38.55), expand = F) +
  geom_point(data = mdnr, aes(x = long_nudge, y = lat, color = year),
             size = 5) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.position = 'none')

upper <- ggplot() +
  geom_sf(data = nan) +
  coord_sf(label_axes = '-NE-',
           xlim = c(-75.83, -75.55), ylim = c(38.52, 38.7), expand = F) +
  geom_point(data = dnrec, aes(x = long, y = lat_nudge, color = year),
             size = 5) +
  geom_point(data = mdnr, aes(x = long_nudge, y = lat, color = year),
             size = 5) +
  labs(x = NULL, y = NULL, color = 'Year') +
  theme_bw()
########
### Need to annotate: Federalsburg, Seaford, Lower Nanticoke, Upper Nanticoke,
###     Marsyhhope, Broad, Deep
### Need to create: inset, labeled with James and York


library(patchwork)
lower + upper


