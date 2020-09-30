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




library(ggplot2)

lower <-
  ggplot(data = nan) +
  geom_sf() +
  coord_sf(xlim = c(-75.97, -75.74), ylim = c(38.2467, 38.55), expand = F) +
  theme_bw()

upper <-
  ggplot(data = nan) +
  geom_sf() +
  coord_sf(label_axes = '-NE-',
           xlim = c(-75.83, -75.55), ylim = c(38.52, 38.7), expand = F) +
  theme_bw()


library(patchwork)
lower + upper
