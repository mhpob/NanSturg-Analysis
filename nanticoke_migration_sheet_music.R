library(TelemetryR);library(ggplot2); library(lubridate); library(dplyr)

dnr_trans <- c(23900, 23901, 23902, 23903, 23904,
               26350, 26351, 26352, 26353, 26354,
               27543, 27544, 27545, 27546, 27547)

dnr_det <- c('p:/obrien/biotelemetry/detections/dnr/marshyhope',
             'p:/obrien/biotelemetry/detections/dnr/nanticoke')
dnr_det <- lapply(dnr_det, vemsort)
dnr_det <- do.call(rbind, dnr_det)
dnr_det <- filter(dnr_det, trans.num %in% dnr_trans)

dnr_det$l.system <- ifelse(grepl('Broad', dnr_det$station),
                         'Broad Creek',
                  ifelse(grepl('Marsh', dnr_det$station),
                         'Marshyhope',
                  ifelse(grepl('Nanti', dnr_det$station) |
                           dnr_det$station %in% c('Seaford Draw Bridge',
                                                  'Woodland Ferry'),
                         'Nanticoke', dnr_det$station)))

dnr_det$s.system <- ifelse(grepl('Brook', dnr_det$station), 'Mid-Marshyhope',
                    ifelse(grepl('Grey|Fook', dnr_det$station),
                           'Upper Marshyhope',
                    ifelse(grepl('Lewis|Chapter|mouth', dnr_det$station),
                           'Lower Nanticoke',
                    ifelse(grepl('Ferry Point', dnr_det$station),
                           'Mid-Nanticoke',
                    ifelse(grepl('Sharp|Seaford|Woodland', dnr_det$station),
                           'Upper Nanticoke', dnr_det$l.system)))))

dnr_det_lsys <- dnr_det %>%
  mutate(date.floor = lubridate::floor_date(date.local, unit = 'day')) %>%
  distinct(date.floor, trans.num, l.system) %>%
  arrange(trans.num)


ggplot() + geom_raster(data = dnr_det_lsys,
                       aes(x = date.floor, y = as.factor(trans.num),
                           fill = system)) +
  labs(x = 'Date', y = 'Transmitter', fill = 'System') +
  scale_x_datetime(limits = c(lubridate::ymd_hms('2015-06-15 00:00:00'),
                   lubridate::ymd_hms('2015-10-20 00:00:00')))


dnr_det_ssys <- dnr_det %>%
  mutate(date.floor = lubridate::floor_date(date.local, unit = 'day')) %>%
  distinct(date.floor, trans.num, s.system) %>%
  arrange(trans.num)

nan.cols <- colorRampPalette(c('green', 'darkgreen'))(3)
marsh.cols <- colorRampPalette(c('lightblue', 'blue'))(2)
other.cols <- colorRampPalette(c('red', 'violet'))(2)

cols <- c('Upper Nanticoke' = nan.cols[1], 'Mid-Nanticoke' = nan.cols[2],
          'Lower Nanticoke' = nan.cols[3], 'Upper Marshyhope' = marsh.cols[1],
          'Mid-Marshyhope' = marsh.cols[2], 'Broad Creek' = other.cols[1],
          'Deep Creek' = other.cols[2])

ggplot() + geom_raster(data = dnr_det_ssys,
                       aes(x = date.floor, y = as.factor(trans.num),
                           fill = s.system)) +
  labs(x = 'Date (2015)', y = 'Transmitter', fill = 'System') +
  scale_fill_manual(values = cols, breaks = c('Upper Nanticoke', 'Mid-Nanticoke',
                               'Lower Nanticoke', 'Upper Marshyhope',
                               'Mid-Marshyhope', 'Broad Creek', 'Deep Creek')) +
  scale_x_datetime(limits = c(lubridate::ymd_hms('2015-06-15 00:00:00'),
                              lubridate::ymd_hms('2015-10-20 00:00:00')))
