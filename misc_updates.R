library(lubridate); library(dplyr)


sharp <- read.csv('p:/obrien/biotelemetry/nanticoke/tidesunmoon.csv',
                  stringsAsFactors = F) %>%
  mutate(time = lubridate::ymd_hms(time))

dnr <- read.csv('p:/obrien/biotelemetry/nanticoke/dnrec sonde/dnr_2017.csv') %>%
  mutate(Date = lubridate::ymd(Date))
names(dnr) <- c('time', 'temp', 'ph', 'do', 'turb')
dnr <- reshape2::melt(dnr, id.vars = 'time',
                      variable.name = 'var', value.name = 'val')

rec_wt <- read.csv('p:/obrien/biotelemetry/nanticoke/vps data/receiver/VR2W_124474_20171006_1.csv') %>%
  rename_with(function(.) tolower(gsub('\\.\\.|ï', '', .))) %>%
  filter(grepl(16250, transmitter)) %>%
  mutate(date = ymd_hms(date.and.timeutc.),
         WT = 0.1569 * sensor.value - 5,
         floor = floor_date(date, unit = 'hour')) %>%
  group_by(floor) %>%
  summarize(mean = mean(WT),
            sd = sd(WT))

env <- rbind(sharp, dnr)

env <- env %>%
  filter(time > '2017-08-27',
         time < '2017-09-25')

fish_pos <- read.csv(
  'p:/obrien/biotelemetry/nanticoke/vps results/positions/all-calc-positions.csv',
  stringsAsFactors = F) %>%
  rename_all(tolower) %>%
  filter(grepl('^\\d', transmitter)) %>%
  mutate(datetime = lubridate::ymd_hms(datetime),
         datefloor = lubridate::floor_date(datetime, 'hour'))

pdat <- fish_pos%>%
  distinct(datefloor, transmitter) %>%
  group_by(datefloor)%>%
  summarize(n())

pdat2 <- env %>%
  filter(var == 'sun') %>%
  mutate(datefloor = lubridate::floor_date(time, 'day'),
         datefloor = case_when(val == 'set' ~ datefloor + days(1),
                               T ~ datefloor)) %>%
  reshape2::dcast(datefloor ~ val, value.var = 'time')


library(ggplot2)
ggplot() + geom_rect(data = pdat2, aes(xmin = as.POSIXct(rise, origin="1970-01-01"),
                                       xmax = as.POSIXct(set, origin="1970-01-01"),
                                       ymin = 0, ymax = Inf), fill = 'lightgray') +
  geom_col(data = pdat, aes(x = datefloor, y = `n()`)) +
  labs(x = NULL, y = 'Fish Detected/6hr') +
  theme_bw()


det_plot <- ggplot() +
  geom_col(data = pdat, aes(x = datefloor, y = `n()`)) +
  scale_x_datetime(limits = as.POSIXct(c('2017-08-30', '2017-10-04')),
                   expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 7), expand = F) +
  labs(x = NULL, y = paste('Fish detected\n(per hour)')) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,1,1), 'mm'))

wt_plot <- ggplot() +
  geom_line(data = rec_wt, aes(x = floor, y = mean)) +
  ylim(c(17, 25.5)) +
  labs(x = NULL, y = paste('Water temperature\n(°C)')) +
  scale_x_datetime(limits = as.POSIXct(c('2017-08-30', '2017-10-04')),
                   expand = c(0, 0)) +
  theme_bw() +
  theme(plot.margin = unit(c(0, 0, 0, 1), 'mm'))

library(patchwork); library(ragg)

agg_tiff('manuscript/figures/figure4.tif',
         width = 1950,
         height = 985,
         res = 600,
         compression = 'lzw',
         scaling = 0.5)
det_plot / wt_plot +
  plot_layout(heights = c(2, 1))
dev.off()



habitat <- sf::read_sf(dsn ='C:/Users/secor/Downloads/2015 Atlantic Sturgeon Habitat Geodatabase and Report Nanticoke and Tributaries-2016-01-19/2015 Atlantic Sturgeon Habitat Geodatabase Nanticoke and Tributaries 01132016.gdb',
                   layer = 'RiverBed_Habitat_Polygons_CMECS_SC_01132016') %>%
  filter(Location == 'Marshyhope Creek, MD') %>%
  mutate(sed_type = case_when(SubGroup == '<Null>' ~ Group_,
                              T ~ SubGroup)) %>%
  # Reproject from UTM to longlat (epsg = 4326)
  sf::st_transform(4326)


# Load position data
fish_pos <- read.csv(
  'p:/obrien/biotelemetry/nanticoke/vps results/positions/all-calc-positions.csv',
  stringsAsFactors = F) %>%
  rename_all(tolower) %>%
  filter(grepl('^\\d', transmitter)) %>%
  mutate(datetime = lubridate::ymd_hms(datetime),
         datefloor = lubridate::floor_date(datetime, '6hour')) %>%
  sf::st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
  sf::st_join(habitat, join = sf::st_intersects)


pdat <- data.frame(fish_pos) %>%
  # distinct(datefloor, transmitter, sed_type) %>%
  group_by(datefloor, sed_type) %>%
  summarize(n())

ggplot() + geom_rect(data = pdat2, aes(xmin = as.POSIXct(rise, origin="1970-01-01"),
                                       xmax = as.POSIXct(set, origin="1970-01-01"),
                                       ymin = 0, ymax = Inf), fill = 'lightgray') +
  geom_col(data = pdat, aes(x = datefloor, y = `n()`, fill = sed_type)) +
  scale_fill_manual(values = c('orange1', 'orangered', 'yellow', 'blue')) +
  labs(x = NULL, y = 'Detections/6hr', fill = NULL) +
  theme_bw()

pdat2 <- env %>%
  filter(var == 'sun',
         time < '2017-09-25') %>%
  mutate(datefloor = lubridate::floor_date(time, 'day'),
         datefloor = case_when(val == 'set' ~ datefloor + days(1),
                               T ~ datefloor)) %>%
  reshape2::dcast(datefloor ~ val, value.var = 'time')


library(ggplot2)
ggplot() + geom_rect(data = pdat2, aes(xmin = as.POSIXct(rise, origin="1970-01-01"),
                                       xmax = as.POSIXct(set, origin="1970-01-01"),
                                       ymin = 0, ymax = Inf), fill = 'lightgray') +
  geom_col(data = pdat, aes(x = datefloor, y = `n()`)) +
  labs(x = NULL, y = 'Detections/6hr') +
  theme_bw()



pdat <- data.frame(fish_pos)%>%
  distinct(datefloor, transmitter) %>%
  group_by(datefloor)%>%
  summarize(n())

pdat3 <- env %>%
  filter(var == 'tide',
         time < '2017-09-25') %>%
  mutate(ind = c(rep(seq(1, 56, 1), each = 2), 57))%>%
  reshape2::dcast(ind ~ val, value.var = 'time')

ggplot() + geom_rect(data = pdat3, aes(xmin = as.POSIXct(Low, origin="1970-01-01"),
                                       xmax = as.POSIXct(High, origin="1970-01-01"),
                                       ymin = 0, ymax = Inf), fill = 'lightgray') +
  geom_col(data = pdat, aes(x = datefloor, y = `n()`)) +
  labs(x = NULL, y = 'Fish Detected/6hr') +
  theme_bw()

pdat <- data.frame(fish_pos) %>%
  distinct(datefloor, transmitter, sed_type) %>%
  group_by(datefloor, sed_type) %>%
  summarize(n())
ggplot() + geom_rect(data = pdat3, aes(xmin = as.POSIXct(Low, origin="1970-01-01"),
                                       xmax = as.POSIXct(High, origin="1970-01-01"),
                                       ymin = 0, ymax = Inf), fill = 'lightgray') +
  geom_col(data = pdat, aes(x = datefloor, y = `n()`, fill = sed_type)) +
  scale_fill_manual(values = c('orange1', 'orangered', 'yellow', 'blue')) +
  labs(x = NULL, y = 'Detections/6hr', fill = NULL) +
  theme_bw()




# Start anew------
library(lubridate); library(dplyr)


sharp <- read.csv('p:/obrien/biotelemetry/nanticoke/tidesunmoon.csv',
                  stringsAsFactors = F) %>%
  mutate(time = lubridate::ymd_hms(time))

dnr <- read.csv('p:/obrien/biotelemetry/nanticoke/dnrec sonde/dnr_2017.csv') %>%
  mutate(Date = lubridate::ymd(Date))
names(dnr) <- c('time', 'temp', 'ph', 'do', 'turb')
dnr <- reshape2::melt(dnr, id.vars = 'time',
                      variable.name = 'var', value.name = 'val')

env <- rbind(sharp, dnr)

env <- env %>%
  filter(time > '2017-08-27',
         time < '2017-09-25')

habitat <- sf::read_sf(dsn ='C:/Users/secor/Downloads/2015 Atlantic Sturgeon Habitat Geodatabase and Report Nanticoke and Tributaries-2016-01-19/2015 Atlantic Sturgeon Habitat Geodatabase Nanticoke and Tributaries 01132016.gdb',
                       layer = 'RiverBed_Habitat_Polygons_CMECS_SC_01132016') %>%
  filter(Location == 'Marshyhope Creek, MD') %>%
  mutate(sed_type = case_when(SubGroup == '<Null>' ~ Group_,
                              T ~ SubGroup)) %>%
  # Reproject from UTM to longlat (epsg = 4326)
  sf::st_transform(4326)


# Load position data
fish_pos <- read.csv(
  'p:/obrien/biotelemetry/nanticoke/vps results/positions/all-calc-positions.csv',
  stringsAsFactors = F) %>%
  rename_all(tolower) %>%
  filter(grepl('^\\d', transmitter)) %>%
  mutate(datetime = lubridate::ymd_hms(datetime),
         datefloor = lubridate::floor_date(datetime, '6hour')) %>%
  sf::st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
  sf::st_join(habitat, join = sf::st_intersects)

# Det/fish/6hr
pdat <- data.frame(fish_pos) %>%
  group_by(datefloor, transmitter) %>%
  summarize(n())

library(ggplot2)
ggplot() + geom_boxplot(data = pdat, aes(x = datefloor, y = `n()`,
                                         group = datefloor)) +
  labs(x = NULL, y = 'Detections per fish per 6 hours') +
  theme_bw()

tide <- env %>%
  filter(var == 'tide',
         time < '2017-09-25') %>%
  mutate(ind = c(rep(seq(1, 56, 1), each = 2), 57))%>%
  reshape2::dcast(ind ~ val, value.var = 'time')

ggplot() + geom_rect(data = tide, aes(xmin = as.POSIXct(Low, origin="1970-01-01"),
                                       xmax = as.POSIXct(High, origin="1970-01-01"),
                                       ymin = 0, ymax = Inf), fill = 'lightgray') +
  geom_boxplot(data = pdat, aes(x = datefloor, y = `n()`,
                                group = datefloor)) +
  labs(x = NULL, y = 'Detections per fish per 6 hours') +
  theme_bw()


sun <- env %>%
  filter(var == 'sun') %>%
  mutate(datefloor = lubridate::floor_date(time, 'day'),
         datefloor = case_when(val == 'set' ~ datefloor + days(1),
                               T ~ datefloor)) %>%
  reshape2::dcast(datefloor ~ val, value.var = 'time')

ggplot() + geom_rect(data = sun, aes(xmin = as.POSIXct(rise, origin="1970-01-01"),
                                       xmax = as.POSIXct(set, origin="1970-01-01"),
                                       ymin = 0, ymax = Inf), fill = 'lightgray') +
  geom_boxplot(data = pdat, aes(x = datefloor, y = `n()`,
                                group = datefloor)) +
  labs(x = NULL, y = 'Detections per fish per 6 hours') +
  theme_bw()


