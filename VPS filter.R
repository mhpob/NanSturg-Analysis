library(TelemetryR)
j <- vemsort('p:/obrien/biotelemetry/nanticoke/vps data/receiver')


library(dplyr); library(lubridate)

j <- j %>%
  mutate(date = ymd_hms(date.local)) %>%
  filter(date > ymd('2017-08-30'),
         date < ymd('2017-10-04'))


WT <- filter(j, grepl(16250, transmitter),
             grepl(124474, receiver)) %>%
  mutate(WT = 0.1569 * sensor.value - 5,
         floor = floor_date(date, unit = 'hour')) %>%
  group_by(floor) %>%
  summarize(mean = mean(WT),
            sd = sd(WT))
k <- filter(j, !grepl('16250|65003|65004|65005|65006|65007|65008|65010|65011|65012|
                      |65013|65014|65015', transmitter)) %>%
  group_by(transmitter) %>%
  summarize(n())

l <- filter(j, grepl(paste(c(seq(27543, 27547, 1), seq(26350, 26354, 1),
                         seq(23900, 23904, 1), seq(21063, 21072, 1)),
                         collapse = '|'),
                     transmitter))
m <- mutate(l, floor = floor_date(date, unit = 'hour'),
            hrs = hour(date))%>%
  distinct(transmitter, floor) %>%
  group_by(floor) %>%
  summarize(n = n())

library(ggplot2)
ggplot() + geom_line(data = WT, aes(floor, mean)) +
  ylim(c(16, 25.5)) +
  scale_x_datetime(limits = c(as.POSIXct('2017-08-30'),as.POSIXct('2017-10-04'))) +
  labs(x = '', y = 'Water Temperature') +
  theme_bw()

ggplot() + geom_col(data = m, aes(x = floor, y = n))+
  scale_x_datetime(limits = c(as.POSIXct('2017-08-30'), as.POSIXct('2017-10-04'))) +
  labs(x = '', y = 'Detections') +
  theme_bw()

ggplot() + geom_line(data = m, aes(x = hrs, y = n)) +
  labs(x = 'Hour of Day', y = 'Total Number of Detections') +
  theme_bw()



USGS.url <- paste('https://waterservices.usgs.gov/nwis/iv/?format=rdb',
        '&sites=01488500&parameterCd=00060&startDT=2017-08-27&endDT=2017-10-04',
        sep = '')

USGS <- read.delim(USGS.url, comment.char = '#', stringsAsFactors = F)
USGS <- USGS[-1, c(3,5)]
names(USGS) <- c('Date.Time', 'Discharge_m3s')

## Convert from ft^3/s to m^3/s
USGS$Discharge_m3s <- as.numeric(USGS$Discharge_m3s) * 0.028317

## Sort out the dates, round to the nearest 6min using lubridate package.
USGS$Date.Time <- ymd_hm(USGS$Date.Time, tz = 'America/New_York')
ggplot() + geom_line(data = USGS, aes(x = Date.Time, y = Discharge_m3s)) +
  scale_x_datetime(limits = c(as.POSIXct('2017-08-30'),as.POSIXct('2017-10-04'))) +
  labs(x = 'Date', y = 'Discharge (m^3/s)') +
  theme_bw()
