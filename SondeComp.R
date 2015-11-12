library(ggplot2); library(dplyr)

sonde <- 'p:/obrien/biotelemetry/nanticoke/dnrec sonde/'
sonde <- paste0(sonde, list.files(path = sonde, pattern = '*.csv'))
sonde <- lapply(sonde, FUN = read.csv, stringsAsFactors = F)
sonde <- do.call(rbind.data.frame, sonde)
names(sonde) <- c('Date.Time', 'Temp', 'Cond', 'Sal', 'pH', 'pH.mv', 'DO.pct',
                  'DO.mg_l', 'Batt', 'Station', 'Lat', 'Long')
sonde$pred.growth <- TelemetryR::sturgrow(sonde$Temp, sonde$Sal, sonde$DO.pct)
test <- reshape2::melt(sonde, id.vars = c('Date.Time', 'Station'),
                       measure.vars = c('Temp', 'Cond', 'Sal', 'pH', 'DO.pct',
                                        'DO.mg_l', 'pred.growth'))

test <- test %>%
  mutate(Date.Time = lubridate::mdy_hms(Date.Time),
         Date = lubridate::floor_date(Date.Time, 'day')) %>%
  group_by(Date, Station, variable) %>%
  summarize(mean = mean(value),
            min = min(value),
            max = max(value))

test <- reshape2::melt(test, id.vars = c('Date', 'Station', 'variable'),
                       measure.vars = c('mean', 'min', 'max'),
                       variable.name = 'Type')



ggplot() + geom_line(data = test, aes(x = Date, y = value, color = Station,
                                      linetype = Type)) +
  facet_wrap(~variable, scales = 'free_y')


