library(ggplot2); library(dplyr)

sonde <- 'p:/obrien/biotelemetry/nanticoke/dnrec sonde/'
sonde <- paste0(sonde, list.files(path = sonde, pattern = '*.csv'))
sonde <- lapply(sonde, FUN = read.csv, stringsAsFactors = F)
sonde <- do.call(rbind.data.frame, sonde)
names(sonde) <- c('Date.Time', 'Temp', 'Cond', 'Sal', 'pH', 'pH.mv', 'DO.pct',
                  'DO.mg_l', 'Batt', 'Station', 'Lat', 'Long')
sonde$pred.growth <- TelemetryR::sturgrow(sonde$Temp, sonde$Sal, sonde$DO.pct)
sonde <- reshape2::melt(sonde, id.vars = c('Date.Time', 'Station'),
                        measure.vars = c('Temp', 'Cond', 'Sal', 'pH', 'DO.pct',
                                         'DO.mg_l', 'pred.growth'),
                        na.rm = T)

sonde <- sonde %>%
  filter(variable %in% c('Temp', 'Sal', 'pH', 'DO.pct', 'pred.growth')) %>%
  mutate(Date.Time = lubridate::mdy_hms(Date.Time),
         Date = lubridate::floor_date(Date.Time, 'day')) %>%
  group_by(Date, Station, variable) %>%
  summarize(mean = mean(value),
            min = min(value),
            max = max(value))

# Ribbon min/mean/max
ggplot(data = sonde) +
  geom_ribbon(aes(x = Date, ymin = min, ymax = max,
                  fill = Station), alpha = 0.5) +
  geom_line(aes(x = Date, y = mean, color = Station)) +
  facet_wrap(~variable, scales = 'free_y')

# Line min/mean/max
ggplot(data = sonde) +
  geom_line(aes(x = Date, y = min, color = Station), linetype = 'dotted') +
  geom_line(aes(x = Date, y = mean, color = Station), linetype ='solid') +
  geom_line(aes(x = Date, y = max, color = Station), linetype = 'dotted') +
  facet_wrap(~variable, scales = 'free_y')


# Plot station vs station
SvS <- reshape2::melt(sonde, id.vars = c('Date', 'Station', 'variable'),
                       measure.vars = c('mean', 'min', 'max'),
                       variable.name = 'Type')
SvS <- reshape2::dcast(SvS, Date + variable + Type ~ Station,
                       value.var = 'value')

ggplot(data = SvS, aes(x = Ferry, y = Marshyhope, color = Type)) +
  geom_point() +
  stat_smooth(method = 'lm', se = F) +
  facet_wrap(~variable, scales = 'free')


