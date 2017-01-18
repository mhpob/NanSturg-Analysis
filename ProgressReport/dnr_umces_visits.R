library(ggplot2); library(reshape2)

times <- read.csv(
  'p:/obrien/biotelemetry/nanticoke/progress reports/datesvisited.csv')
times$MD.DNR <- lubridate::mdy(times$MD.DNR)
times$UMCES <- lubridate::mdy(times$UMCES)
times <- melt(times, measure.vars = c('MD.DNR', 'UMCES'),
              variable.name = 'group', value.name = 'date', na.rm = T)

labs <- c('UMCES', 'MD.DNR')

ggplot() + geom_raster(data = times,
                       aes(x = date, y = group),
                       fill = 'darkgreen') +
  xlim(lubridate::ymd('2015-07-15'), lubridate::ymd('2015-10-02')) +
  scale_y_discrete(labels = rev(labs)) +
  theme_bw() +
  labs(x = 'Date', y = 'Group')
