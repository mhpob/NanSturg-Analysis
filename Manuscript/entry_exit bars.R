library(lubridate); library(data.table)

# Load receiver rkm
mdnr <- fread('manuscript/data/mddnr_receiver_rkm.csv')
dnrec <- fread('manuscript/data/dnrec_receiver_rkm.csv')
dnrec[, ':='('station' = site,
             site = NULL,
             year = NULL)]


# Load detections
m_dets <- fread('manuscript/data/detections/sturgeon_detections.gz')
m_dets <- m_dets[, date.utc := ymd_hms(date.utc)][
  , date.local := with_tz(date.utc, 'America/New_York')]

d_dets <- fread('manuscript/data/detections/de detections.csv',
                col.names = function(.) tolower(gsub('[( )]', '.', .)))
d_dets <- setNames(d_dets, c('date.utc', 'receiver', 'transmitter', 'station', 'lat', 'long'))
d_dets[, ':='(receiver = NULL,
              date.utc = ymd_hms(date.utc))][
                , date.local := with_tz(date.utc, 'America/New_York')]

# Combine
detections <- rbind(m_dets, d_dets)
detections <- unique(detections, by = c('date.utc', 'transmitter', 'lat', 'long'))
detections <- detections[, ':='(date = yday(date.local),
                                dummy.date = (yday(date.local) - 1) + as.Date('2014-01-01'),
                                year = as.factor(year(date.local)))]

#Fish 21067 died on Sept 26, 2018
detections <- detections[!(grepl('21067', transmitter) &
                           date.local >= '2018-09-27')]


rkms <- rbind(mdnr, dnrec)
rkms <- unique(rkms, by = c('station', 'lat', 'long'))


## leaving this object as "test" since there are still some station locations
## That are not specified.
test <- rkms[detections, on = 'station', allow.cartesian= T]
test <- test[!is.na(body)]



test <- unique(test, by = c('body', 'transmitter', 'date', 'year'))
# test <- test[, ':='(body = factor(body, ordered = T,
#                                   levels = c('Deep Creek', 'Broad Creek',
#                                              'Marshyhope Creek', 'Nanticoke River')),
#                     year = factor(year, ordered = T,
#                                   levels = c(2018, 2017, 2016, 2015, 2014)))]

pts <- test[, .(min(date),
         max(date)), by = c('transmitter', 'year', 'body')][
           , .(med_in = median(V1),
               med_out = median(V2)), by = c('year', 'body')]
pts <- melt(pts, c('year', 'body'),
            c('med_in', 'med_out'))
pts <- pts[, dummy.date := (value - 1) + as.Date('2014-01-01')]

library(ggplot2)
ggplot() +
  geom_linerange(data = test,
                 aes(y = dummy.date, x = body,
                      color = year),
                  stat = 'summary',
                  fun.min = 'min',
                  fun.max = 'max',
                  # Negative dodge properly orders the years
                  position = position_dodge(-0.3),
                  size = 1) +
  geom_point(data = pts,
             # dodging is determined by the most-recent group. Explicitly group by year
             aes(y = dummy.date, x = body,
                 color = year, shape = variable, group = year),
             position = position_dodge(-0.3),
             size = 4) +
  labs(x = NULL, y = NULL, color = NULL) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(test$body))) +
  scale_y_date(date_breaks = 'month', date_labels = '%B') +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 60),
        legend.position = c(0.25, 0.25)) +
  scale_shape(guide = 'none')
