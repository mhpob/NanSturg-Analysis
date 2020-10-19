library(lubridate); library(data.table)

# Load receiver rkm
mdnr <- fread('manuscript/data_derived/mddnr_receiver_rkm.csv')
dnrec <- fread('manuscript/data_derived/dnrec_receiver_rkm.csv')
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
test[, body := fcase(grepl('Nanti', body), 'Nanticoke R.',
                     grepl('Marsh', body), 'Marshyhope Cr.',
                     grepl('Broad', body), 'Broad Cr.',
                     grepl('Deep', body), 'Deep Cr.')]
test <- test[, body := factor(body, ordered = T,
                              levels = c('Nanticoke R.', 'Marshyhope Cr.',
                                         'Broad Cr.', 'Deep Cr.'))]


test <- test[year != 2014]

pts <- test[, .(min(date),
         max(date)), by = c('transmitter', 'year', 'body')][
           , .(med_in = median(V1),
               med_out = median(V2)), by = c('year', 'body')]
pts <- melt(pts, c('year', 'body'),
            c('med_in', 'med_out'))
pts <- pts[, dummy.date := (value - 1) + as.Date('2014-01-01')]

library(ggplot2)
fish <- ggplot() +
  geom_linerange(data = test,
                 aes(y = dummy.date, x = body,
                      color = year, linetype = body),
                  stat = 'summary',
                  fun.min = 'min',
                  fun.max = 'max',
                  # Negative dodge properly orders the years
                  position = position_dodge(-0.75),
                  size = 1) +
  geom_point(data = pts,
             # dodging is determined by the most-recent group. Explicitly group by year
             aes(y = dummy.date, x = body,
                 color = year, shape = variable, group = year),
             position = position_dodge(-0.75),
             size = 4) +
  labs(x = NULL, y = NULL, color = NULL, shape = NULL) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(test$body))) +
  scale_y_date(date_breaks = 'month', date_labels = '%B',
               limits = c(as.Date('2014-05-01'), as.Date('2014-11-01')),
               expand = c(0, 0)) +
  scale_color_manual(values = c('#785EF0', '#DC267F', '#FE6100', '#FFB000')) +
  scale_linetype_manual(values  = c('solid', 'dotdash', 'dashed', 'dotted'),
                        guide = 'none') +
  scale_shape(guide = 'none') +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 45),
        axis.text.x = element_blank(),
        legend.position = c(0.1, 0.3),
        legend.background = element_blank(),
        plot.margin = margin(0, 2, 0, 0))


# Add sonde data
mdnr_sonde <- fread('manuscript/data/sonde_data_marshyhope.csv')
setnames(mdnr_sonde, c('date', 'do_mgl', 'temp_c'))
mdnr_sonde[, station := 'marshyhope']

mg2pct <- function(do_mgl, altitude, temperature){
  # copied from here:https://www.waterontheweb.org/under/waterquality/oxygen.html
  P_rel<- exp(5.25 * log(1 - (altitude / 44.3)))
  P_wv <- exp(11.8571 -
                (3840.7 / (temperature + 273.15)) -
                (216961 / (temperature + 273.15) ^ 2))
  theta <- 0.000975 - (1.426e-5 * temperature) + (6.436e-8 * temperature ^ 2)

  C_star <- exp(7.7117 - 1.31403 * log(temperature + 45.93))

  Cp <- C_star * P_rel * (
    ((1 - P_wv / P_rel) * (1 - theta * P_rel)) /
      ((1 - P_wv) * (1 - theta))
  )

  (100 * do_mgl) / Cp
}

mdnr_sonde[, do_pct := mg2pct(do_mgl, 0, temp_c)]

dnrec_sonde <- fread('manuscript/data_derived/dnrec_wq_aggregated.csv')

sonde <- rbind(mdnr_sonde, dnrec_sonde[, .(station, date, do_mgl, do_pct, temp_c)])

# Manually remove bad data
sonde[grepl('broad', station) & date %between% c('2015-08-18', '2015-10-22'),
      ':='(do_pct = NA, do_mgl = NA)]
sonde[grepl('marsh', station) & date %between% c('2015-09-15', '2015-10-22'),
      ':='(do_pct = NA, do_mgl = NA)]
sonde[grepl('seaf', station) & date %between% c('2016-10-27', '2016-11-06'),
      ':='(do_pct = NA, do_mgl = NA)]
sonde[grepl('wood', station) & date %between% c('2018-09-15', '2018-09-27'),
      ':='(do_pct = NA, do_mgl = NA)]

sonde <- melt(sonde, id.vars = c('date', 'station'))
sonde[, dummy.date := (yday(date) - 1) + as.Date('2014-01-01')]

sonde <- sonde[grepl('pct|temp', variable) & station != 'hatchery']

sonde[, variable := fcase(variable == 'do_pct', 'Dissolved oxygen
(% saturation)',
                          variable == 'temp_c', 'Temperature (°C)')]


wq <-
  ggplot(data = sonde) +
  geom_line(aes(x = dummy.date,
                y = value, color = as.factor(year(date)),
                linetype = station),
            size = 0.5) +
  scale_x_date(date_breaks = 'month', date_labels = '%B',
               limits = c(as.Date('2014-05-01'), as.Date('2014-11-01')),
               expand = c(0, 0)) +
  scale_color_manual(values = c('#785EF0', '#DC267F', '#FE6100', '#FFB000')) +

  scale_linetype_manual(values  = c('dashed', 'dotdash', 'dotted', 'solid')) +
  facet_wrap(~ variable, scales = 'free_y', ncol = 1, strip.position = 'left') +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(strip.placement = 'outside',
        strip.background = element_blank(),
        legend.position = 'none',
        plot.margin = margin(0, 2, 0, 5),
        axis.text.x = element_text(angle = 25, hjust = 1),
        axis.title.y = element_text(size = 7))


library(cowplot)
library(ragg)

agg_png('manuscript/figures/entry_exit.png',
        width = 2250, height = 1406, res = 600, scaling = 0.5)

plot_grid(fish, wq, ncol = 1)

dev.off()

