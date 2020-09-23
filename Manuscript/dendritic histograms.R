library(data.table)

# Load receiver rkm
mdnr <- fread('manuscript/data/mddnr_receiver_rkm.csv')
dnrec <- fread('manuscript/data/dnrec_receiver_rkm.csv')
dnrec[, ':='('station' = site,
             site = NULL,
             year = NULL)]


# Load detections
m_dets <- fread('manuscript/data/detections/sturgeon_detections.gz')
m_dets <- m_dets[, date.utc := lubridate::ymd_hms(date.utc)][,
                   date.local := lubridate::with_tz(date.utc, 'America/New_York')]

d_dets <- fread('manuscript/data/detections/de detections.csv',
                col.names = function(.) tolower(gsub('[( )]', '.', .)))
d_dets <- setNames(d_dets, c('date.utc', 'receiver', 'transmitter', 'station', 'lat', 'long'))
d_dets[, ':='(receiver = NULL,
              date.utc = lubridate::ymd_hms(date.utc))][,
              date.local := lubridate::with_tz(date.utc, 'America/New_York')]

# Combine
detections <- rbind(m_dets, d_dets)
detections <- unique(detections, by = c('date.utc', 'transmitter', 'lat', 'long'))


rkms <- rbind(mdnr, dnrec)
rkms <- unique(rkms, by = c('station', 'lat', 'long'))


## leaving this object as "test" since there are still some station locations
## That are not specified.
test <- rkms[detections, on = 'station', allow.cartesian= T]


library(ggplot2); library(patchwork)
dendr <- function(month){
nan <- ggplot(data = test[!is.na(lat) &
                            month(date.local) == month &
                            grepl('Nan', body),]) +
  geom_histogram(aes(x = rkm_body_mouth, group = transmitter),
                 breaks = seq(0, 75, 3), color = 'white') +
  labs(x = 'River kilometer', y = 'Detection count') +
  scale_x_continuous(limits = c(0, 75), expand = c(0, 0))

marsh <- ggplot(data = test[!is.na(lat) &
                              month(date.local) == month &
                              grepl('Marsh', body),]) +
  geom_histogram(aes(x = rkm_body_mouth, group = transmitter),
                 breaks = seq(0, 30, 3), color = 'white') +
  labs(x = NULL, y = NULL)+
  scale_x_continuous(limits = c(0, 30), expand = c(0, 0))


broad <- ggplot(data = test[!is.na(lat) &
                              month(date.local) == month &
                              grepl('Broad', body),]) +
  geom_histogram(aes(x = rkm_body_mouth, group = transmitter),
                 breaks = seq(0, 12, 3), color = 'white') +
  labs(x = NULL, y = NULL)+
  scale_x_continuous(limits = c(0, 12), expand = c(0, 0))


deep <- ggplot(data = test[!is.na(lat) &
                             month(date.local) == month &
                             grepl('Deep', body),]) +
  geom_histogram(aes(x = rkm_body_mouth, group = transmitter),
                 breaks = seq(0, 3, 3), color = 'white') +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(limits = c(0, 3), expand = c(0, 0))



design <- c(
  area(t = 1, r = 100, b = 33, l = 62),
  area(t = 34, r = 100, b = 66, l = 0),
  area(t = 67, r = 92, b = 99, l = 81),
  area(t = 67, r = 100, b = 99, l = 94)
)

marsh + nan + broad + deep + plot_layout(design = design) +
  plot_annotation(title = month.name[month],
                  theme = theme(plot.title = element_text(size = 18))) &
  theme_bw()
}

dendr(9)
