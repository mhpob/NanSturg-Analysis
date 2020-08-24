library(data.table)

# Load receiver rkm
mdnr <- fread('manuscript/data/mddnr_receiver_rkm.csv')
dnrec <- fread('manuscript/data/dnrec_receiver_rkm.csv')
dnrec[, ':='('station' = site,
             site = NULL,
             year = NULL,
             body = NULL)]


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
detections <- unique(detections, by = c('date.utc', 'transmitter', 'station'))


rkms <- rbind(mdnr, dnrec)
rkms <- unique(rkms, by = c('station', 'lat', 'long'))


test <- rkms[detections, on = 'station', allow.cartesian= T]
