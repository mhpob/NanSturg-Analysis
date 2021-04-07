library(data.table)

vps_files <- list.files('p:/obrien/biotelemetry/nanticoke/vps data/receiver',
                        pattern = '*.csv',
                        full.names = TRUE)

vps_files <- lapply(vps_files, fread,
                    fill = T,
                    col.names = function(.) tolower(gsub('[ ()]', '_', .)))

vps <- rbindlist(vps_files)



library(readxl)

spec_sheet <- read_excel('p:/obrien/biotelemetry/nanticoke/vps results/vps-nanticokeriver-01.xls',
                         skip = 1,
                         sheet = 'Stations')
setDT(spec_sheet)

spec_sheet[, c('Name', 'Latitude', 'Longitude') :=
             lapply(.SD,
                    function(.)
                      .[nafill(replace(.I, is.na(.), NA), 'locf')]
             ),
           .SDcols = c('Name', 'Latitude', 'Longitude')]

spec_sheet <- spec_sheet[!is.na(Name)]
setnames(spec_sheet, tolower)


#ARG playing with this
vps[transmitter %in% spec_sheet$device]
##


vps <- vps[spec_sheet[grepl('VR2W', device)], on = c(receiver = 'device'),
         allow.cartesian = T]
vps <- vps[date_and_time__utc_ %between% list(`start time`, `end time`)]


vps[, ':='(trans_from = transmitter,
         station_to = name,
         lat_to = i.latitude,
         lon_to = i.longitude)]
vps[, c(2:19) := NULL]

vps <- vps[spec_sheet[grepl('A69', device), .(name, device, latitude, longitude)],
          on = c(trans_from = 'device')]

setnames(vps, c('date_and_time__utc_', 'name', 'latitude', 'longitude'),
         c('time','station_from', 'lat_from', 'lon_from'))
vps[, trans_from := NULL]
vps <- unique(vps, by = c('time', 'station_to', 'station_from'))

library(sf)
rec_dists <- st_as_sf(spec_sheet,
              coords = c('longitude', 'latitude'),
              crs = 4326)

rec_dists <- st_distance(rec_dists)
rec_dists <- as.data.table(rec_dists)

setnames(rec_dists, spec_sheet$name)
rec_dists[, station_from := spec_sheet$name]

rec_dists <- melt(rec_dists, id.vars = 'station_from',
           variable.name = 'station_to', value.name = 'distance')
rec_dists <- unique(rec_dists, by = c('station_from', 'station_to'))

to_from <- vps[rec_dists, on = c('station_from', 'station_to'), nomatch = 0]



#### manip tides
tides <- fread('p:/obrien/biotelemetry/nanticoke/tidesunmoon.csv')

tides <- tides[var == 'tide']
tides[, ':='(start = time - lubridate::period('1H 33M 7.5S'),
             end = time + lubridate::period('1H 33M 7.5S'),
             stage = tolower(val))]

tide_lookup <- copy(tides)[, ':='(start = end,
                                  end = start[.I + 1],
                                  stage = fifelse(stage == 'high', 'ebb', 'flood'))]

tide_lookup <- rbind(tide_lookup,
                     tides)

tide_lookup <- tide_lookup[complete.cases(tide_lookup), .(stage, start, end)]

#### join tides
to_from[, fake_end := time]

setkey(to_from, time, fake_end)
setkey(tide_lookup, start, end)

to_from <- foverlaps(to_from, tide_lookup)[, fake_end := NULL]

to_from[, id := rleid(start)]

## calc det freq


k <- split(to_from, to_from$id)

p <- lapply(k, function(.) unique(., by = c('station_to', 'station_from'))[
  , .(station_from, lat_from, lon_from, station_to, lat_to, lon_to)])
p <- rbindlist(p, idcol = 'id')
p[, id := as.integer(id)]

k <- lapply(k, function(.) xtabs(~ station_from + station_to, data = .))

k <- lapply(k, function(.) .[rownames(.) %in% colnames(.), colnames(.) %in% rownames(.)])


k <- lapply(k, function(.) round(./diag(.), 2))

k <- lapply(k, data.table)
k <- rbindlist(k, idcol = 'id')
k[, id := as.integer(id)]

j <- p[k, on = c('id', 'station_from', 'station_to')]## doesnt work, need two stages metinks.


k <- k[unique(to_from, by = 'id')[, .(stage, id)], on = 'id']


p <- unique(to_from, by = c('id', 'station_from', 'station_to'))


pp <- p[k, on = c('id', 'stage', 'station_from', 'station_to')]




k <- k[rec_dists, on = c('station_from', 'station_to'), nomatch = 0]

kk <- k[unique(to_from, by = c('station_to', 'station_from'))[
  , .(station_from, lat_from, lon_from, station_to, lat_to, lon_to)],
  on = c('station_from', 'station_to')]





