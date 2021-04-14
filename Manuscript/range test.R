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
spec_sheet <- spec_sheet[-c(23,24),]
setnames(spec_sheet, tolower)


#ARG playing with this
vps <- vps[transmitter %in% spec_sheet$device]
##


vps <- vps[, c(1:3, 6:7)][spec_sheet[grepl('VR2W', device)], on = c(receiver = 'device')]
# vps <- vps[date_and_time__utc_ >= '2017-09-08 00:00:00']


vps[, ':='(trans_from = transmitter,
         station_to = name,
         lat_to = latitude,
         lon_to = longitude,
         name = NULL,
         latitude = NULL,
         longitude = NULL)]

vps <- vps[spec_sheet[grepl('A69', device), .(name, device, latitude, longitude)],
          on = c(trans_from = 'device')]

setnames(vps, c('date_and_time__utc_', 'name', 'latitude', 'longitude'),
         c('time','station_from', 'lat_from', 'lon_from'))
vps[, trans_from := NULL]
# vps <- unique(vps, by = c('time', 'station_to', 'station_from'))

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

# to_from <- vps[rec_dists, on = c('station_from', 'station_to')]



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
# to_from[, fake_end := time]
vps[, fake_end := time]

# setkey(to_from, time, fake_end)
setkey(vps, time, fake_end)
setkey(tide_lookup, start, end)

# to_from <- foverlaps(to_from, tide_lookup)[, fake_end := NULL]
vps <- foverlaps(vps, tide_lookup)[, fake_end := NULL]

# to_from[, id := rleid(start)]
vps[, id := rleid(start)]






k <- to_from[!id %in% c(1, 301), .N, by = .(id, stage, distance)]

m1 <- glm(N ~ stage*distance, family = 'poisson', data = k)

m2 <- glm(N ~ stage + distance, family = 'poisson', data = k)

anova(m1, m2, test = 'LRT')


## calc det freq














k <- split(vps, vps$id)

# p <- lapply(k, function(.) unique(., by = c('station_to', 'station_from'))[
#   , .(station_from, lat_from, lon_from, station_to, lat_to, lon_to)])
# p <- rbindlist(p, idcol = 'id')
# p[, id := as.integer(id)]
#
k <- lapply(k, function(.) xtabs(~ station_from + station_to, data = .))




kk <- lapply(k, function(.) .[rownames(.) %in% colnames(.), colnames(.) %in% rownames(.)])
kk <- lapply(kk, function(x){
  diag(x) <- apply(x, 1, max)
  x
})



kk <- lapply(kk, function(.) round(./diag(.), 2))

kk <- lapply(kk, data.table)

kk <- lapply(kk, function(.){
  .[rec_dists[station_from != 'Ref' & station_to != 'Ref'],
    on = c('station_from', 'station_to')][is.na(N), N := 0]

})





kk <- rbindlist(kk, idcol = 'id')
kk[, id := as.integer(id)]

kkk <- kk[unique(vps, by = 'id')[, .(stage, id)], on = 'id']


m1 <- glm(N ~ stage*distance, family = 'binomial', data = kkk)
m2 <- glm(N ~ stage+distance, family = 'binomial', data = kkk)

anova(m1, m2, test = 'LRT')
