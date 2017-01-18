library(TelemetryR); library(dplyr)

nan <- vemsort('p:/obrien/biotelemetry/detections/dnr/nanticoke')
mar <- vemsort('p:/obrien/biotelemetry/detections/dnr/marshyhope')
data <- rbind(nan, mar)

data <- filter(data, !transmitter %in% paste('A69-9001',
                                      c(26352, 27545, 26353, 26354, 27543,
                                        23901, 23902),
                                      sep = '-'),
               date.local >= '2015-08-24',
               date.local <= '2015-09-28') %>%
  arrange(transmitter, date.utc)

spl.data <- split(data, data$transmitter)

gen.movement <- function(data){
  if(dim(data)[1] <= 1){
    track <- data[1, 'station']
  }
  else{
      track <- data[1, c('station', 'date.local', 'lat', 'long')]
      for(i in seq(1, dim(data)[1] - 1)){
        if(data[i, 'station'] != data[i + 1, 'station']){
          track <- rbind(track, data[i + 1, c('station', 'date.local',
                                              'lat', 'long')])
        }
      }
  }
  track
}

j <- lapply(spl.data,gen.movement)
