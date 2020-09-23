library(lubridate); library(ctmm); library(dplyr)


all_pos <- read.csv(
  'p:/obrien/biotelemetry/nanticoke/vps results/positions/all-calc-positions.csv',
  stringsAsFactors = F) %>%
  rename_all(tolower) %>%
  mutate(datetime = ymd_hms(datetime))

fish_pos <- all_pos %>%
  filter(grepl('^\\d', all_pos$transmitter),
         hpe < 10) %>%
  select(individual.local.identifier = transmitter,
         timestamp = datetime,
         location.lat = lat,
         location.long = lon,
         eobs.horizontal.accuracy.estimate = hpe)


fish <- split(fish_pos, fish_pos$individual.local.identifier)
fish <- fish[[7]]

fish <- as.telemetry(fish)

vg.fish <- variogram(fish)
plot(vg.fish)
plot(vg.fish, fraction = 0.005)

pg.fish <- periodogram(fish)
plot(pg.fish, diagnostic = T)


variogram.fit(vg.fish)


PROTO <- ctmm(mean="periodic", period=c(1 %#% "day",1 %#% "month"))
GUESS <- ctmm.guess(k, PROTO, vg.fish, interactive = F)
control <- list(method="pNewton", cores=2) # CRAN policy limits to 2 processes
FITS <- ctmm.select(k,GUESS,verbose=TRUE,control=control)
## Nyquist frequency estimated at harmonic 3 88.5917638888889 of the period.

