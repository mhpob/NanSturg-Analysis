library(lubridate); library(dplyr)

# Create sun and tide lookups ----
tide_sun_moon <- read.csv('p:/obrien/biotelemetry/nanticoke/tidesunmoon.csv',
                          stringsAsFactors = F) %>%
  mutate(time = ymd_hms(time))

# Sun
sun <- tide_sun_moon %>%
  filter(var == 'sun') %>%
  # Ballpark dawn and dusk as 1 hr one either side of sunrise/set
  mutate(start = time - hours(1),
         end = time + hours(1),
         period = ifelse(val == 'rise' , 'dawn', 'dusk'))

sun_lookup <- sun %>%
  mutate(start2 = end,
         end2 = start[row_number() + 1],
         period2 = ifelse(period == 'dawn', 'day', 'night')) %>%
  select(-start, -end, -period) %>%
  rename(start = start2,
         end = end2,
         period = period2) %>%
  rbind(sun)

# Tide
tide <- tide_sun_moon %>%
  filter(var == 'tide') %>%
  # Pad times with 1/16 of a tidal lunar day (24H50M; 1H33M7.5S)
  mutate(start = time - period('1H 33M 7.5S'),
         end = time + period('1H 33M 7.5S'),
         stage = tolower(val))

tide_lookup <- tide %>%
  mutate(start2 = end,
         end2 = start[row_number() + 1],
         stage2 = ifelse(stage == 'high', 'ebb', 'flood')) %>%
  select(-start, -end, -stage) %>%
  rename(start = start2,
         end = end2,
         stage = stage2) %>%
  rbind(tide)

rm(sun, tide)

# Import positions ----
all_pos <- read.csv(
  'p:/obrien/biotelemetry/nanticoke/vps results/positions/all-calc-positions.csv',
  stringsAsFactors = F) %>%
  mutate(DATETIME = lubridate::ymd_hms(DATETIME)) %>%
  rename_all(tolower)

fish_pos <- all_pos %>%
  filter(grepl('^\\d', all_pos$transmitter),
         hpe <= 10) %>%
  mutate(date = floor_date(datetime, 'hour'))

# Apply lookup tables ----
# Using data.table for the join range
fish_pos <- data.table::setDT(fish_pos)[sun_lookup,
                            on = .(datetime >= start, datetime < end),
                            period := period]

fish_pos <- fish_pos[tide_lookup,
                            on = .(datetime >= start, datetime < end),
                            stage := stage]

# Summary stats----
ggplot() +
  geom_bar(data = agg, aes(x = stage, fill = period), position = 'fill')
