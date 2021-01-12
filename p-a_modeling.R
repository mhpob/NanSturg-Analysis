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
  rbind(sun) %>%
  select(period, start, end) %>%
  mutate(rng = interval(start, end))

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
  rbind(tide) %>%
  select(stage, start, end) %>%
  mutate(rng = interval(start, end))

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


# Trim to per-transmitter date range ----
pa_trim <- split(fish_pos, fish_pos$transmitter)

pa_trim <- lapply(pa_trim, function(x){
  m <- range(as.Date(x$datetime))

  temp_function <- function(y){
    y <- y[y$start > m[1] & y$end < (m[2] + 1),]
    for(i in 1:nrow(y)){
      y$p_a[i] <- ifelse(T %in% (x$datetime %within% y$rng[i]), 1, 0)
    }
    names(y)[1] <- 'val'
    y
  }

  temp_tide <- temp_function(tide_lookup)
  temp_tide$var <- 'tide'
  temp_sun <- temp_function(sun_lookup)
  temp_sun$var <- 'sun'

  rbind(temp_tide, temp_sun)
})

pa_trim <- bind_rows(pa_trim, .id = 'transmitter')

xtabs(data = pa_trim, ~ val + p_a, subset = (var == 'tide'))
xtabs(data = pa_trim, ~ val + p_a, subset = (var == 'sun'))
ggplot() +
  geom_bar(data = filter(pa_trim, p_a == 1, var == 'tide'),
           aes(x = transmitter, fill = val), position = 'fill')


library(lme4)
tide_mod <- glmer(p_a ~ val + (1 | transmitter),
                  data = pa_trim, family = 'binomial',
                  subset = (var == 'tide'))
summary(tide_mod)
tide_means <- lsmeans::lsmeans(tide_mod, pairwise ~ val)

logit2p <- function(x){
  1 / (1 / exp(x) + 1)
}

tide_means <- data.frame(stage = summary(tide_means$lsmeans)$val,
                         mean = logit2p(summary(tide_means$lsmeans)$lsmean),
                         lower = logit2p(summary(tide_means$lsmeans)$asymp.LCL),
                         upper = logit2p(summary(tide_means$lsmeans)$asymp.UCL),
                         tukey = c('A', 'B', 'AB', 'A'))

library(ggplot2)
tide_plot <- ggplot(data = tide_means, aes(x = stage, y = mean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_text(aes(label = tukey, y = upper + 0.1)) +
  labs(x = NULL, y = 'Probability of presence') +
  coord_cartesian(ylim = c(0, 1), expand = F) +
  scale_x_discrete(limits = c('high', 'ebb', 'low', 'flood'),
                   labels = c('High', 'Ebb', 'Low', 'Flood')) +
  theme_bw()


sun_mod <- glmer(p_a ~ val + (1 | transmitter),
                 data = pa_trim, family = 'binomial',
                 subset = (var == 'sun'))
summary(sun_mod)
sun_means <- lsmeans::lsmeans(sun_mod, pairwise ~ val)

sun_means <- data.frame(stage = summary(sun_means$lsmeans)$val,
                         mean = logit2p(summary(sun_means$lsmeans)$lsmean),
                         lower = logit2p(summary(sun_means$lsmeans)$asymp.LCL),
                         upper = logit2p(summary(sun_means$lsmeans)$asymp.UCL),
                         tukey = c('A', 'B', 'A', 'C'))

sun_plot <- ggplot(data = sun_means, aes(x = stage, y = mean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_text(aes(label = tukey, y = upper + 0.1)) +
  labs(x = NULL, y = NULL) +
  coord_cartesian(ylim = c(0, 1), expand = F) +
  scale_x_discrete(labels = c('Dawn', 'Day', 'Dusk', 'Night')) +
  theme_bw()

library(patchwork)

tide_plot + sun_plot


library(ragg)
agg_tiff('manuscript/figures/figure5.tif',
         width = 1950,
         height = 985,
         res = 600,
         compression = 'lzw',
         scaling = 0.75)

tide_plot + sun_plot

dev.off()

names(sun_lookup) <- c('period', 'start_s', 'end_s', 'rng_s')
names(tide_lookup) <- c('stage', 'start_t', 'end_t', 'rng_t')


test <-

tot_mod <- glmer(p_a ~ val + (1 | transmitter),
                            data = pa_trim, family = 'binomial',
                            subset = (var == 'sun'))

