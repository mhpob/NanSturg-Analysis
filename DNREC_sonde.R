library(ggplot2); library(dplyr)
# Marshyhope is closest to M14, Ferry is closest to ND5

sonde <- 'p:/obrien/biotelemetry/nanticoke/dnrec sonde/'
sonde <- paste0(sonde, list.files(path = sonde, pattern = '*.csv'))
sonde <- lapply(sonde, FUN = read.csv, stringsAsFactors = F)
sonde <- do.call(rbind.data.frame, sonde)
names(sonde) <- c('Date.Time', 'Temp', 'Cond', 'Sal', 'pH', 'pH.mv', 'DO.pct',
                  'DO.mg_l', 'Batt', 'Station', 'Lat', 'Long')
sonde$Date.Time <- lubridate::mdy_hms(sonde$Date.Time)

cruise <- read.csv('p:/obrien/biotelemetry/nanticoke/marshnan_data.csv')
cruise <- cruise %>%
  mutate(Date.Time = paste(Date, Time),
         Date.Time = lubridate::mdy_hm(Date.Time))


sondeplot <- function(var, type = 'B', site = 'marshy'){
  title.type <- switch(type,
                       B = 'Bottom ',
                       S = 'Surface ')
  title.var <- switch(var,
                      Depth = 'Depth (m)',
                      Temp = 'Temperature (°C)',
                      Cond = 'Conductivity (µS/cm)',
                      Sal = 'Salinity',
                      DO.pct = 'Dissolved Oxygen (% Saturation)',
                      DO.mg_l = 'Dissolved Oxygen (mg/L)')
  title.site <- switch(site,
                       marshy = ', Marshyhope Sonde',
                       ferry = ', Woodland Ferry',
                       draw = ', Seaford Drawbridge')
  title.print <- paste0(title.type, title.var, title.site)

  sonde.dat <- switch(site,
                   marshy = filter(sonde, grepl('Mar', Station)),
                   ferry = filter(sonde, grepl('Fer', Station)),
                   draw = filter(sonde, grepl('Draw', Station)))
  sonde.dat$plot.var <- sonde.dat[, var]

  cruise.dat <- switch(site,
                       marshy = filter(cruise, Site.ID == 'M14', Type == type),
                       ferry = filter(cruise, Site.ID == 'ND5', Type == type),
                       draw = filter(cruise, Site.ID == 'ND9', Type == type))
  cruise.dat$plot.var <- cruise.dat[, var]


  ggplot(environment = environment()) +
    geom_line(data = sonde.dat,
              aes(x = Date.Time, y = plot.var)) +
    geom_point(data = cruise.dat,
               aes(x = Date.Time, y = plot.var), color = 'red', size = 3) +
    theme_bw() +
    labs(x = 'Date', y = title.var, title = title.print)
}

sondeplot('DO.mg_l', type = 'B', site = 'marshy')





library(reshape2)
cruise.long <- melt(cruise, id.vars = c('Date.Time', 'Site.ID',
                                        'Cruise', 'Type'),
                    measure.vars = c('Temp', 'Cond', 'Sal',
                                     'DO.pct', 'DO.mg_l'))

sonde.long <- melt(sonde, id.vars = c('Date.Time', 'Station'),
                   measure.vars = c('Temp', 'Cond', 'Sal',
                                    'DO.pct', 'DO.mg_l'))

allplot <- function(type = 'B', site = 'marshy'){
  title.type <- switch(type,
                       B = 'Bottom',
                       S = 'Surface')
  title.site <- switch(site,
                       marshy = ', Marshyhope Sonde',
                       ferry = ', Woodland Ferry',
                       draw = ', Seaford Drawbridge')
  title.print <- paste0(title.type, title.site)

  sonde.dat <- switch(site,
                      marshy = filter(sonde.long, grepl('Mar', Station)),
                      ferry = filter(sonde.long, grepl('Fer', Station)),
                      draw = filter(sonde.long, grepl('Draw', Station)))

  cruise.dat <- switch(site,
                 marshy = filter(cruise.long, Site.ID == 'M14', Type == type),
                 ferry = filter(cruise.long, Site.ID == 'ND5', Type == type),
                 draw = filter(cruise.long, Site.ID == 'ND9', Type == type))

  ggplot() + geom_line(data = sonde.dat,
                     aes(x = Date.Time, y = value)) +
    geom_point(data = cruise.dat,
               aes(x = Date.Time, y = value), color = 'red', size = 5) +
    facet_wrap(~variable, scales = 'free_y') +
    theme_bw() +
    labs(x = 'Date', y = 'Value', title = title.print)
}

allplot(site = 'marshy')
