library(TelemetryR); library(rgdal); library(ggplot2); library(dplyr)

## Input -------------------------------------------------------------------
marnan <- readOGR('c:/users/secor lab/desktop/gis products/nanticoke2015',
                'MarshNan')

marnan <- spChFIDs(marnan, paste0(marnan@data$River, row.names(marnan)))

marnan.df <- fortify(marnan)

mar.plot <- filter(marnan.df, grepl('Mar', group))
nan.plot <- filter(marnan.df, grepl('Nan', group))

# Detection data
wq.data <- read.csv('p:/obrien/biotelemetry/nanticoke/marshnan_data.csv',
                    stringsAsFactors = F)
wq.data$Detections <- as.factor(wq.data$Detections)
wq.data <- wq.data %>%
  mutate(pred.growth = sturgrow(Temp, Sal, DO.pct),
         Date = lubridate::mdy(Date))

WQplot <- function(var, type = 'B', system = 'all'){
  title.type <- switch(type,
                       B = 'Bottom',
                       S = 'Surface')
  title.print <- switch(var,
                  Depth = 'Depth (m)',
                  Temp = 'Temperature (°C)',
                  Cond = 'Conductivity (µS/cm)',
                  Sal = 'Salinity',
                  DO.pct = 'Dissolved Oxygen (% Saturation)',
                  DO.mg_l = 'Dissolved Oxygen (mg/L)',
                  pred.growth = 'Predicted Growth (per day)')
  title.print <- paste(title.type, title.print, sep = ' ')

  legend.print <- switch(var,
                   Depth = 'Depth',
                   Temp = 'Temperature',
                   Cond = 'Conductivity',
                   Sal = 'Salinity',
                   DO.pct = 'DO (%)',
                   DO.mg_l = 'DO (mg/L)',
                   pred.growth = 'Growth')

  system.plot <- switch(system,
                        all = marnan.df,
                        nan = nan.plot,
                        mar = mar.plot)

  system.dat <- switch(system,
                   all = wq.data,
                   nan = filter(wq.data, grepl('Nan', Site.ID)),
                   mar = filter(wq.data, grepl('Mar', Site.ID)))
  system.dat$plot.var <- system.dat[, var]

  ggplot(environment = environment()) +
  geom_point(data = filter(system.dat, Type == type),
             aes(x = DD.Long, y = DD.Lat, color = plot.var,
                 size = Detections),
                 environment = environment())+
  facet_wrap(~ Date, ncol = 2) +
  scale_color_continuous(low = 'blue', high = 'orange') +
  scale_size_manual(values = c(4, 10),
                    breaks = c('0', '1')) +
  geom_point(data = filter(system.dat, Detections != '0'),
             aes(x = DD.Long, y = DD.Lat, size = Detections),
             shape = 21, color = 'black') +
  geom_polygon(data = system.plot,
                     aes(x = long, y = lat, group = group),
                     fill = 'lightgray', color = 'black', alpha = 0.3) +
  theme_bw() +
  labs(x = 'Longitude', y = 'Latitude', title = title.print,
       color = legend.print)
}

WQplot('DO.pct')


circles <- ptcirc(wq.data %>% distinct(Site.ID) %>%
                    select(DD.Long, DD.Lat), 800)
ggplot() +
  geom_point(aes(x = c(-75.814226, -75.810498), y = c(38.647510, 38.643767)),
             color = 'red', size = 4) +
  geom_point(data = distinct(wq.data, Site.ID),
             aes(x = DD.Long, y = DD.Lat)) +
  geom_path(data = circles,
             aes(x = long, y = lat, group = circle)) +
  geom_polygon(data = marnan.df,
                     aes(x = long, y = lat, group = group),
                     fill = 'lightgray', color = 'black', alpha = 0.3) +
  theme_bw() +
  labs(x = 'Longitude', y = 'Latitude')
