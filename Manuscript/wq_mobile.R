library(data.table); library(lubridate)

mobile <- fread('data/marshnan_data.csv',
                col.names = function(.) tolower(gsub('[ .]', '_', .)))
mobile <- mobile[type == 'B']
mobile[, wk := floor_date(mdy(date), 'week')]

mobile <- melt(mobile, measure.vars = c('depth', 'temp', 'sal', 'do_pct'))
mobile[, ':='(variable = fcase(variable == 'depth', 'Bottom depth (m)',
                               variable == 'temp', 'Bottom temperature (°C)',
                               variable == 'sal', 'Salinity',
                               variable == 'do_pct', 'Dissolved Oxygen (%)'),
              wk = as.factor(strftime(wk, format = '%b %d')),
              detections = as.factor(detections))]

library(ggplot2)

wq_plot <- ggplot(data = mobile) +
  geom_boxplot(aes(x = wk, y = value, fill = detections)) +
  scale_fill_grey(start = 0.5, end = 1) +
  facet_wrap(~ variable, scales = 'free_y') +
  labs(x = NULL, y = 'Value', fill = 'Detections') +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = c(0.94, 0.89),
        legend.margin = margin(0,0,0,0))


library(ragg)
agg_png('manuscript/figures/2015wq.png',
        width = 2100, height = 1247, res = 600, scaling = 0.5)

wq_plot

dev.off()
