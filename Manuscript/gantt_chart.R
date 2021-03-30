library(ggplot2); library(ragg); library(data.table)

tasks <- fread('manuscript/data/sturgeon gantt.csv')

tasks[, ':='(system = factor(system,
                             ordered = T,
                             levels = c('Marshyhope', 'Upper Nanticoke',
                                        'Broad Creek', 'Deep Creek',
                                        'Lower Nanticoke')),
             activity = factor(activity,
                               ordered = T,
                               levels = rev(c('Tagging', 'Population genetics',
                                          'Telemetry array', 'Bottom substrate survey',
                                          'Water quality', 'Mobile telemetry',
                                          'Egg mats', 'Plankton nets',
                                          'Telemetry positioning'))))]


agg_tiff('manuscript/figures/gantt2.tif',
        width = 2250, height = 1406, res = 600, scaling = 0.5,
        compression = 'lzw')

ggplot(data = tasks) +
  geom_segment(aes(y = as.numeric(activity) - (as.numeric(system) - 3) / 6,
                   yend = as.numeric(activity) - (as.numeric(system) - 3) / 6,
                   x = start, xend = end, color = system),
                 size = 3) +
  scale_y_continuous(breaks = 1:9, labels = levels(tasks$activity)) +
  scale_color_viridis_d(direction = -1, option = 'cividis') +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.87, 0.15),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.margin = margin(0, 0, 0, 0),
        plot.margin = margin(0, 0, 0, 0))

dev.off()
