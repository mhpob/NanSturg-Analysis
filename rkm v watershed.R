library(ggplot2); library(ragg)

tribs <- read.csv('manuscript/data/fig1data.csv')


agg_tiff('manuscript/figures/figure1_final.tif',
         width = 2250, height = 1721, res = 600, scaling = 0.5,
         compression = 'lzw')

ggplot(data = tribs, aes(x = river.km, y = watershed, label = Abbreviation,
                         color = ifelse(grepl('^NR', Abbreviation), T, F))) +
  geom_point(show.legend =  F) +
  ggrepel::geom_text_repel(show.legend =  F) +
  scale_color_manual(values = c('black', 'blue')) +
  scale_y_log10(limits = c(100, NA),
                breaks = c(10^(2:6)),
                labels = c('10\u00B2', '10\u00B3', '10\u2074', '10\u2075', '10\u2076')) +
  scale_x_continuous(limits = c(0, NA),
                     expand = expansion(c(0, 0.04))) +
  labs(x = 'Tributary extent (km)', y = expression(Watershed~(km^2))) +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.margin = margin(0, 2, 1, 0))

dev.off()
