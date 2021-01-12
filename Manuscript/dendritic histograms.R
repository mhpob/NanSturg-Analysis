library(data.table)

# Load receiver rkm
mdnr <- fread('manuscript/data_derived/mddnr_receiver_rkm.csv')
dnrec <- fread('manuscript/data_derived/dnrec_receiver_rkm.csv')
dnrec[, ':='('station' = site,
             site = NULL,
             year = NULL)]


# Load detections
m_dets <- fread('manuscript/data/detections/sturgeon_detections.gz')
m_dets <- m_dets[, date.utc := lubridate::ymd_hms(date.utc)][,
                   date.local := lubridate::with_tz(date.utc, 'America/New_York')]

d_dets <- fread('manuscript/data/detections/de detections.csv',
                col.names = function(.) tolower(gsub('[( )]', '.', .)))
d_dets <- setNames(d_dets, c('date.utc', 'receiver', 'transmitter', 'station', 'lat', 'long'))
d_dets[, ':='(receiver = NULL,
              date.utc = lubridate::ymd_hms(date.utc))][,
              date.local := lubridate::with_tz(date.utc, 'America/New_York')]

# Combine
detections <- rbind(m_dets, d_dets)
detections <- unique(detections, by = c('date.utc', 'transmitter', 'lat', 'long'))


rkms <- rbind(mdnr, dnrec)
rkms <- unique(rkms, by = c('station', 'lat', 'long'))


## leaving this object as "test" since there are still some station locations
## That are not specified.
test <- rkms[detections, on = 'station', allow.cartesian= T]


library(ggplot2); library(patchwork)


# SQRT seems to emphasize the differences on the top end, while LOG10 seems to
# emphasize those on the bottom

dendr_sqrt <- function(month){
  nan <- ggplot(data = test[!is.na(lat) &
                              month(date.local) == month &
                              grepl('Nan', body),]) +
    geom_histogram(aes(x = rkm_body_mouth, group = transmitter),
                   breaks = seq(0, 75, 3), color = 'white', size = 0) +
    labs(x = NULL, y = 'Detection count') +
    coord_trans(y = 'sqrt') +
    scale_y_continuous(breaks = 10 ^ (2:5),
                       limits = c(0, 1.5e5), expand = c(0, 0),
                       labels = c('\n100', 10 ^ (3:4), 1e5)) +
    scale_x_continuous(limits = c(0, 75), expand = c(0, 0))

  marsh <- ggplot(data = test[!is.na(lat) &
                                month(date.local) == month &
                                grepl('Marsh', body),]) +
    geom_histogram(aes(x = rkm_body_mouth, group = transmitter),
                   breaks = seq(0, 30, 3), color = 'white', size = 0) +
    labs(x = NULL, y = NULL) +
    coord_trans(y = 'sqrt', clip = 'off') +
    scale_y_continuous(breaks = 10 ^ (2:4),
                       limits = c(0, 6e4), expand = c(0, 0),
                       labels = c('\n100', 10 ^ (3:4))) +
    scale_x_continuous(limits = c(0, 30), expand = c(0, 0))


  broad <- ggplot(data = test[!is.na(lat) &
                                month(date.local) == month &
                                grepl('Broad', body),]) +
    geom_histogram(aes(x = rkm_body_mouth, group = transmitter),
                   breaks = seq(0, 12, 3), color = 'white', size = 0) +
    labs(x = NULL, y = NULL)+
    coord_trans(y = 'sqrt') +
    scale_y_continuous(breaks = 10 ^ (1:4),
                       limits = c(0, 1e4), expand = c(0, 0),
                       labels = c('\n10', 10 ^ (2:4))) +
    scale_x_continuous(limits = c(0, 12), expand = c(0, 0))


  deep <- ggplot(data = test[!is.na(lat) &
                               month(date.local) == month &
                               grepl('Deep', body),]) +
    geom_histogram(aes(x = rkm_body_mouth, group = transmitter),
                   breaks = seq(0, 3, 3), color = 'white', size = 0) +
    labs(x = NULL, y = NULL) +
    coord_trans(y = 'sqrt') +
    scale_y_continuous(breaks = 10 ^ (1:4),
                       limits = c(0, 1e4), expand = c(0, 0),
                       labels = c('\n10', 10 ^ (2:4))) +
    scale_x_continuous(limits = c(0, 3), expand = c(0, 0))



  design <- c(
    area(t = 1, r = 100, b = 33, l = 62),
    area(t = 34, r = 100, b = 66, l = 0),
    area(t = 67, r = 92, b = 99, l = 81),
    area(t = 67, r = 100, b = 99, l = 94)
  )

  marsh + nan + broad + deep + plot_layout(design = design) &
    # plot_annotation(title = month.name[month],
    #                 theme = theme(plot.title = element_text(size = 18))) &
    theme_bw() &
    theme(plot.margin = margin(0, 3, 0, 0),
          axis.text = element_text(size = 6, vjust = 0.25),
          axis.title.y = element_text(size = 7),
          axis.line.x.bottom = element_line(color = 'blue'))
}

library(ragg)
agg_png('manuscript/figures/dendr_hist/test.png', width = 3900, height = 3870,
        res = 600)
plot_grid(dendr_sqrt(5), dendr_sqrt(6),
          dendr_sqrt(7), dendr_sqrt(8),
          dendr_sqrt(9), dendr_sqrt(10),
          nrow = 3)
dev.off()









agg_png('manuscript/figures/dendr_hist/sqrt_6jun2.png', width = 1950, height = 1290,
        res = 600)
dendr_sqrt(6)
dev.off()

agg_png('manuscript/figures/dendr_hist/sqrt_7jul2.png', width = 1950, height = 1290,
        res = 600)
dendr_sqrt(7)
dev.off()

agg_png('manuscript/figures/dendr_hist/sqrt_8aug2.png', width = 1950, height = 1290,
        res = 600)
dendr_sqrt(8)
dev.off()

agg_png('manuscript/figures/dendr_hist/sqrt_9sept2.png', width = 1950, height = 1290,
        res = 600)
dendr_sqrt(9)
dev.off()

agg_png('manuscript/figures/dendr_hist/sqrt_10oct2.png', width = 1950, height = 1290,
        res = 600)
dendr_sqrt(10)
dev.off()






setEPS()
postscript('manuscript/figures/dendr_hist/test.eps', width = 3.25, height = 2.15,
           horizontal = FALSE, paper = "special")
dendr_sqrt(9)
dev.off()





# Log-transformed y axis
dendr_log <- function(month){
  # create log10(x + 0.01) transformation
  log10_p <- scales::trans_new(name = 'log10_p',
                               transform = function(.) log10(. + 0.01),
                               inverse = function(.) (10 ^ .) - 0.01)

  nan <- ggplot(data = test[!is.na(lat) &
                              month(date.local) == month &
                              grepl('Nan', body),]) +
    geom_histogram(aes(x = rkm_body_mouth, group = transmitter),
                   breaks = seq(0, 75, 3), color = 'white') +
    labs(x = NULL, y = 'Detection count') +
    coord_trans(y = log10_p, ylim = c(1, 1.5e5), expand = F) +
    scale_y_continuous(breaks = 10 ^ (0:5),
                       labels = c(10 ^ (0:4), 1e5),
                       limits = c(0, 1.5e5)) +
    scale_x_continuous(limits = c(0, 75), expand = c(0, 0))

  marsh <- ggplot(data = test[!is.na(lat) &
                                month(date.local) == month &
                                grepl('Marsh', body),]) +
    geom_histogram(aes(x = rkm_body_mouth, group = transmitter),
                   breaks = seq(0, 30, 3), color = 'white') +
    labs(x = NULL, y = NULL) +
    coord_trans(y = log10_p, ylim = c(1, 6e4), expand = F) +
    scale_y_continuous(breaks = 10 ^ (0:4),
                       labels = 10 ^ (0:4),
                       limits = c(0, 6e4))  +
    scale_x_continuous(limits = c(0, 30), expand = c(0, 0))


  broad <- ggplot(data = test[!is.na(lat) &
                                month(date.local) == month &
                                grepl('Broad', body),]) +
    geom_histogram(aes(x = rkm_body_mouth, group = transmitter),
                   breaks = seq(0, 12, 3), color = 'white') +
    labs(x = NULL, y = NULL)+
    coord_trans(y = log10_p, ylim = c(1, 1e4), expand = F) +
    scale_y_continuous(breaks = 10 ^ (0:3),
                       limits = c(0, 1e4)) +
    scale_x_continuous(limits = c(0, 12), expand = c(0, 0))


  deep <- ggplot(data = test[!is.na(lat) &
                               month(date.local) == month &
                               grepl('Deep', body),]) +
    geom_histogram(aes(x = rkm_body_mouth, group = transmitter),
                   breaks = seq(0, 3, 3), color = 'white') +
    labs(x = NULL, y = NULL) +
    coord_trans(y = log10_p, ylim = c(1, 1e4), expand = F) +
    scale_y_continuous(breaks = 10 ^ (0:3),
                       limits = c(0, 1e4)) +
    scale_x_continuous(limits = c(0, 3), expand = c(0, 0))



  design <- c(
    area(t = 1, r = 100, b = 33, l = 62),
    area(t = 34, r = 100, b = 66, l = 0),
    area(t = 67, r = 92, b = 99, l = 81),
    area(t = 67, r = 100, b = 99, l = 94)
  )

  marsh + nan + broad + deep + plot_layout(design = design) &
    # plot_annotation(title = month.name[month],
    #                 theme = theme(plot.title = element_text(size = 18))) &
    theme_bw() &
    theme(plot.margin = margin(0, 3, 0, 0))
}


library(ragg)
agg_png('manuscript/figures/dendr_hist/log_5may.png', width = 4500, height = 2550,
        res = 600)
dendr_log(5)
dev.off()

agg_png('manuscript/figures/dendr_hist/log_6jun.png', width = 4500, height = 2550,
        res = 600)
dendr_log(6)
dev.off()

agg_png('manuscript/figures/dendr_hist/log_7jul.png', width = 4500, height = 2550,
        res = 600)
dendr_log(7)
dev.off()

agg_png('manuscript/figures/dendr_hist/log_8aug.png', width = 4500, height = 2550,
        res = 600)
dendr_log(8)
dev.off()

agg_png('manuscript/figures/dendr_hist/log_9sept.png', width = 4500, height = 2550,
        res = 600)
dendr_log(9)
dev.off()

agg_png('manuscript/figures/dendr_hist/log_10oct.png', width = 4500, height = 2550,
        res = 600)
dendr_log(10)
dev.off()