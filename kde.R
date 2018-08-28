library(ggplot2); library(ks); library(dplyr); library(sf)

# Import data ----
all_pos <- read.csv(
  'p:/obrien/biotelemetry/nanticoke/vps results/positions/all-calc-positions.csv',
  stringsAsFactors = F) %>%
  rename_all(tolower) %>%
  mutate(datetime = lubridate::ymd_hms(datetime),
         wk = lubridate::floor_date(datetime, 'week'))

fish_pos <- all_pos %>%
  filter(grepl('^\\d', all_pos$transmitter),
         hpe <= 10)

# Habitat
habitat <- st_read(dsn ='C:/Users/secor/Downloads/2015 Atlantic Sturgeon Habitat Geodatabase and Report Nanticoke and Tributaries-2016-01-19/2015 Atlantic Sturgeon Habitat Geodatabase Nanticoke and Tributaries 01132016.gdb',
                   layer = 'RiverBed_Habitat_Polygons_CMECS_SC_01132016')

# Select polygons in the Marshyhope
habitat <- habitat %>%
  # Select polygons in the Marshyhope
  filter(Location == 'Marshyhope Creek, MD') %>%
  # Slight manipulation of underlying data
  mutate(OBJECTID = as.character(OBJECTID),
         SubGroup = case_when(SubGroup == '<Null>' ~ '',
                              T ~ as.character(SubGroup))) %>%
  # Reproject to lonlat
  st_transform(4326) %>%
  st_crop(xmin = -75.8145, xmax = -75.8095, ymin = 38.6415, ymax = 38.649)


# Calculate kernel density estimate (KDE) ----
kde_func <- function(spl.factor = 'wk', cont.level = '50%'){
  pos.list <- split(as.data.frame(fish_pos[, c('lon','lat')]),
                    lapply(spl.factor, function(x) fish_pos[, x]))

  ## lapply with break if Hpi doesn't converge. Use tryCatch to set failures as
  ##   NULL and move on to next set of detections.
  pos.bandw <- lapply(X = pos.list, FUN = function(x){
    tryCatch(Hpi(x), error = function(e){NULL})
  })
  pos.bandw <- pos.bandw[!sapply(pos.bandw, is.null)]
  if(length(pos.list) != length(pos.bandw)){
    cat(paste('Transmitters did not converge.',
              paste(setdiff(names(pos.list), names(pos.bandw)), collapse = ', '),
              'dropped.'))
    pos.list <- pos.list[names(pos.bandw)]
  }

  pos.kde <- lapply(X = names(pos.list),
                    FUN = function(i){kde(x = pos.list[[i]],
                                          H = pos.bandw[[i]])})
  names(pos.kde) <- names(pos.list)


  # Prepare KDE for plotting ----
  # Create a list (transmitters) of lists (Contour groups)
  temporary.contour.function <- function(i, cont.level){
    contourLines(x = pos.kde[[i]]$eval.points[[1]],
                 y = pos.kde[[i]]$eval.points[[2]],
                 z = pos.kde[[i]]$estimate,
                 levels = pos.kde[[i]]$cont[cont.level])
  }

  kde.plot <- lapply(names(pos.kde),
                     temporary.contour.function, cont.level = '50%')

  names(kde.plot) <- names(pos.list)

  for(i in seq(1, length(kde.plot), 1)){
    names(kde.plot[[i]]) <- seq(1, length(kde.plot[[i]]), 1)
    kde.plot[[i]] <- lapply(kde.plot[[i]], data.frame)
  }

  kde.plot <- lapply(names(kde.plot), function(i){do.call(rbind, kde.plot[[i]])})
  names(kde.plot) <- names(pos.list)
  kde.plot <- do.call(rbind, kde.plot)

  for(i in 1:length(spl.factor)){
    kde.plot[, spl.factor[i]] <- sapply(strsplit(row.names(kde.plot), "[.]"),
                                       `[[`, i)
  }

  kde.plot$contour <- do.call(paste,
                              c(lapply(spl.factor, function(x) kde.plot[, x]),
                                sep = '.'))

  kde.plot$contour <- paste(kde.plot$contour,
                            unlist(lapply(strsplit(row.names(kde.plot), "[.]"),
                                    `[[`, i + 1)),
                            sep = ':')
  row.names(kde.plot) <- NULL
  kde.plot
}

kde_wk50 <- kde_func()
kde_trans50 <- kde_func('transmitter')
test <- kde_func(c('wk','transmitter'))

# Plotting ----
# By week
ggplot() +
  geom_sf(data = habitat, aes(fill = interaction(Group_, SubGroup)),
          color = 'black') +
  coord_sf(xlim = c(-75.8145, -75.8095), ylim = c(38.6422, 38.6486), expand = F) +
  geom_path(data = kde_wk50, aes(x, y, group = contour),
            color = 'black', lwd = 1) +
  facet_wrap(~ wk) +
  scale_fill_manual(values = c('orange1', 'orangered', 'yellow',
                               'lightblue', 'blue'),
                    labels = c('Mud', 'Muddy Sand', 'Sand', 'Gravelly Sand',
                               'Sandy Gravel')) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.position = 'none', axis.ticks = element_blank(),
        axis.text = element_blank())

# By transmitter
ggplot() +
  geom_sf(data = habitat, aes(fill = interaction(Group_, SubGroup)),
          color = 'black') +
  coord_sf(xlim = c(-75.8145, -75.8095), ylim = c(38.6422, 38.6486), expand = F) +
  geom_path(data = kde_trans50, aes(x, y, group = contour),
            color = 'black', lwd = 1) +
  facet_wrap(~ group, nrow = 2) +
  scale_fill_manual(values = c('orange1', 'orangered', 'yellow',
                               'lightblue', 'blue'),
                    labels = c('Mud', 'Muddy Sand', 'Sand', 'Gravelly Sand',
                               'Sandy Gravel')) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.position = 'none', axis.ticks = element_blank(),
        axis.text = element_blank())

# Week by transmitter
ggplot() +
  geom_sf(data = habitat, aes(fill = interaction(Group_, SubGroup)),
          color = NA) +
  coord_sf(xlim = c(-75.8145, -75.8095), ylim = c(38.6422, 38.6486), expand = F) +
  geom_path(data = kde_wktrans, aes(x, y, group = contour),
            color = 'black', lwd = 1) +
  facet_grid(wk ~ transmitter) +
  scale_fill_manual(values = c('orange1', 'orangered', 'yellow',
                               'lightblue', 'blue'),
                    labels = c('Mud', 'Muddy Sand', 'Sand', 'Gravelly Sand',
                               'Sandy Gravel')) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.position = 'none', axis.ticks = element_blank(),
        axis.text = element_blank())
