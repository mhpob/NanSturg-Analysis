library(ggplot2); library(rgdal); library(ks); library(dplyr)
pos <- read.csv('p:/obrien/biotelemetry/nanticoke/vps results/positions/all-calc-positions.csv',
              stringsAsFactors = F)
pos <- pos[grepl('^\\d', pos$TRANSMITTER),]
pos$DATETIME <- lubridate::ymd_hms(pos$DATETIME)
pos$wk <- lubridate::floor_date(pos$DATETIME, 'week')


# Calculate kernel density estimate (KDE).
## lapply with break if Hpi doesn't converge. Use tryCatch to set failures as
##   NULL and move on to next set of detections.
pos.list <- split(as.data.frame(pos[, c('LON','LAT')]),
                  as.factor(pos$wk))

pos.bandw <- lapply(X = pos.list, FUN = function(x){
  tryCatch(Hpi(x), error = function(e){NULL})
})
pos.bandw <- pos.bandw[!sapply(pos.bandw, is.null)]
if(length(pos.bandw) != length(pos.bandw)){
  cat(paste('Transmitters did not converge.',
            paste(setdiff(names(pos.list), names(pos.bandw)), collapse = ', '),
            'dropped.'))
  pos.list <- pos.list[names(pos.bandw)]
}

pos.kde <- lapply(X = names(pos.list),
                  FUN = function(i){kde(x = pos.list[[i]],
                                        H = pos.bandw[[i]])})
names(pos.kde) <- names(pos.list)

# Plotting
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

kde.plot$transmitter <- gsub("[.].*", "", row.names(kde.plot))
kde.plot$contour <- unlist(lapply(strsplit(row.names(kde.plot), "[.]"),
                                  `[[`, 2))
kde.plot$contour <- paste(kde.plot$transmitter, kde.plot$contour, sep = ':')
row.names(kde.plot) <- NULL

# Shapefiles ----
# General Marsyhope
# marnan <- readOGR('c:/users/secor/desktop/gis products/nanticoke2015',
#                   'MarshNan')
#
# marnan <- spChFIDs(marnan, paste0(marnan@data$River, row.names(marnan)))
#
# marnan.df <- fortify(marnan)
#
# mar.plot <- filter(marnan.df, grepl('Mar', group))

# Habitat
habitat <- readOGR(dsn ='C:/Users/secor/Downloads/2015 Atlantic Sturgeon Habitat Geodatabase and Report Nanticoke and Tributaries-2016-01-19/2015 Atlantic Sturgeon Habitat Geodatabase Nanticoke and Tributaries 01132016.gdb')
habitat <- habitat[habitat$Location == 'Marshyhope Creek, MD',]

hab.data <- habitat@data
hab.data$OBJECTID <- as.character(hab.data$OBJECTID)
hab.data$SubGroup <- ifelse(hab.data$SubGroup == '<Null>', '',
                            levels(hab.data$SubGroup)[hab.data$SubGroup])

habitat <- spTransform(habitat, CRS = CRS('+proj=longlat +zone=18 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))

hab.df <- fortify(habitat, region = 'OBJECTID')

hab.df <- left_join(hab.df, hab.data, by = c('id' = 'OBJECTID'))

# Pick only habitat polygons that are within the area of interest
trim <- filter(hab.df,
               long >= -75.8145,
               long <= -75.8095,
               lat >= 38.6415,
               lat <= 38.649)
hab.df <- filter(hab.df, id %in% unique(trim$id))

library(ggplot2)
ggplot() +
  geom_polygon(data = hab.df, aes(x = long, y = lat, group = group,
                                  fill = interaction(Group_, SubGroup))) +
  coord_map(xlim = c(-75.8145, -75.8095), ylim = c(38.6415, 38.649)) +
  geom_path(data = kde.plot, aes(x, y, group = contour),
            color = 'black', lwd = 1) +
  facet_wrap(~ transmitter) +
  # scale_fill_manual(values = c('orange1', 'orangered', 'yellow',
  #                              'lightblue', 'blue'),
  #                   labels = c('Mud', 'Muddy Sand', 'Sand', 'Gravelly Sand',
  #                              'Sandy Gravel')) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.position = 'none', axis.ticks = element_blank(),
        axis.text = element_blank())
