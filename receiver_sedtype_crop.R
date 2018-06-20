library(dplyr); library(sf)

# 1) Import receiver locations ----
# Start with predetermined distance to get the code right, then move onto VPS-derived
receivers <- read.csv('p:/obrien/biotelemetry/nanticoke/MDNR_DNREC Receiver Locations.csv',
                      stringsAsFactors = F)

# 2) Create circles around points ----
receivers <- receivers %>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326) %>%
  st_transform(26918) %>%
  st_buffer(dist = 500)

# 3) Import habitat polygons ----
habitat <- st_read(dsn ='C:/Users/secor/Downloads/2015 Atlantic Sturgeon Habitat Geodatabase and Report Nanticoke and Tributaries-2016-01-19/2015 Atlantic Sturgeon Habitat Geodatabase Nanticoke and Tributaries 01132016.gdb',
                   layer = 'RiverBed_Habitat_Polygons_CMECS_SC_01132016')%>%
  # Slight manipulation of underlying data
  mutate(SubGroup = case_when(SubGroup == '<Null>' ~ as.character(Group_),
                              T ~ as.character(SubGroup)))

# 4) sf::st_join using st_intersects as function, or just st_intersects ----
# Only works if habitat has a buffer of zero due to 'self-intersection'
hab_rec <- st_intersection(st_buffer(habitat, 0), receivers)

# 5) Clip away ----
hab.clip <- hab_rec %>%
  mutate(area = st_area(.)) %>%
  group_by(Name, SubGroup) %>%
  summarize(grp.area = sum(area)) %>%
  mutate(prop = as.numeric(grp.area / sum(grp.area)))

# Summary ----
ggplot() + geom_histogram(data = hab.clip, aes(as.numeric(grp.area) * 0.0001)) +
  facet_wrap(~ SubGroup) +
  labs(x = 'Area within receiver coverage (ha)', y = 'Count') +
  theme_bw()

ggplot() + geom_histogram(data = hab.clip, aes(prop)) +
  facet_wrap(~ SubGroup) +
  labs(x = 'Proportion within receiver coverage', y = 'Count') +
  theme_bw()

# Bar plot popups ----
bars <- vector('list', nrow(receivers))
names(bars) <- receivers$Name

for(i in unique(hab.clip$Name)){
    barplot(data.frame(hab.clip)[hab.clip$Name == i,'prop'],
            names.arg = data.frame(hab.clip)[hab.clip$Name == i,'SubGroup'],
            las = 2)
  bars[[i]] <- recordPlot()}

for(i in 1:length(bars)){
  if(is.null(bars[[i]])){
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, 'No habitat data in range \n of this receiver.',
         cex = 1.6, col = "black")
    bars[[i]] <- recordPlot()
  }
}

# Plotting ----

library(mapview)
map <- mapview(habitat) +
  mapview(receivers, zcol = 'Name', alpha.regions = 0.1,
          popup = popupGraph(bars)) +
  mapview(hab.clip, zcol = 'SubGroup')

mapshot(map, url = 'p:/obrien/biotelemetry/nanticoke/interactive map/map/habitat.html')
