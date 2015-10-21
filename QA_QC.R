data <- read.csv('p:/obrien/biotelemetry/nanticoke/marshnan_data.csv',
                 stringsAsFactors = F)
library(dplyr)
qaqc <- function(cruise){
  data.sub <- filter(data, Cruise == cruise)
  pairs(data.sub[, 6:13])
}

qaqc('2015_6A')
