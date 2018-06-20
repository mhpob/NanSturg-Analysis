

sharpDL <- function(mon){
  raw.data <- httr::POST(url = 'http://www.saltwatertides.com/cgi-bin/maryland87month2.cgi',
                         body = list(site = 'Maryland',
                                     station_number = 8571858,
                                     month = mon, year = 2017,
                                     start_date = 1,
                                     maximum_days = 21),
                         encode = 'form')

  raw.data <- httr::content(raw.data)
  xml.data <- xml2::xml_text(xml2::xml_find_all(raw.data, './/pre'))

  data <- readr::read_fwf(xml.data,
                          readr::fwf_widths(c(4, 7, 6, 13, 6, 11, 5, 14, 2)),
                          skip = 4)
  data <- data[rowSums(is.na(data)) != ncol(data),]

  names(data) <- c('day', 'dnum', 'tide', 'time', 'height', 'sunrise_set',
                   'moonrise_set', 'moontime', 'moonvis')

  if(grepl('M', data$time)){
    data$time <- lubridate::ymd_hm(paste0('2017-', mon, '-', data$dnum, ' ', data$time))
  }else{
    data$time <- lubridate::ymd_hms(paste0('2017-', mon, '-', data$dnum, ' ', data$time))
  }
  data$sunrise_set <- lubridate::ymd_hms(paste0('2017-', mon, '-', data$dnum, ' ', data$sunrise_set))
  data$moontime <- lubridate::ymd_hms(paste0('2017-', mon, '-', data$dnum, ' ', data$moontime))

  tide <- data[, c('time', 'tide')]
  tide <-  cbind(tide[, 'time'], 'tide', tide[, 'tide'])
  names(tide) <- c('time', 'var', 'val')

  sun <- data[, 'sunrise_set']
  sun <- sun[complete.cases(sun),]
  sun <- cbind(sun, 'sun', rep(c('rise', 'set')))
  names(sun) <- c('time', 'var', 'val')

  moon <- data[, c('moonrise_set', 'moontime')]
  moon <- moon[complete.cases(moon),]
  moon <- cbind(moon[, 'moontime'], 'moon', moon[, 'moonrise_set'])
  names(moon) <- c('time', 'var', 'val')

  mvis <- tidyr::fill(dplyr::group_by(data, dnum), moonvis)
  mvis <- mvis[mvis$moonrise_set == 'Rise', c('moonrise_set', 'moontime', 'moonvis')]
  mvis <- mvis[complete.cases(mvis),]
  mvis <- cbind(mvis[, 'moontime'], 'moonvis', mvis[, 'moonvis'])
  names(mvis) <- c('time', 'var', 'val')


  rbind(tide, sun, moon, mvis)

}

sharpvals <- lapply(c(08,09,10), sharpDL)
sharpvals <- do.call(rbind, sharpvals)

write.csv(sharpvals, 'p:/obrien/biotelemetry/nanticoke/tidesunmoon.csv',
         row.names = F)
