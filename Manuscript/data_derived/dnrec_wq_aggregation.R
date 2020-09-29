library(readxl); library(data.table)

files <- list.files('manuscript/data', pattern = '.xl', full.names = T)

# See what the sheet names are
lapply(files, excel_sheets)



# Function to pull in and aggregate data by day
dnrec_pull <- function(string, file_list){
  pull <- lapply(file_list, function(.){
    name <- grep(string, excel_sheets(.), value = T)
    data <- read_xlsx(., sheet = name, range = cell_cols(1:8))

    # One column passes date as Excel numeric. Fix that.
    if(inherits(data[[1]], 'POSIXt') == F){
      data[[1]] <- as.POSIXlt(as.numeric(data[[1]]) * 24 * 60 * 60,
                              origin = '1899-12-30', tz = 'UTC')
    }

    data[[1]] <- as.POSIXct(as.character(data[[1]]),
                            origin = as.POSIXct("1970-01-01", tz = "America/New_York"),
                            tz = "America/New_York")
    data
  })

  # Combine list
  pull <- rbindlist(pull, use.names = F)

  setnames(pull, c('date.edt', 'temp_c', 'spcond_us', 'sal', 'ph', 'ph_mv',
                    'do_pct', 'do_mgl'))

  pull <- pull[!is.na(date.edt)]

  # Aggregate by day
  pull[, lapply(.SD, mean, na.rm = T), by = (date = as.Date(date.edt))]
}


# Note that you'll get yelled at. During date conversion there are a few summary
#   rows that are dropped (on purpose)
broad <- dnrec_pull('Broad', files)
seaford <- dnrec_pull('Seaf', files)
woodland <- dnrec_pull('Wood|Jack', files)


# Hatchery has a header in 2015. Need to do this one separately.
hatchery <- dnrec_pull('Hatch', files[-1])

hatch_2015 <- read_xlsx(files[1], sheet = 'Hatchery', skip = 6,
                        range = cell_limits(c(7, 1), c(NA, 8)))
setnames(hatch_2015, c('date.edt', 'temp_c', 'spcond_us', 'sal', 'ph', 'ph_mv',
                       'do_pct', 'do_mgl'))
hatch_2015 <- data.table(hatch_2015)[, lapply(.SD, mean, na.rm = T),
                                     by = (date = as.Date(date.edt))]

hatchery <- rbind(hatch, hatch_2015)



# Bind all
all <- rbindlist(list('broad creek' = broad,
                      'seaford' = seaford,
                      'woodland ferry' = woodland,
                      'hatchery' = hatchery),
                 idcol = 'station')

fwrite(all, 'manuscript/data_derived/dnrec_wq_aggregated.csv',
       dateTimeAs = 'write.csv')
