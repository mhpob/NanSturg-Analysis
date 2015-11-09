library(TelemetryR); library(ggplot2); library(dplyr)
wq.dat <- read.csv('p:/obrien/biotelemetry/nanticoke/marshnan_data.csv',
                   stringsAsFactors = F)

wq.dat <- wq.dat %>%
  mutate(pred.growth = sturgrow(Temp, Sal, DO.pct))

wq_mean <- wq.dat %>%
  group_by(Site.ID, Cruise) %>%
  summarize(temp = mean(Temp),
            do = mean(DO.pct),
            sal = mean(Sal),
            det = mean(Detections))

# Plot general histogram, histogram where detections > 0, cumdist of detections
hist_cum_plot <- function (var, width, type = 'B') {
  lims <- NULL
  data <- wq.dat[wq.dat$Type == type,]
  lims[1] <- ifelse(abs(min(data[, var])) < 1,
                    min(data[, var]) - abs(min(data[, var])) / 5,
                    floor(min(data[, var])))
  lims[2] <- ifelse(abs(max(data[, var])) < 1,
                    max(data[, var]) + abs(max(data[, var])) / 5,
                    ceiling(max(data[, var])))
  brks <- seq(lims[1], lims[2], width)
  brks <- if(lims[2] > max(brks)) c(brks, max(brks) + width) else brks

  cum.dat <- substitute(data %>% filter(Detections > 0) %>% arrange(var) %>%
                           mutate(cumulative = cumsum(Detections),
                                  cumulative = cumulative/max(cumulative)*100),
                        list(var = as.name(var)))
  cum.dat <- eval(cum.dat)


  par(mar = c(5, 4, 4, 5) + 0.1)
  j <- hist(data[, var], breaks = brks, plot = F)
  hist(data[, var], breaks = brks,
       xlim = lims,
       ylim = c(0, max(j$counts)),
       xlab = var,
       main = paste('Histogram of', var))

  par(new = T)
  hist(data[data$Detections > 0, var], breaks = brks,
       col = 'red', xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = '',
       xlim = lims,
       ylim = c(0, max(j$counts)))

  par(new = T)
  plot(cum.dat[, var], cum.dat[, 'cumulative'],
       xlim = lims,
       type = 'l', col = 'blue',
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = '',)
  axis(4, at = c(0, 20, 40, 60, 80, 100), col.axis = 'blue')
    mtext('Cumulative Detections (%)', side = 4, line = 3, col = 'blue')
}

hist_cum_plot('Temp', 2)
hist_cum_plot('pred.growth', 0.01)
hist_cum_plot('DO.pct', 5)
hist_cum_plot('DO.mg_l', 0.5)
hist_cum_plot('Sal', 0.5)
hist_cum_plot('Cond', 200)
hist_cum_plot('Depth', 1)

# Plot histogram and overlay detections via points
pts_histo <- function(var, width, type = 'B'){
  data <- wq.dat %>%
    filter(Type == type)
  lims <- NULL
  lims[1] <- ifelse(abs(min(data[, var])) < 1,
                    min(data[, var]) - abs(min(data[, var])) / 5,
                    floor(min(data[, var])))
  lims[2] <- ifelse(abs(max(data[, var])) < 1,
                    max(data[, var]) + abs(max(data[, var])) / 5,
                    ceiling(max(data[, var])))
  brks <- seq(lims[1], lims[2], width)
  brks <- if(lims[2] > max(brks)) c(brks, max(brks) + width) else brks


  data <- data %>%
    mutate(bin = findInterval(data[, var], brks))
  det.bins <- data %>%
    group_by(bin) %>%
    summarize(detect = sum(Detections))

  histo <- hist(data[, var], breaks = brks, plot = F)
  mids <- data.frame(mids = histo$mids)
  mids$bin <- row.names(mids)

  det.bins <- merge(det.bins, mids)
  det.bins <- det.bins[det.bins$detect != 0,]

  par(mar = c(5, 4, 4, 5) + 0.1)
  plot(histo, main = var, xlab = var,
       xlim = lims)
  par(new = T)
  plot(det.bins$mids, det.bins$detect,
       xlim = lims,
       col = 'blue', xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  axis(4, at = seq(0, ceiling(range(det.bins$detect)[2]), 2),
       col.axis = 'blue')
  mtext('Detections', side = 4, line = 3, col = 'blue')
}

pts_histo('Temp', 0.5)
pts_histo('pred.growth', 0.01)
pts_histo('DO.pct', 5)
pts_histo('Sal', 0.5)
pts_histo('Cond', 200)
