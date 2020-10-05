library(data.table)

mobile <- fread('c:/users/darpa2/analysis/nansturg-analysis/manuscript/data/marshnan_data.csv',
                col.names = function(.) tolower(gsub('[ .]', '_', .)))
mobile <- mobile[type == 'B']
mobile[, week := week(as.Date(date, format = '%m/%d/%Y'))]


library(TelemetryR)


QAplot <- function(data, env, width){
  min.data <- min(data.frame(data)[,env], na.rm = T)
  max.data <- max(data.frame(data)[,env], na.rm = T)

  QA.result <- quo_an(data.frame(data)[,env],
                      data$detections,
                      bin_width = width,
                      pres_abs = T, R = 2000)

  brks <- seq(min.data, max.data, width)
  brks <- if (max.data > max(brks)) c(brks, max(brks) + width) else brks
  lims <- (max.data - min.data) / length(brks)

  par(mar = c(3, 3, 3, 3) + 0.1)
  env.hist <- hist(data.frame(data)[,env], breaks = brks, freq = T,
                   xlim = c(min.data - lims, max.data + lims),
                   ylim = c(0, max(QA.result$wq.var, na.rm = T) + 10),
                   xaxt = 'n', xlab = '', ylab = '', yaxt = 'n', main = '',
                   col = NA)
  axis(4)
  mtext('Number of Observations', side = 4, line = 2.25)

  par(new = T)
  plot(env.hist$mids, QA.result$Qe, type = 'b', col = 'red', lwd = 3,
       xlim = c(min.data - lims, max.data + lims),
       ylim = c(0, ceiling(max(QA.result[, 6:8][is.finite(as.matrix(QA.result[, 6:8]))]))),
       yaxt = 'n', ylab = '', xlab = '', main = env)
  lines(env.hist$mids, QA.result$CI_0.975, lty = 5, lwd = 2, col = 'pink')
  lines(env.hist$mids, QA.result$CI_0.025, lty = 5, lwd = 2, col = 'pink')
  mtext('Value', side = 1, line = 2.25)
  axis(2)
  mtext('Quotient', side = 2, line = 2.25)

  abline(h = 1, lty = 3, col = 'blue')
}

# Plot quotient by week
mobile[, QAplot(.SD, 'temp', 1), by = 'week']
mobile[, QAplot(.SD, 'cond', 500), by = 'week']
mobile[, QAplot(.SD, 'sal', 1), by = 'week']
mobile[, QAplot(.SD, 'do_pct', 10), by = 'week']
