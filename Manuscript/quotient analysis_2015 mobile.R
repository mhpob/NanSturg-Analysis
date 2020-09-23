library(data.table)

mobile <- fread('data/marshnan_data.csv',
                col.names = function(.) tolower(gsub('[ .]', '_', .)))
mobile <- mobile[type == 'B']


library(TelemetryR)

subs <- mobile[grepl('1A', cruise)]
t <- quo_an(mobile$temp, mobile$detections, pres_abs = T)

hist(mobile$temp, breaks = seq(min(mobile$temp), ceiling(max(mobile$temp)), by = 1))


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
       ylim = c(0, ceiling(max(QA.result[, 6:8], na.rm = T))),
       yaxt = 'n', ylab = '', xlab = '', main = env)
  lines(env.hist$mids, QA.result$CI_0.975, lty = 5, lwd = 2, col = 'pink')
  lines(env.hist$mids, QA.result$CI_0.025, lty = 5, lwd = 2, col = 'pink')
  mtext('Value', side = 1, line = 2.25)
  axis(2)
  mtext('Quotient', side = 2, line = 2.25)

  abline(h = 1, lty = 3, col = 'blue')
}

QAplot(mobile, 'temp', 1)
QAplot(mobile, 'cond', 500)
QAplot(mobile, 'sal', 1)
QAplot(mobile, 'do_mg_l', 2)


library(mgcv)

tt <- gam(detections ~ s(temp),
          family = quasibinomial(),
          data = mobile)
summary(tt)
