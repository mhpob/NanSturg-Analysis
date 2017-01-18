library(ggplot2); library(reshape2); library(dplyr)

wq.data <- read.csv('p:/obrien/biotelemetry/nanticoke/marshnan_data.csv',
                    stringsAsFactors = F)
wq.data$Detections <- as.factor(wq.data$Detections)
wq.data$st.pos <- ifelse(wq.data$Detections == 0, F, T)
wq.data <- wq.data %>%
  mutate(pred.growth = sturgrow(Temp, Sal, DO.pct),
         Date = lubridate::mdy(Date))
b.wq <- filter(wq.data, Type == 'B')

b.wq <- melt(data = b.wq, id.vars= c('Date', 'st.pos'),
             measure.vars = c('Depth', 'Temp', 'Sal', 'DO.pct', 'pred.growth'))


ggplot() + geom_boxplot(data = b.wq,
                        aes(x = Date, y = value, fill = st.pos,
                            group = interaction(factor(Date), st.pos))) +
  facet_wrap(~variable, scales = 'free_y') +
  labs(y = 'Value')
