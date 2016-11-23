require(TTR)
data = read.csv(file = '~/Git/marketalgo/data_qc_raw_hourly/audusd.csv', header = FALSE)
colnames(data) <- c('Timestamp', 'Open', 'High', 'Low', 'Close')

EMA <- EMA(data[,('Close')], 10)
RSI <- RSI(data[,('Close')], 20)
SlowRSI <- EMA(RSI(data[,('Close')], 20), 10)
MACD <- MACD(data[,('Close')], 10, 20, 10, percent = FALSE)

data = data.frame(data, EMA, RSI, SlowRSI, MACD)
names(data)[names(data) == 'macd'] <- 'MACD'
data <- subset(data, select = -c(signal) )

tail(data, 50)
nrow(data)

write.table(
  data,
  file = "~/Git/marketalgo/data_qc_converted_hourly/audusd.csv",
  na = "",
  sep = ",",
  row.names = FALSE
)