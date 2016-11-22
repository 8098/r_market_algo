require(TTR)
data = read.csv(file = '~/Git/marketalgo/data_raw_qc/audusd.csv', header = FALSE)
colnames(data) <- c('Timestamp', 'Open', 'High', 'Low', 'Close')

EMA <- EMA(data[,('Close')], 10)
RSI <- RSI(data[,('Close')], 20)
SlowRSI <- EMA(RSI(data[,('Close')], 20), 10)

data = data.frame(data, EMA, RSI, SlowRSI)
head(data, 50)
nrow(data)

write.table(
  data,
  file = "~/Git/marketalgo/data_converted_qc/audusd.csv",
  na = "",
  sep = ",",
  row.names = FALSE
)