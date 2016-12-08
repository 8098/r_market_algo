require(TTR)
require(quantmod)
require(DataCombine)
require(dplyr)

# VARIABLES
start_time <- Sys.time()
directory_hourly = "~/marketalgo/data_qc_hourly/"
directory_daily = "~/marketalgo/data_qc_daily/"

# HOURLY DATA - GET TA COLUMNS AND OVERWRITE
print(directory_hourly)
files <- list.files(directory_hourly, pattern = "csv", full.names = TRUE)
for (i in 1:length(files)) {
  current_file <- files[i]
  print(current_file)
  
  new_filename <- paste(directory_hourly, "new_", basename(current_file), sep = "")
  print(new_filename)
  
  data = read.csv(file = current_file)
  
  data$Symbol <- substr(basename(current_file), 1, nchar(basename(current_file)) - 4)
  data$Timestamp <- as.POSIXct(data$Timestamp, format="%Y%m%d %H:%M")
  data$Year <- strftime(data$Timestamp, "%Y")
  data$Month <- strftime(data$Timestamp, "%b")
  data$Date <- strftime(data$Timestamp, "%Y-%m-%d")
  data$Time <- strftime(data$Timestamp, "%H:%M")
  data$HL2 <- (data$High + data$Low) / 2
  data$CandleSize <- (data$High - data$Low) / data$Close * 100
  
  EMA <- EMA(data[,('Close')], 10)
  RSI <- RSI(data[,('Close')], 20)
  SlowRSI <- EMA(RSI(data[,('Close')], 20), 10)
  MACD <- MACD(data[,('Close')], 10, 20, 10, percent = FALSE)
  MACD <- subset(MACD, select = -c(signal))
  colnames(MACD) <- c('MACD')
  
  data = data.frame(data, EMA, RSI, SlowRSI, MACD)
  
  Change1Period <- Delt(data$Close) * 100
  Change5Period <- Delt(data$Open, data$Close, k = 5) * 100
  Change10Period <- Delt(data$Open, data$Close, k = 10) * 100
  Change25Period <- Delt(data$Open, data$Close, k = 25) * 100
  Change50Period <- Delt(data$Open, data$Close, k = 50) * 100
  Change100Period <- Delt(data$Open, data$Close, k = 100) * 100
  Change200Period <- Delt(data$Open, data$Close, k = 200) * 100
  colnames(Change1Period) <- c('Change1Period')
  colnames(Change5Period) <- c('Change5Period')
  colnames(Change10Period) <- c('Change10Period')
  colnames(Change25Period) <- c('Change25Period')
  colnames(Change50Period) <- c('Change50Period')
  colnames(Change100Period) <- c('Change100Period')
  colnames(Change200Period) <- c('Change200Period')
  
  ChangeFuture1 <- lead(Change1Period, 1)
  colnames(ChangeFuture1) <- c('ChangeFuture1')
  
  EMAChange <- Delt(data$EMA) * 100
  RSIChange <- Delt(data$RSI) * 100
  SlowRSIChange <- Delt(data$SlowRSI) * 100
  MACDChange <- Delt(data$MACD) * 100
  colnames(EMAChange) <- c('EMAChange')
  colnames(RSIChange) <- c('RSIChange')
  colnames(SlowRSIChange) <- c('SlowRSIChange')
  colnames(MACDChange) <- c('MACDChange')
  
  data = data.frame(data, Change1Period, Change5Period, Change10Period, Change25Period, Change50Period, Change100Period, Change200Period,
    EMAChange, RSIChange, SlowRSIChange, MACDChange, ChangeFuture1)
  
  data <- slide(data, Var = 'Change5Period', NewVar = 'ChangeFuture5', slideBy = 5, reminder = FALSE)
  data <- slide(data, Var = 'Change10Period', NewVar = 'ChangeFuture10', slideBy = 10, reminder = FALSE)
  data <- slide(data, Var = 'Change25Period', NewVar = 'ChangeFuture25', slideBy = 25, reminder = FALSE)
  data <- slide(data, Var = 'Change50Period', NewVar = 'ChangeFuture50', slideBy = 50, reminder = FALSE)
  data <- slide(data, Var = 'Change100Period', NewVar = 'ChangeFuture100', slideBy = 100, reminder = FALSE)
  data <- slide(data, Var = 'Change200Period', NewVar = 'ChangeFuture200', slideBy = 200, reminder = FALSE)
  
  data <- data[c('Symbol', 'Row', 'Timestamp', 'Year', 'Month', 'Date', 'Time', 'Open', 'High', 'Low', 'Close', 'HL2', 'CandleSize'
    , 'Change1Period', 'Change5Period', 'Change10Period', 'Change25Period', 'Change50Period', 'Change100Period', 'Change200Period'
    , 'EMA', 'EMAChange', 'RSI', 'RSIChange', 'SlowRSI', 'SlowRSIChange', 'MACD', 'MACDChange'
    , 'ChangeFuture1', 'ChangeFuture5', 'ChangeFuture10', 'ChangeFuture25', 'ChangeFuture50', 'ChangeFuture100', 'ChangeFuture200')]
  
  write.table(
    data,
    file = new_filename,
    na = "",
    sep = ",",
    row.names = FALSE
  )
  
  file.remove(current_file)
  file.rename(new_filename, current_file)
}

# DAILY DATA - GET TA COLUMNS AND OVERWRITE
print(directory_daily)
files <- list.files(directory_daily, pattern = "csv", full.names = TRUE)
for (i in 1:length(files)) {
  current_file <- files[i]
  print(current_file)
  
  new_filename <- paste(directory_daily, "new_", basename(current_file), sep = "")
  print(new_filename)
  
  data = read.csv(file = current_file)
  
  data$Symbol <- substr(basename(current_file), 1, nchar(basename(current_file)) - 4)
  data$Timestamp <- as.POSIXct(data$Timestamp, format="%Y%m%d %H:%M")
  data$Year <- strftime(data$Timestamp, "%Y")
  data$Month <- strftime(data$Timestamp, "%b")
  data$Date <- strftime(data$Timestamp, "%Y-%m-%d")
  data$Time <- strftime(data$Timestamp, "%H:%M")
  data$HL2 <- (data$High + data$Low) / 2
  data$CandleSize <- (data$High - data$Low) / data$Close * 100
  
  EMA <- EMA(data[,('Close')], 10)
  RSI <- RSI(data[,('Close')], 20)
  SlowRSI <- EMA(RSI(data[,('Close')], 20), 10)
  MACD <- MACD(data[,('Close')], 10, 20, 10, percent = FALSE)
  MACD <- subset(MACD, select = -c(signal))
  colnames(MACD) <- c('MACD')
  
  data = data.frame(data, EMA, RSI, SlowRSI, MACD)
  
  Change1Period <- Delt(data$Close) * 100
  Change5Period <- Delt(data$Open, data$Close, k = 5) * 100
  Change10Period <- Delt(data$Open, data$Close, k = 10) * 100
  Change25Period <- Delt(data$Open, data$Close, k = 25) * 100
  Change50Period <- Delt(data$Open, data$Close, k = 50) * 100
  Change100Period <- Delt(data$Open, data$Close, k = 100) * 100
  Change200Period <- Delt(data$Open, data$Close, k = 200) * 100
  colnames(Change1Period) <- c('Change1Period')
  colnames(Change5Period) <- c('Change5Period')
  colnames(Change10Period) <- c('Change10Period')
  colnames(Change25Period) <- c('Change25Period')
  colnames(Change50Period) <- c('Change50Period')
  colnames(Change100Period) <- c('Change100Period')
  colnames(Change200Period) <- c('Change200Period')
  
  ChangeFuture1 <- lead(Change1Period, 1)
  colnames(ChangeFuture1) <- c('ChangeFuture1')
  
  EMAChange <- Delt(data$EMA) * 100
  RSIChange <- Delt(data$RSI) * 100
  SlowRSIChange <- Delt(data$SlowRSI) * 100
  MACDChange <- Delt(data$MACD) * 100
  colnames(EMAChange) <- c('EMAChange')
  colnames(RSIChange) <- c('RSIChange')
  colnames(SlowRSIChange) <- c('SlowRSIChange')
  colnames(MACDChange) <- c('MACDChange')
  
  data = data.frame(data, Change1Period, Change5Period, Change10Period, Change25Period, Change50Period, Change100Period, Change200Period,
                    EMAChange, RSIChange, SlowRSIChange, MACDChange, ChangeFuture1)
  
  data <- slide(data, Var = 'Change5Period', NewVar = 'ChangeFuture5', slideBy = 5, reminder = FALSE)
  data <- slide(data, Var = 'Change10Period', NewVar = 'ChangeFuture10', slideBy = 10, reminder = FALSE)
  data <- slide(data, Var = 'Change25Period', NewVar = 'ChangeFuture25', slideBy = 25, reminder = FALSE)
  data <- slide(data, Var = 'Change50Period', NewVar = 'ChangeFuture50', slideBy = 50, reminder = FALSE)
  data <- slide(data, Var = 'Change100Period', NewVar = 'ChangeFuture100', slideBy = 100, reminder = FALSE)
  data <- slide(data, Var = 'Change200Period', NewVar = 'ChangeFuture200', slideBy = 200, reminder = FALSE)
  
  data <- data[c('Symbol', 'Row', 'Timestamp', 'Year', 'Month', 'Date', 'Time', 'Open', 'High', 'Low', 'Close', 'HL2', 'CandleSize'
                 , 'Change1Period', 'Change5Period', 'Change10Period', 'Change25Period', 'Change50Period', 'Change100Period', 'Change200Period'
                 , 'EMA', 'EMAChange', 'RSI', 'RSIChange', 'SlowRSI', 'SlowRSIChange', 'MACD', 'MACDChange'
                 , 'ChangeFuture1', 'ChangeFuture5', 'ChangeFuture10', 'ChangeFuture25', 'ChangeFuture50', 'ChangeFuture100', 'ChangeFuture200')]
  
  write.table(
    data,
    file = new_filename,
    na = "",
    sep = ",",
    row.names = FALSE
  )
  
  file.remove(current_file)
  file.rename(new_filename, current_file)
}

print(Sys.time() - start_time)