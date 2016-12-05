require(TTR)
require(quantmod)

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
  data$Date <- strftime(data$Timestamp, "%Y-%m-%d")
  data$Time <- strftime(data$Timestamp, "%H:%M")
  data$HL2 <- (data$High + data$Low) / 2
  data$CandleSize <- (data$High - data$Low) / data$Close
  
  EMA <- EMA(data[,('Close')], 10)
  RSI <- RSI(data[,('Close')], 20)
  SlowRSI <- EMA(RSI(data[,('Close')], 20), 10)
  MACD <- MACD(data[,('Close')], 10, 20, 10, percent = FALSE)
  
  data = data.frame(data, EMA, RSI, SlowRSI, MACD)
  
  Change <- Delt(data$Close) * 100
  EMAChange <- Delt(data$EMA) * 100
  RSIChange <- Delt(data$RSI) * 100
  SlowRSIChange <- Delt(data$SlowRSI) * 100
  MACDChange <- Delt(data$macd) * 100
  
  data = data.frame(data, Change, EMAChange, RSIChange, SlowRSIChange, MACDChange)
  
  names(data)[names(data) == 'macd'] <- 'MACD'
  names(data)[names(data) == 'Delt.1.arithmetic'] <- 'Change'
  names(data)[names(data) == 'Delt.1.arithmetic.1'] <- 'EMAChange'
  names(data)[names(data) == 'Delt.1.arithmetic.2'] <- 'RSIChange'
  names(data)[names(data) == 'Delt.1.arithmetic.3'] <- 'SlowRSIChange'
  names(data)[names(data) == 'Delt.1.arithmetic.4'] <- 'MACDChange'
  data <- subset(data, select = -c(signal))
  data <- data[c('Symbol', 'Row', 'Timestamp', 'Date', 'Time', 'Open', 'High', 'Low'
                 , 'Close', 'HL2', 'CandleSize', 'Change', 'EMA', 'EMAChange', 'RSI'
                 , 'RSIChange', 'SlowRSI', 'SlowRSIChange', 'MACD', 'MACDChange')]
  
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
  data$Date <- strftime(data$Timestamp, "%Y-%m-%d")
  data$Time <- strftime(data$Timestamp, "%H:%M")
  data$HL2 <- (data$High + data$Low) / 2
  data$CandleSize <- (data$High - data$Low) / data$Close
  
  EMA <- EMA(data[,('Close')], 10)
  RSI <- RSI(data[,('Close')], 20)
  SlowRSI <- EMA(RSI(data[,('Close')], 20), 10)
  MACD <- MACD(data[,('Close')], 10, 20, 10, percent = FALSE)
  
  data = data.frame(data, EMA, RSI, SlowRSI, MACD)
  
  Change <- Delt(data$Close) * 100
  EMAChange <- Delt(data$EMA) * 100
  RSIChange <- Delt(data$RSI) * 100
  SlowRSIChange <- Delt(data$SlowRSI) * 100
  MACDChange <- Delt(data$macd) * 100
  
  data = data.frame(data, Change, EMAChange, RSIChange, SlowRSIChange, MACDChange)
  
  names(data)[names(data) == 'macd'] <- 'MACD'
  names(data)[names(data) == 'Delt.1.arithmetic'] <- 'Change'
  names(data)[names(data) == 'Delt.1.arithmetic.1'] <- 'EMAChange'
  names(data)[names(data) == 'Delt.1.arithmetic.2'] <- 'RSIChange'
  names(data)[names(data) == 'Delt.1.arithmetic.3'] <- 'SlowRSIChange'
  names(data)[names(data) == 'Delt.1.arithmetic.4'] <- 'MACDChange'
  data <- subset(data, select = -c(signal) )
  data <- data[c('Symbol', 'Row', 'Timestamp', 'Date', 'Time', 'Open', 'High', 'Low'
                 , 'Close', 'HL2', 'CandleSize', 'Change', 'EMA', 'EMAChange', 'RSI'
                 , 'RSIChange', 'SlowRSI', 'SlowRSIChange', 'MACD', 'MACDChange')]
  
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