require(TTR)

# VARIABLES
directory_hourly = "~/Git/marketalgo/data_qc_hourly/"
directory_daily = "~/Git/marketalgo/data_qc_daily/"

# HOURLY DATA - GET TA COLUMNS AND OVERWRITE
print(directory_hourly)
files <- list.files(directory_hourly, pattern = "csv", full.names = TRUE)
for (i in 1:length(files)) {
  current_file <- files[i]
  print(current_file)
  
  new_filename <- paste(directory_hourly, "new_", basename(current_file), sep = "")
  print(new_filename)
  
  data = read.csv(file = current_file)
  
  EMA <- EMA(data[,('Close')], 10)
  RSI <- RSI(data[,('Close')], 20)
  SlowRSI <- EMA(RSI(data[,('Close')], 20), 10)
  MACD <- MACD(data[,('Close')], 10, 20, 10, percent = FALSE)
  
  data = data.frame(data, EMA, RSI, SlowRSI, MACD)
  names(data)[names(data) == 'macd'] <- 'MACD'
  data <- subset(data, select = -c(signal) )
  
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
  
  EMA <- EMA(data[,('Close')], 10)
  RSI <- RSI(data[,('Close')], 20)
  SlowRSI <- EMA(RSI(data[,('Close')], 20), 10)
  MACD <- MACD(data[,('Close')], 10, 20, 10, percent = FALSE)
  
  data = data.frame(data, EMA, RSI, SlowRSI, MACD)
  names(data)[names(data) == 'macd'] <- 'MACD'
  data <- subset(data, select = -c(signal) )
  
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