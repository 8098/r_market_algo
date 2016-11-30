# VARIABLES
source_hourly <- "~/Git/marketalgo/data_qc_raw_hourly/"
source_daily <-"~/Git/marketalgo/data_qc_raw_daily/"
destination_hourly <- "~/Git/marketalgo/data_qc_hourly/"
destination_daily <-"~/Git/marketalgo/data_qc_daily/"

# HOURLY DATA - GET DATA IN FILE, ADD HEADERS, REMOVE UNUSED COLUMNS AND OVERWRITE
print(source_hourly)
files <- list.files(source_hourly, pattern = "csv", full.names = TRUE)
for (i in 1:length(files)) {
  current_file <- files[i]
  print(current_file)

  filename <- paste(source_hourly, basename(current_file), sep = "")
  new_filename <- paste(destination_hourly, basename(current_file), sep = "")
  print(new_filename)

  data <- read.table(current_file, header = FALSE, sep = ",")
  data <- data[,c(1:5)]
  colnames(data) <- c('Timestamp', 'Open', 'High', 'Low', 'Close')

  write.table(
    data,
    file = new_filename,
    sep = ",",
    row.names = FALSE
  )
}

# DAILY DATA - GET DATA IN FILE, ADD HEADERS, REMOVE UNUSED COLUMNS AND OVERWRITE
print(source_daily)
files <- list.files(source_daily, pattern = "csv", full.names = TRUE)
for (i in 1:length(files)) {
  current_file <- files[i]
  print(current_file)
  
  filename <- paste(source_daily, basename(current_file), sep = "")
  new_filename <- paste(destination_daily, basename(current_file), sep = "")
  print(new_filename)
  
  data <- read.table(current_file, header = FALSE, sep = ",")
  data <- data[,c(1:5)]
  colnames(data) <- c('Timestamp', 'Open', 'High', 'Low', 'Close')
  
  write.table(
    data,
    file = new_filename,
    sep = ",",
    row.names = FALSE
  )
}