# VARIABLES
start_time <- Sys.time()
source_hourly <- "~/marketalgo/data_qc_raw_hourly/"
source_daily <-"~/marketalgo/data_qc_raw_daily/"
destination_hourly <- "~/marketalgo/data_qc_hourly/"
destination_daily <-"~/marketalgo/data_qc_daily/"

# HOURLY DATA - DELETE EXISTING FILES
print(destination_hourly)
files <- list.files(destination_hourly, pattern = "csv", full.names = TRUE)
for (i in 1:length(files)) {
  current_file <- files[i]
  print(current_file)

  file.remove(current_file)
}

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
  data$Row <- seq.int(nrow(data))
  colnames(data) <- c('Timestamp', 'Open', 'High', 'Low', 'Close', 'Row')
  data <- data[c('Row', 'Timestamp', 'Open', 'High', 'Low', 'Close')]

  write.table(
    data,
    file = new_filename,
    sep = ",",
    row.names = FALSE
  )
}

# DAILY DATA - DELETE EXISTING FILES
print(destination_daily)
files <- list.files(destination_daily, pattern = "csv", full.names = TRUE)
for (i in 1:length(files)) {
  current_file <- files[i]
  print(current_file)
  
  file.remove(current_file)
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
  data$Row <- seq.int(nrow(data))
  colnames(data) <- c('Timestamp', 'Open', 'High', 'Low', 'Close', 'Row')
  data <- data[c('Row', 'Timestamp', 'Open', 'High', 'Low', 'Close')]
  
  write.table(
    data,
    file = new_filename,
    sep = ",",
    row.names = FALSE
  )
}

print(Sys.time() - start_time)