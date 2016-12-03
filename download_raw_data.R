# VARIABLES
start_time <- Sys.time()
temp_path = "~/marketalgo/temp/"
urls_hourly = "~/marketalgo/supporting_files/DownloadLinksHourly.csv"
urls_daily = "~/marketalgo/supporting_files/DownloadLinksDaily.csv"
destination_hourly = "~/marketalgo/data_qc_raw_hourly"
destination_daily = "~/marketalgo/data_qc_raw_daily"

# HOURLY DATA - DELETE EXISTING FILES
print(destination_hourly)
files <- list.files(destination_hourly, pattern = "csv", full.names = TRUE)
for (i in 1:length(files)) {
  current_file <- files[i]
  print(current_file)
  
  file.remove(current_file)
}

# HOURLY DATA - DOWNLOAD
print("Downloading hourly data...")
data = read.csv(file = urls_hourly, header = TRUE)
symbols <- data[['Symbol']]
urls <- data[['URL']]
for(i in urls) {
  temp_destination <- paste(temp_path, basename(i), sep = "")
  destination <- destination_hourly
  print(paste("URL:", i))
  print(paste("Temp Destination:", temp_destination))
  download.file(url = i, destfile = temp_destination, quiet = TRUE)
  print(paste("Final Destination:", destination))
  unzip(zipfile = temp_destination, exdir = destination)
  file.remove(temp_destination)
}

# DAILY DATA - DOWNLOAD
print("Downloading daily data...")
data = read.csv(file = urls_daily, header = TRUE)
symbols <- data[['Symbol']]
urls <- data[['URL']]
for(i in urls) {
  temp_destination <- paste(temp_path, basename(i), sep = "")
  destination <- destination_daily
  print(paste("URL:", i))
  print(paste("Temp Destination:", temp_destination))
  download.file(url = i, destfile = temp_destination, quiet = TRUE)
  print(paste("Final Destination:", destination))
  unzip(zipfile = temp_destination, exdir = destination)
  file.remove(temp_destination)
}

print(Sys.time() - start_time)