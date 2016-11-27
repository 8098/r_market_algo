temp_path = "~/Git/marketalgo/temp/"
urls_hourly = "~/Git/marketalgo/download_links/DownloadLinksHourly.csv"
destination_path_hourly = "~/Git/marketalgo/data_qc_raw_hourly"
urls_daily = "~/Git/marketalgo/download_links/DownloadLinksDaily.csv"
destination_path_daily = "~/Git/marketalgo/data_qc_raw_daily"

print("Downlaoding hourly data...")
data = read.csv(file = urls_hourly, header = TRUE)
symbols <- data[['Symbol']]
urls <- data[['URL']]
for(i in urls) {
  temp_destination <- paste(temp_path, basename(i), sep = "")
  destination <- destination_path_hourly
  print(paste("URL:", i))
  print(paste("Temp Destination:", temp_destination))
  download.file(url = i, destfile = temp_destination, quiet = TRUE)
  print(paste("Final Destination:", destination))
  unzip(zipfile = temp_destination, exdir = destination)
  file.remove(temp_destination)
}

print("Downlaoding daily data...")
data = read.csv(file = urls_daily, header = TRUE)
symbols <- data[['Symbol']]
urls <- data[['URL']]
for(i in urls) {
  temp_destination <- paste(temp_path, basename(i), sep = "")
  destination <- destination_path_daily
  print(paste("URL:", i))
  print(paste("Temp Destination:", temp_destination))
  download.file(url = i, destfile = temp_destination, quiet = TRUE)
  print(paste("Final Destination:", destination))
  unzip(zipfile = temp_destination, exdir = destination)
  file.remove(temp_destination)
}