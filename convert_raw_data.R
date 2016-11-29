# VARIABLES
directories <- c("~/Git/marketalgo/data_qc_hourly/", "~/Git/marketalgo/data_qc_daily/")

# GET FILES IN DIRECTORY
for (i in 1:length(directories)) {
  current_dir <- directories[i]
  print(current_dir)

  files <- list.files(current_dir, pattern = "csv", full.names = TRUE)
  
  # GET DATA IN FILE, ADD HEADERS, REMOVE UNUSED COLUMNS AND OVERWRITE
  for (i in 1:length(files)) {
    current_file <- files[i]
    print(current_file)
  
    filename <- paste(current_dir, basename(current_file), sep = "")
    new_filename <- paste(current_dir, "new_", basename(current_file), sep = "")
  
    data <- read.table(current_file, header = FALSE, sep = ",")
    data <- data[,c(1:5)]
    colnames(data) <- c('Timestamp', 'Open', 'High', 'Low', 'Close')
  
    write.table(
      data,
      file = new_filename,
      sep = ",",
      row.names = FALSE
    )
  
    file.remove(filename)
    file.rename(new_filename, filename)
  }
}