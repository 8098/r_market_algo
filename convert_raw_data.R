directories <- c("~/Git/marketalgo/data_qc_raw_hourly/", "~/Git/marketalgo/data_qc_raw_daily/")

for (i in 1:length(directories)) {
  current_dir <- directories[i]
  cat(current_dir, "\n")

  files <- list.files(current_dir, pattern = "csv", full.names = TRUE)
  
  for (i in 1:length(files)) {
    current_file <- files[i]
    cat(current_file, "\n")
  
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