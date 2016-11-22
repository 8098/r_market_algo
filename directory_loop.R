in_path = "~/Git/marketalgo/data_qc_raw_hourly/"
out_path = "~/Git/marketalgo/test/"
files <- list.files(in_path, pattern = "csv", full.names = TRUE)
for (i in 1:length(files)) {
  cat(basename(files[i]), files[i])
  cat("\n")
  data <- read.table(files[i], header = FALSE, sep = ",")
  colnames(data) <- c('Timestamp', 'Open', 'High', 'Low', 'Close')
  write.table(
    data,
    file = paste(out_path, basename(files[i])),
    sep = ",",
    row.names = FALSE
  )
}