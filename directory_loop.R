in_path = '~/Git/marketalgo/data_raw_qc'
out_path = '~/Git/marketalgo/test'
file.names <- dir(path, pattern ='csv')
for(i in 1:length(file.names)){
  cat(file.names[i], files[i])
  cat("\n")
}