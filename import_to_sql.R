require(RODBC)
# require(rsqlserver)
# require(devtools)
# install_github("jmp75/rClr", build_vignettes=TRUE)
# install_github('agstudy/rsqlserver', build_vignettes=TRUE) 

# VARIABLES
start_time <- Sys.time()
directory_hourly = "~/Git/marketalgo/data_qc_hourly/"
directory_daily = "~/Git/marketalgo/data_qc_daily/"
table_hourly = "ImportQCHourly"
table_daily = "ImportQCDaily"
connection <- odbcDriverConnect("Driver={SQL Server};server=localhost;database=MarketAlgo;uid=sa;pwd=LukeSkywalker!")

# HOURLY DATA - INSERT DATA TO SQL TABLE
print(directory_hourly)
files <- list.files(directory_hourly, pattern = "csv", full.names = TRUE)
sqlClear(connection, table_hourly)
for (i in 1:length(files)) {
  current_file <- files[i]
  print(current_file)

  data = read.csv(file = current_file, header = TRUE, sep = ",")
  sqlSave(connection, data, tablename = table_hourly, append = TRUE, rownames = FALSE, fast = TRUE)
}

# DAILY DATA - INSERT DATA TO SQL TABLE
print(directory_daily)
files <- list.files(directory_daily, pattern = "csv", full.names = TRUE)
sqlClear(connection, table_daily)
for (i in 1:length(files)) {
  current_file <- files[i]
  print(current_file)
  
  data = read.csv(file = current_file, header = TRUE, sep = ",")
  sqlSave(connection, data, tablename = table_daily, append = TRUE, rownames = FALSE, fast = TRUE)
}

print(Sys.time() - start_time)