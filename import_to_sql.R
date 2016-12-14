require(RODBC)

# VARIABLES
start_time <- Sys.time()
directory_hourly = "~/marketalgo/data_qc_hourly/"
directory_daily = "~/marketalgo/data_qc_daily/"
table_hourly = "ImportQCHourly"
table_daily = "ImportQCDaily"
connection <- odbcDriverConnect("Driver={ODBC Driver 13 for SQL Server};server=localhost;database=MarketAlgo;uid=sa;pwd=LukeSkywalker!")

# HOURLY DATA - INSERT DATA TO SQL TABLE
print(directory_hourly)
files <- list.files(directory_hourly, pattern = "csv", full.names = TRUE)
sqlClear(connection, table_hourly)
for (i in 1:length(files)) {
  current_file <- files[i]
  print(current_file)
  
  data = read.csv(file = current_file, header = TRUE, sep = ",")
  sqlSave(connection, data, tablename = table_hourly, append = TRUE, rownames = FALSE, fast = TRUE)
  odbcClose(connection)
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
  odbcClose(connection)
}

print(Sys.time() - start_time)