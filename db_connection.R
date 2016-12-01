require(RODBC)
# require(rsqlserver)
# require(devtools)
# install_github("jmp75/rClr", build_vignettes=TRUE)
# install_github('agstudy/rsqlserver', build_vignettes=TRUE) 

start.time <- proc.time()

file = "~/Git/marketalgo/test/audusd.csv"
data = read.csv(file = file, header = FALSE)
colnames(data) <- c('Timestamp', 'Open', 'High', 'Low', 'Close')

connection <- odbcDriverConnect("Driver={SQL Server};server=138.68.59.249;database=MarketAlgo;uid=sa;pwd=LukeSkywalker!")
# results <- sqlQuery(connection, "SELECT TOP 1000 * FROM [dbo].[Test]")
# View(results)
sqlSave(connection, data, tablename = 'dbo.Test2', append = TRUE, rownames = FALSE, fast = TRUE)

# driver <- dbDriver("SqlServer")
# connection <- dbConnect(driver, url = "server=138.68.59.249;database=MarketAlgo;user=sa;password=LukeSkywalker!")
# dbBulkCopy(connection, "[dbo].[Test2]", value = data)

end.time <- proc.time()
duration.time <- end.time - start.time
print(paste("Process duration (seconds):", duration.time))