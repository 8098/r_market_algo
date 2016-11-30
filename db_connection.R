require(RODBC)

file = "~/Git/marketalgo/test/audusd.csv"
data = read.csv(file = file, header = FALSE)
colnames(data) <- c('Timestamp', 'Open', 'High', 'Low', 'Close')

connection <- odbcDriverConnect("Driver={SQL Server};server=138.68.59.249;database=MarketAlgo;uid=sa;pwd=LukeSkywalker!")
# results <- sqlQuery(connection, "SELECT TOP 1000 * FROM [dbo].[Test]")
# View(results)
sqlSave(connection, data, tablename = 'dbo.Test', append = TRUE, rownames = FALSE)