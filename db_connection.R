require(RODBC)

connection <- odbcDriverConnect("Driver={SQL Server};server=localhost;database=MarketAlgo;uid=sa;pwd=LukeSkywalker!")
results <- sqlQuery(connection, "SELECT TOP 1000 * FROM [dbo].[ImportQCHourlyConverted]")
View(results)