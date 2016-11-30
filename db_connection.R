require(RODBC)
require(RSQLServer)
require(devtools)
install_github("jmp75/rClr", build_vignettes=TRUE)
install_github('agstudy/rsqlserver', build_vignettes=TRUE) 

start
file = "~/Git/marketalgo/test/audusd.csv"
data = read.csv(file = file, header = FALSE)
colnames(data) <- c('Timestamp', 'Open', 'High', 'Low', 'Close')

# connection <- odbcDriverConnect("Driver={SQL Server};server=138.68.59.249;database=MarketAlgo;uid=sa;pwd=LukeSkywalker!")
# results <- sqlQuery(connection, "SELECT TOP 1000 * FROM [dbo].[Test]")
# View(results)
# sqlSave(connection, data, tablename = 'dbo.Test2', append = TRUE, rownames = FALSE)

driver <- dbDriver("SqlServer")
connection <- dbConnect(driver, url = "server=138.68.59.249;database=MarketAlgo;uid=sa;pwd=LukeSkywalker!")
results <- dbSendQuery(connection, "SELECT TOP 100 * FROM dbo.Test")
View(results)