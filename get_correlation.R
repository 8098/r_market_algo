require(RODBC)
require(caret)
require(Hmisc)

# VARIABLES
set.seed(8098)
start_time <- Sys.time()
connection <- odbcDriverConnect("Driver={ODBC Driver 13 for SQL Server};server=localhost;database=MarketAlgo;uid=sa;pwd=LukeSkywalker!")

# HELPER FUNCTIONS
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(Column1 = rownames(m)[row(m)[ut]],
             Column2 = rownames(m)[col(m)[ut]],
             Correlation=t(m)[ut])
}

# GET DATA FROM SQL
data <- sqlQuery(connection, "SELECT * FROM [QCHourlyAndDaily] WHERE [Symbol] = 'audusd'")
odbcClose(connection)


#TRANSFORM DATA
data <- subset(data, select = -c(Symbol, Row, Timestamp, Year, Date, Open, High, Low, Close
    , HL2, CandleSize, EMA, DailyHL2, DailyCandleSize, DailyEMA))

dummy <- dummyVars(" ~ .", data = data)
data <- data.frame(predict(dummy, newdata = data))

# GET CORRELATION
corMasterList <- rcorr(as.matrix(data), type="pearson")
corList <- flattenSquareMatrix(corMasterList$r)
corList <- corList[order(-abs(corList$Correlation)),]

# EXPORT RESULTS
write.table(
  corList,
  file = 'Correlation Results.csv',
  na = "",
  sep = ",",
  row.names = FALSE
)

# PRINT PROCESSING TIME
print(Sys.time() - start_time)