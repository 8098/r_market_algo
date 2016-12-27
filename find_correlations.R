require(RODBC)
require(caret)
require(pROC)
require(doMC)
require(plyr)
require(dplyr)

##### VARIABLES #####
set.seed(8098)
registerDoMC(cores = 6)
start_time <- Sys.time()
file_destination <- "~/marketalgo/correlations/"
outcomeName <- "ChangeFuture5"
resultsCorrelation <- data.frame(matrix(vector(), 0, 5, dimnames=list(c(), c("symbol", "column", "abs.correlation", "raw.correlation", "pct.occurence"))), stringsAsFactors = FALSE)
connection_string <- "Driver={ODBC Driver 13 for SQL Server};server=localhost;database=MarketAlgo;uid=sa;pwd=LukeSkywalker!"

##### GET SYMBOLS FROM SQL #####
connection <- odbcDriverConnect(connection_string)
symbols <- sqlQuery(connection, "SELECT * FROM Symbols")
odbcClose(connection)

for (i in 1:length(symbols$Symbol)) {
  symbol <- as.character(symbols$Symbol[i])
  print(symbol)
  
  ##### GET DATA FROM SQL #####
  connection <- odbcDriverConnect(connection_string)
  rawSql <- sqlQuery(connection, paste("SELECT * FROM QCHourlyAndDaily WHERE Symbol = '", symbol, "'", sep = ""), rows_at_time = 1000)
  odbcClose(connection)
  
  
  ##### TRANSFORM DATA #####
  data <- subset(rawSql, select = -c(Row, Symbol, Timestamp, Year, Month, Day, Date, Time, Open, High, Low, Close, HL2, CandleSize, EMA20, EMA100
                                     , Change50Period, Change100Period, Change200Period
                                     , DailyEMA20, DailyEMA100
                                     , DailyChange50Period, DailyChange100Period, DailyChange200Period, DailyChangeEMA20, DailyChangeEMA100
                                     , ChangeFuture1, ChangeFuture10, ChangeFuture15, ChangeFuture20, ChangeFuture50, ChangeFuture100, ChangeFuture200))
  
  
  data$RSI5 <- cut(data$RSI5, pretty(data$RSI5, n = 10))
  data$RSI14 <- cut(data$RSI14, pretty(data$RSI14, n = 10))
  data$SlowRSI5 <- cut(data$SlowRSI5, pretty(data$SlowRSI5, n = 10))
  data$SlowRSI14 <- cut(data$SlowRSI14, pretty(data$SlowRSI14, n = 10))
  data$MACD <- ifelse(data$MACD > 0, 'Up', 'Down')
  data$MACDSignal <- ifelse(data$MACDSignal > 0, 'Up', 'Down')
  data$BBPct <- cut(data$BBPct, pretty(data$BBPct, n = 10))
  data$BBWidth <- cut(data$BBWidth, pretty(data$BBWidth, n = 10))
  data$StochFastK <- cut(data$StochFastK, pretty(data$StochFastK, n = 10))
  data$StochFastD <- cut(data$StochFastD, pretty(data$StochFastD, n = 10))
  data$StochSlowD <- cut(data$StochSlowD, pretty(data$StochSlowD, n = 10))
  
  data$Change1Period <- ifelse(data$Change1Period > 0, 'Up', 'Down')
  data$Change5Period <- ifelse(data$Change5Period > 0, 'Up', 'Down')
  data$Change10Period <- ifelse(data$Change10Period > 0, 'Up', 'Down')
  data$Change15Period <- ifelse(data$Change15Period > 0, 'Up', 'Down')
  data$Change20Period <- ifelse(data$Change20Period > 0, 'Up', 'Down')
  data$ChangeEMA20 <- ifelse(data$ChangeEMA20 > 0, 'Up', 'Down')
  data$ChangeEMA100 <- ifelse(data$ChangeEMA100 > 0, 'Up', 'Down')
  data$ChangeRSI5 <- ifelse(data$ChangeRSI5 > 0, 'Up', 'Down')
  data$ChangeRSI14 <- ifelse(data$ChangeRSI14 > 0, 'Up', 'Down')
  data$ChangeSlowRSI5 <- ifelse(data$ChangeSlowRSI5 > 0, 'Up', 'Down')
  data$ChangeSlowRSI14 <- ifelse(data$ChangeSlowRSI14 > 0, 'Up', 'Down')
  data$ChangeMACD <- ifelse(data$ChangeMACD > 0, 'Up', 'Down')
  data$ChangeMACDSignal <- ifelse(data$ChangeMACDSignal > 0, 'Up', 'Down')
  data$ChangeBBPct <- ifelse(data$ChangeBBPct > 0, 'Up', 'Down')
  data$ChangeBBWidth <- ifelse(data$ChangeBBWidth > 0, 'Up', 'Down')
  data$ChangeStochFastK <- ifelse(data$ChangeStochFastK > 0, 'Up', 'Down')
  data$ChangeStochFastD <- ifelse(data$ChangeStochFastD > 0, 'Up', 'Down')
  data$ChangeStochSlowD <- ifelse(data$ChangeStochSlowD > 0, 'Up', 'Down')
  
  data$ConsecChange1Period <- cut(data$ConsecChange1Period, pretty(data$ConsecChange1Period))
  data$ConsecChangeEMA20 <- cut(data$ConsecChangeEMA20, pretty(data$ConsecChangeEMA20))
  data$ConsecChangeEMA100 <- cut(data$ConsecChangeEMA100, pretty(data$ConsecChangeEMA100))
  data$ConsecChangeRSI5 <- cut(data$ConsecChangeRSI5, pretty(data$ConsecChangeRSI5))
  data$ConsecChangeRSI14 <- cut(data$ConsecChangeRSI14, pretty(data$ConsecChangeRSI14))
  data$ConsecChangeSlowRSI5 <- cut(data$ConsecChangeSlowRSI5, pretty(data$ConsecChangeSlowRSI5))
  data$ConsecChangeSlowRSI14 <- cut(data$ConsecChangeSlowRSI14, pretty(data$ConsecChangeSlowRSI14))
  data$ConsecChangeMACD <- cut(data$ConsecChangeMACD, pretty(data$ConsecChangeMACD))
  data$ConsecChangeMACDSignal <- cut(data$ConsecChangeMACDSignal, pretty(data$ConsecChangeMACDSignal))
  data$ConsecChangeBBPct <- cut(data$ConsecChangeBBPct, pretty(data$ConsecChangeBBPct))
  data$ConsecChangeBBWidth <- cut(data$ConsecChangeBBWidth, pretty(data$ConsecChangeBBWidth))
  data$ConsecChangeStochFastK <- cut(data$ConsecChangeStochFastK, pretty(data$ConsecChangeStochFastK))
  data$ConsecChangeStochFastD <- cut(data$ConsecChangeStochFastD, pretty(data$ConsecChangeStochFastD))
  data$ConsecChangeStochSlowD <- cut(data$ConsecChangeStochSlowD, pretty(data$ConsecChangeStochSlowD))
  
  data$DailyCandleSize <- cut(data$DailyCandleSize, pretty(data$DailyCandleSize, n = 10))
  data$DailyRSI5 <- cut(data$DailyRSI5, pretty(data$DailyRSI5, n = 10))
  data$DailyRSI14 <- cut(data$DailyRSI14, pretty(data$DailyRSI14, n = 10))
  data$DailySlowRSI5 <- cut(data$DailySlowRSI5, pretty(data$DailySlowRSI5, n = 10))
  data$DailySlowRSI14 <- cut(data$DailySlowRSI14, pretty(data$DailySlowRSI14, n = 10))
  data$DailyMACD <- ifelse(data$DailyMACD > 0, 'Up', 'Down')
  data$DailyMACDSignal <- ifelse(data$DailyMACDSignal > 0, 'Up', 'Down')
  data$DailyBBPct <- cut(data$DailyBBPct, pretty(data$DailyBBPct, n = 10))
  data$DailyBBWidth <- cut(data$DailyBBWidth, pretty(data$DailyBBWidth, n = 10))
  data$DailyStochFastK <- cut(data$DailyStochFastK, pretty(data$DailyStochFastK, n = 10))
  data$DailyStochFastD <- cut(data$DailyStochFastD, pretty(data$DailyStochFastD, n = 10))
  data$DailyStochSlowD <- cut(data$DailyStochSlowD, pretty(data$DailyStochSlowD, n = 10))
  
  data$DailyChange1Period <- ifelse(data$DailyChange1Period > 0, 'Up', 'Down')
  data$DailyChange5Period <- ifelse(data$DailyChange5Period > 0, 'Up', 'Down')
  data$DailyChange10Period <- ifelse(data$DailyChange10Period > 0, 'Up', 'Down')
  data$DailyChange15Period <- ifelse(data$DailyChange15Period > 0, 'Up', 'Down')
  data$DailyChange20Period <- ifelse(data$DailyChange20Period > 0, 'Up', 'Down')
  data$DailyChangeRSI5 <- ifelse(data$DailyChangeRSI5 > 0, 'Up', 'Down')
  data$DailyChangeRSI14 <- ifelse(data$DailyChangeRSI14 > 0, 'Up', 'Down')
  data$DailyChangeSlowRSI5 <- ifelse(data$DailyChangeSlowRSI5 > 0, 'Up', 'Down')
  data$DailyChangeSlowRSI14 <- ifelse(data$DailyChangeSlowRSI14 > 0, 'Up', 'Down')
  data$DailyChangeMACD <- ifelse(data$DailyChangeMACD > 0, 'Up', 'Down')
  data$DailyChangeMACDSignal <- ifelse(data$DailyChangeMACDSignal > 0, 'Up', 'Down')
  data$DailyChangeBBPct <- ifelse(data$DailyChangeBBPct > 0, 'Up', 'Down')
  data$DailyChangeBBWidth <- ifelse(data$DailyChangeBBWidth > 0, 'Up', 'Down')
  data$DailyChangeStochFastK <- ifelse(data$DailyChangeStochFastK > 0, 'Up', 'Down')
  data$DailyChangeStochFastD <- ifelse(data$DailyChangeStochFastD > 0, 'Up', 'Down')
  data$DailyChangeStochSlowD <- ifelse(data$DailyChangeStochSlowD > 0, 'Up', 'Down')
  
  data$DailyConsecChange1Period <- cut(data$DailyConsecChange1Period, pretty(data$DailyConsecChange1Period))
  data$DailyConsecChangeEMA20 <- cut(data$DailyConsecChangeEMA20, pretty(data$DailyConsecChangeEMA20))
  data$DailyConsecChangeEMA100 <- cut(data$DailyConsecChangeEMA100, pretty(data$DailyConsecChangeEMA100))
  data$DailyConsecChangeRSI5 <- cut(data$DailyConsecChangeRSI5, pretty(data$DailyConsecChangeRSI5))
  data$DailyConsecChangeRSI14 <- cut(data$DailyConsecChangeRSI14, pretty(data$DailyConsecChangeRSI14))
  data$DailyConsecChangeSlowRSI5 <- cut(data$DailyConsecChangeSlowRSI5, pretty(data$DailyConsecChangeSlowRSI5))
  data$DailyConsecChangeSlowRSI14 <- cut(data$DailyConsecChangeSlowRSI14, pretty(data$DailyConsecChangeSlowRSI14))
  data$DailyConsecChangeMACD <- cut(data$DailyConsecChangeMACD, pretty(data$DailyConsecChangeMACD))
  data$DailyConsecChangeMACDSignal <- cut(data$DailyConsecChangeMACDSignal, pretty(data$DailyConsecChangeMACDSignal))
  data$DailyConsecChangeBBPct <- cut(data$DailyConsecChangeBBPct, pretty(data$DailyConsecChangeBBPct))
  data$DailyConsecChangeBBWidth <- cut(data$DailyConsecChangeBBWidth, pretty(data$DailyConsecChangeBBWidth))
  data$DailyConsecChangeStochFastK <- cut(data$DailyConsecChangeStochFastK, pretty(data$DailyConsecChangeStochFastK))
  data$DailyConsecChangeStochFastD <- cut(data$DailyConsecChangeStochFastD, pretty(data$DailyConsecChangeStochFastD))
  data$DailyConsecChangeStochSlowD <- cut(data$DailyConsecChangeStochSlowD, pretty(data$DailyConsecChangeStochSlowD))
  
  data[[outcomeName]] <- ifelse(data[[outcomeName]] > 0, 1, 0)
  
  dummy <- dummyVars(" ~ .", data = data, fullRank = TRUE)
  data <- data.frame(predict(dummy, newdata = data))
  
  
  ##### AGGREGATE RESULTS #####
  for (i in 1:length(names(data))) {
    currentColumn <- names(data[i])
    corCheck <- data.frame(data[[currentColumn]], data[[outcomeName]])
    colnames(corCheck) <- c('column', 'outcome')
    corCheck <- na.omit(corCheck[corCheck$column == 1, ])
    
    countFullData <- nrow(data)
    countColumn <- nrow(corCheck)
    pctOccurence <- countColumn / countFullData * 100
    countUps <- sum(corCheck$outcome)
    
    rawCorrelation <- countUps /  countColumn
    absCorrelation <- ifelse(rawCorrelation < 0.5, 1 - rawCorrelation, rawCorrelation)
    resultsCorrelation <- rbind(resultsCorrelation, data.frame(symbol = symbol, column = currentColumn, abs.correlation = absCorrelation
                                                               , raw.correlation = rawCorrelation, pct.occurence = pctOccurence))
  }
  
  resultsCorrelation <- resultsCorrelation[resultsCorrelation$column != outcomeName,]
}

##### WRITE RESULTS TO CSV #####
write.table(
  resultsCorrelation,
  file = paste(file_destination, "resultsCorrelation-", outcomeName, " ", as.character(format(Sys.time(), "%Y-%d-%m %H%M%S")), ".csv", sep = ""),
  sep = ",",
  row.names = FALSE
)

##### PRINT PROCESSING TIME #####
print(Sys.time() - start_time)