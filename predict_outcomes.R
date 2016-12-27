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
outcomeName <- "ChangeFuture5"
file_destination <- "~/marketalgo/predictions/"
train_split <- 0.75
resultsRelativeImportance <- data.frame(matrix(vector(), 0, 8, dimnames=list(c(), c("symbol", "outcomeName", "method", "model", "row", "var", "rel.imp", "count"))), stringsAsFactors = FALSE)
resultsModel <- data.frame(matrix(vector(), 0, 7, dimnames=list(c(), c("symbol", "outcomeName", "method", "model", "type", "result", "count"))), stringsAsFactors = FALSE)
resultsTestPredictions <- data.frame(matrix(vector(), 0, 11, dimnames=list(c(), c("symbol", "outcomeName", "method", "model", "row", "result", "prediction", "prob.down", "prob.up"
                                                                                  , "correct", "count"))), stringsAsFactors = FALSE)
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
  data <- subset(rawSql, select = -c(Symbol, Timestamp, Year, Month, Day, Date, Time, Open, High, Low, Close, HL2, CandleSize, EMA20, EMA100, Change50Period, Change100Period, Change200Period
                                   , DailyEMA20, DailyEMA100, DailyEMA20PricePosition, DailyEMA100PricePosition
                                   , DailyChange5Period, DailyChange10Period, DailyChange15Period, DailyChange20Period, DailyChange50Period, DailyChange100Period, DailyChange200Period, DailyChangeEMA20, DailyChangeEMA100
                                   , DailyConsecChange1Period, DailyConsecChangeEMA20, DailyConsecChangeEMA100, DailyConsecChangeRSI5, DailyConsecChangeRSI14, DailyConsecChangeSlowRSI5
                                   , DailyConsecChangeSlowRSI14, DailyConsecChangeMACD, DailyConsecChangeMACDSignal, DailyConsecChangeBBPct, DailyConsecChangeBBWidth
                                   , DailyConsecChangeStochFastK, DailyConsecChangeStochFastD, DailyConsecChangeStochSlowD
                                   , ChangeFuture1, ChangeFuture10, ChangeFuture15, ChangeFuture20, ChangeFuture50, ChangeFuture100, ChangeFuture200))
  
  dummy <- dummyVars(" ~ .", data = data, fullRank = TRUE)
  data <- data.frame(predict(dummy, newdata = data))
  data <- data[order(data$Row),]
  
  ##### MODEL PREDICTION - GBM #####
  method <- "gbm"
  
  data[[outcomeName]] <- as.factor(ifelse(data[[outcomeName]] > 0, 'Up', 'Down'))
  
  predictorsNames <- names(data)[names(data) != outcomeName & names(data) != 'Row']
  
  trainData <- subset(data, Row < quantile(Row, train_split))
  testData  <- subset(data, Row >= quantile(Row, train_split))
  # modelData <- subset(data, Row < quantile(Row, train_split))
  # finalTestData  <- subset(data, Row >= quantile(Row, train_split))
  # splitIndex <- createDataPartition(modelData[[outcomeName]], p = .75, list = FALSE, times = 1)
  # trainData <- modelData[ splitIndex,]
  # testData  <- modelData[-splitIndex,]
  
  objControl <- trainControl(method = 'cv', number = 3, returnResamp = 'none', summaryFunction = twoClassSummary, classProbs = TRUE)
  objModel <- train(trainData[,predictorsNames], trainData[[outcomeName]],
                    method = method,
                    trControl = objControl,
                    metric = "ROC",
                    preProc = c("center", "scale"))
  
  predictionsRaw <- predict(object = objModel, testData[,predictorsNames], type = 'raw')
  predictionsProb <- predict(object = objModel, testData[,predictorsNames], type = 'prob')
  # predictionsRaw <- predict(object = objModel, finalTestData[,predictorsNames], type = 'raw')
  # predictionsProb <- predict(object = objModel, finalTestData[,predictorsNames], type = 'prob')
  
  auc <- roc(ifelse(testData[[outcomeName]] == "Up", 1 , 0), predictionsProb[[2]])
  # auc <- roc(ifelse(finalTestData[[outcomeName]] == "Up", 1 , 0), predictionsProb[[2]])
  
  ##### AGGREGATE RESULTS #####
  var <- varImp(objModel, scale = FALSE)$importance
  var$var <- rownames(var)
  var <- var[order(-var$Overall),]
  
  resultsRelativeImportance <- rbind(resultsRelativeImportance, data.frame(symbol = symbol, outcomeName = outcomeName, method = objModel$method, model = objModel$modelType
                                                   , row = seq(from = 1, to = length(var$var), by = 1), var = var$var, rel.imp = var$Overall, count = 1))
  
  resultsModel <- rbind(resultsModel, data.frame(symbol = symbol, outcomeName = outcomeName, method = objModel$method, model = objModel$modelType, type = "accuracy"
                                                 , result = postResample(pred = predictionsRaw, obs = as.factor(testData[[outcomeName]]))[1], row.names = NULL, count = 1))
  resultsModel <- rbind(resultsModel, data.frame(symbol = symbol, outcomeName = outcomeName, method = objModel$method, model = objModel$modelType
                                                 , type = "auc", result = as.numeric(auc$auc), count = 1))
  
  tempProbs <- data.frame(testData$Row, testData[[outcomeName]], predictionsRaw, predictionsProb)
  colnames(tempProbs) <- c('row', 'result', 'prediction', 'prob.down', 'prob.up')
  resultsTestPredictions <- rbind(resultsTestPredictions, data.frame(symbol = symbol, outcomeName = outcomeName, method = objModel$method, model = objModel$modelType
                                                 , row = tempProbs$row, result = tempProbs$result, prediction = tempProbs$prediction, prob.down = tempProbs$prob.down
                                                 , prob.up = tempProbs$prob.up, correct = ifelse(tempProbs$result == tempProbs$prediction, 1 , 0), count = 1))
  
  stop("STOP")
}

##### WRITE RESULTS TO CSV #####
write.table(
  resultsRelativeImportance,
  file = paste(file_destination, "resultsRelativeImportance-", outcomeName, " ", as.character(format(Sys.time(), "%Y-%d-%m %H%M%S")), ".csv", sep = ""),
  sep = ",",
  row.names = FALSE
)

write.table(
  resultsModel,
  file = paste(file_destination, "resultsModel-", outcomeName, " ", as.character(format(Sys.time(), "%Y-%d-%m %H%M%S")), ".csv", sep = ""),
  sep = ",",
  row.names = FALSE
)

write.table(
  resultsTestPredictions,
  file = paste(file_destination, "resultsTestPredictions-", outcomeName, " ", as.character(format(Sys.time(), "%Y-%d-%m %H%M%S")), ".csv", sep = ""),
  sep = ",",
  row.names = FALSE
)

##### PRINT PROCESSING TIME #####
print(Sys.time() - start_time)