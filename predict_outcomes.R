require(RODBC)
require(caret)
require(pROC)
require(doMC)

##### VARIABLES #####
set.seed(8098)
registerDoMC(cores = 3)
start_time <- Sys.time()
outcomeName <- "ChangeFuture5"
file_destination <- "~/marketalgo/predictions/"
resultsRelativeImportance <- data.frame(matrix(vector(), 0, 7, dimnames=list(c(), c("symbol", "outcomeName", "method", "model", "row", "var", "rel.imp"))), stringsAsFactors = FALSE)
resultsModel <- data.frame(matrix(vector(), 0, 6, dimnames=list(c(), c("symbol", "outcomeName", "method", "model", "type", "result"))), stringsAsFactors = FALSE)
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
  data <- sqlQuery(connection, paste("SELECT * FROM QCHourlyAndDaily WHERE Symbol = '", symbol, "'", sep = ""))
  odbcClose(connection)
  
  
  ##### TRANSFORM DATA #####
  data <- subset(data, select = -c(Symbol, Row, Timestamp, Year, Date, Open, High, Low, Close
                                   , HL2, CandleSize, EMA, DailyHL2, DailyCandleSize, DailyEMA
                                   , ChangeFuture1, ChangeFuture10, ChangeFuture25, ChangeFuture50, ChangeFuture100, ChangeFuture200))
  
  data$ClassChange1Period <- as.factor(ifelse(data$ClassChange1Period > 0, 'Up', 'Down'))
  data$ClassChange5Period <- as.factor(ifelse(data$ClassChange5Period > 0, 'Up', 'Down'))
  data$ClassChange10Period <- as.factor(ifelse(data$ClassChange10Period > 0, 'Up', 'Down'))
  data$ClassChange25Period <- as.factor(ifelse(data$ClassChange25Period > 0, 'Up', 'Down'))
  data$ClassChange50Period <- as.factor(ifelse(data$ClassChange50Period > 0, 'Up', 'Down'))
  data$ClassChange100Period <- as.factor(ifelse(data$ClassChange100Period > 0, 'Up', 'Down'))
  data$ClassChange200Period <- as.factor(ifelse(data$ClassChange200Period > 0, 'Up', 'Down'))
  data$ClassChangeEMA <- as.factor(ifelse(data$ClassChangeEMA > 0, 'Up', 'Down'))
  data$ClassChangeRSI <- as.factor(ifelse(data$ClassChangeRSI > 0, 'Up', 'Down'))
  data$ClassChangeSlowRSI <- as.factor(ifelse(data$ClassChangeSlowRSI > 0, 'Up', 'Down'))
  data$ClassChangeMACD <- as.factor(ifelse(data$ClassChangeMACD > 0, 'Up', 'Down'))
  data$DailyClassChange1Period <- as.factor(ifelse(data$DailyClassChange1Period > 0, 'Up', 'Down'))
  data$DailyClassChange5Period <- as.factor(ifelse(data$DailyClassChange5Period > 0, 'Up', 'Down'))
  data$DailyClassChange10Period <- as.factor(ifelse(data$DailyClassChange10Period > 0, 'Up', 'Down'))
  data$DailyClassChange25Period <- as.factor(ifelse(data$DailyClassChange25Period > 0, 'Up', 'Down'))
  data$DailyClassChange50Period <- as.factor(ifelse(data$DailyClassChange50Period > 0, 'Up', 'Down'))
  data$DailyClassChange100Period <- as.factor(ifelse(data$DailyClassChange100Period > 0, 'Up', 'Down'))
  data$DailyClassChange200Period <- as.factor(ifelse(data$DailyClassChange200Period > 0, 'Up', 'Down'))
  data$DailyClassChangeEMA <- as.factor(ifelse(data$DailyClassChangeEMA > 0, 'Up', 'Down'))
  data$DailyClassChangeRSI <- as.factor(ifelse(data$DailyClassChangeRSI > 0, 'Up', 'Down'))
  data$DailyClassChangeSlowRSI <- as.factor(ifelse(data$DailyClassChangeSlowRSI > 0, 'Up', 'Down'))
  data$DailyClassChangeMACD <- as.factor(ifelse(data$DailyClassChangeMACD > 0, 'Up', 'Down'))
  
  dummy <- dummyVars(" ~ .", data = data, fullRank = FALSE)
  data <- data.frame(predict(dummy, newdata = data))
  
  ##### MODEL PREDICTION - GLMNET #####
  # method <- "glmnet"
  # 
  # data[[outcomeName]] <- ifelse(data[[outcomeName]] > 0, 1, 0)
  # 
  # predictorsNames <- names(data)[names(data) != outcomeName]
  # 
  # splitIndex <- createDataPartition(data[[outcomeName]], p = .75, list = FALSE, times = 1)
  # trainDF <- data[ splitIndex,]
  # testDF  <- data[-splitIndex,]
  # 
  # objControl <- trainControl(method = 'cv', number = 3, returnResamp = 'none')
  # objModel <- train(trainDF[,predictorsNames], trainDF[[outcomeName]], 
  #                   method = method,  
  #                   metric = "RMSE", 
  #                   trControl = objControl)
  # 
  # predictionsGlmnet <- predict(object = objModel, testDF[,predictorsNames])
  # 
  # auc <- roc(testDF[[outcomeName]], predictionsGlmnet)
  # 
  # ##### AGGREGATE RESULTS #####
  # var <- varImp(objModel, scale = FALSE)$importance
  # var$var <- rownames(var)
  # var <- var[order(-var$Overall),]
  # 
  # resultsRelativeImportance <- rbind(resultsRelativeImportance, data.frame(symbol = symbol, outcomeName = outcomeName, method = objModel$method, model = objModel$modelType
  #                                                  , row = seq(from = 1, to = length(var$var), by = 1), var = var$var, rel.imp = var$Overall))
  # 
  # resultsModel <- rbind(resultsModel, data.frame(symbol = symbol, outcomeName = outcomeName, method = objModel$method, model = objModel$modelType, type = "accuracy"
  #                                                , result = postResample(pred = predictionsGlmnet, obs = testDF[[outcomeName]])[1], row.names = NULL))
  # resultsModel <- rbind(resultsModel, data.frame(symbol = symbol, outcomeName = outcomeName, method = objModel$method, model = objModel$modelType
  #                                                , type = "auc", result = as.numeric(auc$auc)))
  
  ##### MODEL PREDICTION - GBM #####
  method <- "gbm"
  
  data[[outcomeName]] <- as.factor(ifelse(data[[outcomeName]] > 0, 'Up', 'Down'))
  
  predictorsNames <- names(data)[names(data) != outcomeName]
  
  splitIndex <- createDataPartition(data[[outcomeName]], p = .75, list = FALSE, times = 1)
  trainDF <- data[ splitIndex,]
  testDF  <- data[-splitIndex,]
  
  objControl <- trainControl(method = 'cv', number = 3, returnResamp = 'none', summaryFunction = twoClassSummary, classProbs = TRUE)
  objModel <- train(trainDF[,predictorsNames], trainDF[[outcomeName]],
                    method = method,
                    trControl = objControl,
                    metric = "ROC",
                    preProc = c("center", "scale"))
  
  predictionsGbmRaw <- predict(object = objModel, testDF[,predictorsNames], type = 'raw')
  predictionsGbmProb <- predict(object = objModel, testDF[,predictorsNames], type = 'prob')
  
  auc <- roc(ifelse(testDF[[outcomeName]] == "Up", 1 , 0), predictionsGbmProb[[2]])
  
  ##### AGGREGATE RESULTS #####
  var <- varImp(objModel, scale = FALSE)$importance
  var$var <- rownames(var)
  var <- var[order(-var$Overall),]
  
  resultsRelativeImportance <- rbind(resultsRelativeImportance, data.frame(symbol = symbol, outcomeName = outcomeName, method = objModel$method, model = objModel$modelType
                                                   , row = seq(from = 1, to = length(var$var), by = 1), var = var$var, rel.imp = var$Overall))
  
  resultsModel <- rbind(resultsModel, data.frame(symbol = symbol, outcomeName = outcomeName, method = objModel$method, model = objModel$modelType, type = "accuracy"
                                                 , result = postResample(pred = predictionsGbmRaw, obs = as.factor(testDF[[outcomeName]]))[1], row.names = NULL))
  resultsModel <- rbind(resultsModel, data.frame(symbol = symbol, outcomeName = outcomeName, method = objModel$method, model = objModel$modelType
                                                 , type = "auc", result = as.numeric(auc$auc)))
}

##### WRITE RESULTS TO CSV #####
write.table(
  resultsRelativeImportance,
  file = paste(file_destination, "resultsRelativeImportance ", as.character(format(Sys.time(), "%Y-%d-%m %H%M%S")), ".csv", sep = ""),
  sep = ",",
  row.names = FALSE
)

write.table(
  resultsModel,
  file = paste(file_destination, "resultsModel ", as.character(format(Sys.time(), "%Y-%d-%m %H%M%S")), ".csv", sep = ""),
  sep = ",",
  row.names = FALSE
)

##### PRINT PROCESSING TIME #####
print(Sys.time() - start_time)