require(RODBC)
require(caret)
require(pROC)
require(doMC)

# VARIABLES
set.seed(8098)
registerDoMC(cores = detectCores())
start_time <- Sys.time()
connection <- odbcDriverConnect("Driver={ODBC Driver 13 for SQL Server};server=localhost;database=MarketAlgo;uid=sa;pwd=LukeSkywalker!")

# GET DATA FROM SQL
data <- sqlQuery(connection, "SELECT * FROM QCHourlyAndDaily")
odbcClose(connection)


#TRANSFORM DATA
data <- subset(data, select = -c(Symbol, Row, Timestamp, Year, Date, Open, High, Low, Close
  , HL2, CandleSize, EMA, DailyHL2, DailyCandleSize, DailyEMA
  , ChangeFuture1, ChangeFuture10, ChangeFuture25, ChangeFuture50, ChangeFuture100, ChangeFuture200))

# data$RSI <- cut(data$RSI, pretty(data$RSI))
# data$SlowRSI <- cut(data$SlowRSI, pretty(data$SlowRSI))
# data$MACD <- cut(data$MACD, pretty(data$MACD))
# data$Change1Period <- cut(data$Change1Period, pretty(data$Change1Period))
# data$Change5Period <- cut(data$Change5Period, pretty(data$Change5Period))
# data$Change10Period <- cut(data$Change10Period, pretty(data$Change10Period))
# data$Change25Period <- cut(data$Change25Period, pretty(data$Change25Period))
# data$Change50Period <- cut(data$Change50Period, pretty(data$Change50Period))
# data$Change100Period <- cut(data$Change100Period, pretty(data$Change100Period))
# data$Change200Period <- cut(data$Change200Period, pretty(data$Change200Period))
# data$ChangeEMA <- cut(data$ChangeEMA, pretty(data$ChangeEMA))
# data$ChangeRSI <- cut(data$ChangeRSI, pretty(data$ChangeRSI))
# data$ChangeSlowRSI <- cut(data$ChangeSlowRSI, pretty(data$ChangeSlowRSI))
# data$ChangeMACD <- cut(data$ChangeMACD, pretty(data$ChangeMACD))
# data$DailyRSI <- cut(data$DailyRSI, pretty(data$DailyRSI))
# data$DailySlowRSI <- cut(data$DailySlowRSI, pretty(data$DailySlowRSI))
# data$DailyMACD <- cut(data$DailyMACD, pretty(data$DailyMACD))
# 
# data$DailyChange1Period <- cut(data$DailyChange1Period, pretty(data$DailyChange1Period))
# data$DailyChange5Period <- cut(data$DailyChange5Period, pretty(data$DailyChange5Period))
# data$DailyChange10Period <- cut(data$DailyChange10Period, pretty(data$DailyChange10Period))
# data$DailyChange25Period <- cut(data$DailyChange25Period, pretty(data$DailyChange25Period))
# data$DailyChange50Period <- cut(data$DailyChange50Period, pretty(data$DailyChange50Period))
# data$DailyChange100Period <- cut(data$DailyChange100Period, pretty(data$DailyChange100Period))
# data$DailyChange200Period <- cut(data$DailyChange200Period, pretty(data$DailyChange200Period))
# data$DailyChangeEMA <- cut(data$DailyChangeEMA, pretty(data$DailyChangeEMA))
# data$DailyChangeRSI <- cut(data$DailyChangeRSI, pretty(data$DailyChangeRSI))
# data$DailyChangeSlowRSI <- cut(data$DailyChangeSlowRSI, pretty(data$DailyChangeSlowRSI))
# data$DailyChangeMACD <- cut(data$DailyChangeMACD, pretty(data$DailyChangeMACD))

dummy <- dummyVars(" ~ .", data = data, fullRank = FALSE)
data <- data.frame(predict(dummy, newdata = data))

data$ChangeFuture5 <- as.factor(ifelse(data$ChangeFuture5 > 0, 'Up', 'Down'))

# MODEL PREDICTION
predictorsNames <- names(data)[names(data) != 'ChangeFuture5']
  
splitIndex <- createDataPartition(data$ChangeFuture5, p = .75, list = FALSE, times = 1)
trainDF <- data[ splitIndex,]
testDF  <- data[-splitIndex,]

objControl <- trainControl(method = 'cv', number = 3, returnResamp = 'none', summaryFunction = twoClassSummary, classProbs = TRUE)
objModel <- train(trainDF[,predictorsNames], trainDF$ChangeFuture5, 
  method='gbm', 
  trControl=objControl,  
  metric = "ROC",
  preProc = c("center", "scale"))

summary(objModel)
print(objModel)

predictions <- predict(object = objModel, testDF[,predictorsNames], type = 'raw')
print(postResample(pred = predictions, obs = as.factor(testDF$ChangeFuture5)))

predictions <- predict(object = objModel, testDF[,predictorsNames], type = 'prob')
auc <- roc(ifelse(testDF$ChangeFuture5 == "Up", 1 , 0), predictions[[2]])
print(auc$auc)

# PRINT PROCESSING TIME
print(Sys.time() - start_time)