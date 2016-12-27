require(TTR)
require(quantmod)
require(DataCombine)
require(dplyr)

##### VARIABLES #####
start_time <- Sys.time()
directories <- c("~/marketalgo/data_qc_hourly/", "~/marketalgo/data_qc_daily/")


##### LOOP THROUGH DIRECTORIES #####
for (i in 1:length(directories)) {
  current_directory = directories[i]
  print(current_directory)
  
  ##### GET TA COLUMNS AND OVERWRITE #####
  files <- list.files(current_directory, pattern = "csv", full.names = TRUE)
  for (i in 1:length(files)) {
    current_file <- files[i]
    print(current_file)
    
    new_filename <- paste(current_directory, "new_", basename(current_file), sep = "")
    print(new_filename)
    
    data = read.csv(file = current_file)
    
    data$Symbol <- substr(basename(current_file), 1, nchar(basename(current_file)) - 4)
    data$Timestamp <- as.POSIXct(data$Timestamp, format="%Y%m%d %H:%M")
    data$Year <- strftime(data$Timestamp, "%Y")
    data$Month <- strftime(data$Timestamp, "%b")
    data$Day <- strftime(data$Timestamp, "%a")
    data$Date <- strftime(data$Timestamp, "%Y-%m-%d")
    data$Time <- strftime(data$Timestamp, "%H:%M")
    data$HL2 <- (data$High + data$Low) / 2
    data$CandleSize <- (data$High - data$Low) / data$Close
    
    EMA20 <- EMA(data[,('Close')], 20)
    EMA100 <- EMA(data[,('Close')], 100)
    EMA20PricePosition <- ifelse(data$Close > EMA20, 'Above', 'Below')
    EMA100PricePosition <- ifelse(data$Close > EMA100, 'Above', 'Below')
    EMAPositions <- ifelse(EMA20 > EMA100, 'Bull', 'Bear')
    RSI5 <- RSI(data[,('Close')], 5)
    RSI14 <- RSI(data[,('Close')], 14)
    SlowRSI5 <- EMA(RSI(data[,('Close')], 5), 10)
    SlowRSI14 <- EMA(RSI(data[,('Close')], 14), 10)
    MACD <- MACD(data[,('Close')], 12, 26, 9, percent = FALSE)
    colnames(MACD) <- c('MACD', 'MACDSignal')
    BB <- as.data.frame(BBands(data[,('Close')], n = 20, sd = 2))
    BB$BBWidth <- BB$up - BB$dn
    BB$BBPricePosition <- ifelse(data$Close > BB$up, 'Above', ifelse(data$Close < BB$dn, 'Below', 'Inside'))
    BB <- subset(BB, select = -c(dn, mavg, up))
    colnames(BB) <- c('BBPct', 'BBWidth','BBPricePosition')
    Stoch <- stoch(data[,c('High', 'Low', 'Close')], nFastK = 14, nFastD = 3, nSlowD = 3)
    colnames(Stoch) <- c('StochFastK', 'StochFastD','StochSlowD')
    
    data = data.frame(data, EMA20, EMA100, EMA20PricePosition, EMA100PricePosition, EMAPositions, RSI5, RSI14, SlowRSI5, SlowRSI14, MACD, BB, Stoch)
    
    Change1Period <- Delt(data$Close)
    Change5Period <- Delt(data$Close, k = 5)
    Change10Period <- Delt(data$Close, k = 10)
    Change15Period <- Delt(data$Close, k = 15)
    Change20Period <- Delt(data$Close, k = 20)
    Change50Period <- Delt(data$Close, k = 50)
    Change100Period <- Delt(data$Close, k = 100)
    Change200Period <- Delt(data$Close, k = 200)
    colnames(Change1Period) <- c('Change1Period')
    colnames(Change5Period) <- c('Change5Period')
    colnames(Change10Period) <- c('Change10Period')
    colnames(Change15Period) <- c('Change15Period')
    colnames(Change20Period) <- c('Change20Period')
    colnames(Change50Period) <- c('Change50Period')
    colnames(Change100Period) <- c('Change100Period')
    colnames(Change200Period) <- c('Change200Period')
    
    ChangeEMA20 <- ifelse(Delt(data$EMA20) == "Inf", 0, ifelse(Delt(data$EMA20) > 1000, 1000, ifelse(Delt(data$EMA20) < -1000, -1000, Delt(data$EMA20))))
    ChangeEMA100 <- ifelse(Delt(data$EMA100) == "Inf", 0, ifelse(Delt(data$EMA100) > 1000, 1000, ifelse(Delt(data$EMA100) < -1000, -1000, Delt(data$EMA100))))
    ChangeRSI5 <- ifelse(Delt(data$RSI5) == "Inf", 0, ifelse(Delt(data$RSI5) > 1000, 1000, ifelse(Delt(data$RSI5) < -1000, -1000, Delt(data$RSI5))))
    ChangeRSI14 <- ifelse(Delt(data$RSI14) == "Inf", 0, ifelse(Delt(data$RSI14) > 1000, 1000, ifelse(Delt(data$RSI14) < -1000, -1000, Delt(data$RSI14))))
    ChangeSlowRSI5 <- ifelse(Delt(data$SlowRSI5) == "Inf", 0, ifelse(Delt(data$SlowRSI5) > 1000, 1000, ifelse(Delt(data$SlowRSI5) < -1000, -1000, Delt(data$SlowRSI5))))
    ChangeSlowRSI14 <- ifelse(Delt(data$SlowRSI14) == "Inf", 0, ifelse(Delt(data$SlowRSI14) > 1000, 1000, ifelse(Delt(data$SlowRSI14) < -1000, -1000, Delt(data$SlowRSI14))))
    ChangeMACD <- ifelse(Delt(data$MACD) == "Inf", 0, ifelse(Delt(data$MACD) > 1000, 1000, ifelse(Delt(data$MACD) < -1000, -1000, Delt(data$MACD))))
    ChangeMACDSignal <- ifelse(Delt(data$MACDSignal) == "Inf", 0, ifelse(Delt(data$MACDSignal) > 1000, 1000, ifelse(Delt(data$MACDSignal) < -1000, -1000, Delt(data$MACDSignal))))
    ChangeBBPct <- ifelse(Delt(data$BBPct) == "Inf", 0, ifelse(Delt(data$BBPct) > 1000, 1000, ifelse(Delt(data$BBPct) < -1000, -1000, Delt(data$BBPct))))
    ChangeBBWidth <- ifelse(Delt(data$BBWidth) == "Inf", 0, ifelse(Delt(data$BBWidth) > 1000, 1000, ifelse(Delt(data$BBWidth) < -1000, -1000, Delt(data$BBWidth))))
    ChangeStochFastK <- ifelse(Delt(data$StochFastK) == "Inf", 0, ifelse(Delt(data$StochFastK) > 1000, 1000, ifelse(Delt(data$StochFastK) < -1000, -1000, Delt(data$StochFastK))))
    ChangeStochFastD <- ifelse(Delt(data$StochFastD) == "Inf", 0, ifelse(Delt(data$StochFastD) > 1000, 1000, ifelse(Delt(data$StochFastD) < -1000, -1000, Delt(data$StochFastD))))
    ChangeStochSlowD <- ifelse(Delt(data$StochSlowD) == "Inf", 0, ifelse(Delt(data$StochSlowD) > 1000, 1000, ifelse(Delt(data$StochSlowD) < -1000, -1000, Delt(data$StochSlowD))))
    
    colnames(ChangeEMA20) <- c('ChangeEMA20')
    colnames(ChangeEMA100) <- c('ChangeEMA100')
    colnames(ChangeRSI5) <- c('ChangeRSI5')
    colnames(ChangeRSI14) <- c('ChangeRSI14')
    colnames(ChangeSlowRSI5) <- c('ChangeSlowRSI5')
    colnames(ChangeSlowRSI14) <- c('ChangeSlowRSI14')
    colnames(ChangeMACD) <- c('ChangeMACD')
    colnames(ChangeMACDSignal) <- c('ChangeMACDSignal')
    colnames(ChangeBBPct) <- c('ChangeBBPct')
    colnames(ChangeBBWidth) <- c('ChangeBBWidth')
    colnames(ChangeStochFastK) <- c('ChangeStochFastK')
    colnames(ChangeStochFastD) <- c('ChangeStochFastD')
    colnames(ChangeStochSlowD) <- c('ChangeStochSlowD')
    
    data = data.frame(data, Change1Period, Change5Period, Change10Period, Change15Period, Change20Period, Change50Period, Change100Period, Change200Period
                      , ChangeEMA20, ChangeEMA100, ChangeRSI5, ChangeRSI14, ChangeSlowRSI5, ChangeSlowRSI14, ChangeMACD, ChangeMACDSignal
                      , ChangeBBPct, ChangeBBWidth, ChangeStochFastK, ChangeStochFastD, ChangeStochSlowD)
    
    data$ConsecChange1Period <- sequence(rle(ifelse(data$Change1Period > 0, 1, 0))$lengths)
    data$ConsecChangeEMA20 <- sequence(rle(ifelse(data$ChangeEMA20 > 0, 1, 0))$lengths)
    data$ConsecChangeEMA100 <- sequence(rle(ifelse(data$ChangeEMA100 > 0, 1, 0))$lengths)
    data$ConsecChangeRSI5 <- sequence(rle(ifelse(data$ChangeRSI5 > 0, 1, 0))$lengths)
    data$ConsecChangeRSI14 <- sequence(rle(ifelse(data$ChangeRSI14 > 0, 1, 0))$lengths)
    data$ConsecChangeSlowRSI5 <- sequence(rle(ifelse(data$ChangeSlowRSI5 > 0, 1, 0))$lengths)
    data$ConsecChangeSlowRSI14 <- sequence(rle(ifelse(data$ChangeSlowRSI14 > 0, 1, 0))$lengths)
    data$ConsecChangeMACD <- sequence(rle(ifelse(data$ChangeMACD > 0, 1, 0))$lengths)
    data$ConsecChangeMACDSignal <- sequence(rle(ifelse(data$ChangeMACDSignal > 0, 1, 0))$lengths)
    data$ConsecChangeBBPct <- sequence(rle(ifelse(data$ChangeBBPct > 0, 1, 0))$lengths)
    data$ConsecChangeBBWidth <- sequence(rle(ifelse(data$ChangeBBWidth > 0, 1, 0))$lengths)
    data$ConsecChangeStochFastK <- sequence(rle(ifelse(data$ChangeStochFastK > 0, 1, 0))$lengths)
    data$ConsecChangeStochFastD <- sequence(rle(ifelse(data$ChangeStochFastD > 0, 1, 0))$lengths)
    data$ConsecChangeStochSlowD <- sequence(rle(ifelse(data$ChangeStochSlowD > 0, 1, 0))$lengths)
    
    data <- slide(data, Var = 'Change1Period', NewVar = 'ChangeFuture1', slideBy = 1, reminder = FALSE)
    data <- slide(data, Var = 'Change5Period', NewVar = 'ChangeFuture5', slideBy = 5, reminder = FALSE)
    data <- slide(data, Var = 'Change10Period', NewVar = 'ChangeFuture10', slideBy = 10, reminder = FALSE)
    data <- slide(data, Var = 'Change15Period', NewVar = 'ChangeFuture15', slideBy = 15, reminder = FALSE)
    data <- slide(data, Var = 'Change20Period', NewVar = 'ChangeFuture20', slideBy = 20, reminder = FALSE)
    data <- slide(data, Var = 'Change50Period', NewVar = 'ChangeFuture50', slideBy = 50, reminder = FALSE)
    data <- slide(data, Var = 'Change100Period', NewVar = 'ChangeFuture100', slideBy = 100, reminder = FALSE)
    data <- slide(data, Var = 'Change200Period', NewVar = 'ChangeFuture200', slideBy = 200, reminder = FALSE)
    
    data <- data[c('Symbol', 'Row', 'Timestamp', 'Year', 'Month', 'Day', 'Date', 'Time', 'Open', 'High', 'Low', 'Close', 'HL2', 'CandleSize'
                   , 'EMA20', 'EMA100', 'EMA20PricePosition', 'EMA100PricePosition', 'EMAPositions', 'RSI5', 'RSI14', 'SlowRSI5', 'SlowRSI14', 'MACD', 'MACDSignal'
                   , 'BBPct', 'BBWidth', 'BBPricePosition', 'StochFastK', 'StochFastD', 'StochSlowD'
                   , 'Change1Period', 'Change5Period', 'Change10Period', 'Change15Period', 'Change20Period', 'Change50Period', 'Change100Period', 'Change200Period'
                   , 'ChangeEMA20', 'ChangeEMA100', 'ChangeRSI5', 'ChangeRSI14', 'ChangeSlowRSI5', 'ChangeSlowRSI14', 'ChangeMACD', 'ChangeMACDSignal'
                   , 'ChangeBBPct', 'ChangeBBWidth', 'ChangeStochFastK', 'ChangeStochFastD', 'ChangeStochSlowD'
                   , 'ConsecChange1Period', 'ConsecChangeEMA20', 'ConsecChangeEMA100', 'ConsecChangeRSI5', 'ConsecChangeRSI14', 'ConsecChangeSlowRSI5', 'ConsecChangeSlowRSI14', 'ConsecChangeMACD', 'ConsecChangeMACDSignal'
                   , 'ConsecChangeBBPct', 'ConsecChangeBBWidth', 'ConsecChangeStochFastK', 'ConsecChangeStochFastD', 'ConsecChangeStochSlowD'
                   , 'ChangeFuture1', 'ChangeFuture5', 'ChangeFuture10', 'ChangeFuture15', 'ChangeFuture20', 'ChangeFuture50', 'ChangeFuture100', 'ChangeFuture200')]

    write.table(
      data,
      file = new_filename,
      # na = "",
      sep = ",",
      row.names = FALSE
    )
    
    file.remove(current_file)
    file.rename(new_filename, current_file)
  }
}

##### PRINT PROCESSING TIME #####
print(Sys.time() - start_time)