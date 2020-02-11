# Hypothesis 2 - Supervised Time Series Analysis
#
# 25th November 2019
#
# UPDATE
# 1.00      25/11/2019  Initial Version - Aiden
# 1.10    
# ************************************************
# ************************************************
# clears all objects in "global environment"

#*************************************************
# Global variables

FILENAME          <- "Accidents0515.csv"    # Raw Accidents Dataset
SPLIT_TRAIN       <- 2903                   #Number of days to split train and test for DL (default train is 2913 days = 2005-2012)
BASICNN_EPOCHS    <- 150                    #Number of Epochs
DEEP_HIDDEN       <- c(30,2)                #Number of neurons in each layer
DEEP_STOPPING     <- 30                     #Number of times no improvement before stop
DEEP_TOLERANCE    <- 1e-4                   #Error threshold
DEEP_ACTIVATION   <- "Tanh"                 #Non-linear activation function
DEEP_REPRODUCABLE <- TRUE                   #Set to TRUE to test training is same for each run

TIME_SLOTS        <-   20                   #Number of back-slots (days) to use as windowed NN input

RNN_SLOTS         <-  365                   #Number of days to use to train the RNN
RNN_NEURONS       <-   25                   #Number of hidden neurons
RNN_EPOCHS        <- 5000                   #Number of training cycles


MYLIBRARIES<-c("quantmod",
               "timeSeries",
               "formattable",
               "rnn",
               "h2o",
               "plyr",
               "ggplot2")

# ************************************************
# main() :
# main entry point to execute ML data analytics
#
# INPUT: None
# OUTPUT :None
# ************************************************
time_series<-function(){
  
  # Read CSV File
  accidents <- read_csv(csvFilename = FILENAME)
  
  # Summarise number of Accidents by day 
  accidents$Date <- as.character(accidents$Date)
  
  Accidents_TS <- plyr::count(accidents$Date)
  Accidents_TS <- Accidents_TS[order(as.Date(Accidents_TS$x, format="%d/%m/%Y")),]
  row.names(Accidents_TS) <- Accidents_TS[,1]
  Accidents_TS$x <- NULL
  
  
  #Plot time series of accidents
  print("Time Series of the Number of Accidents 2005-2015")
  
  NumberOfAcc <- Accidents_TS$freq
  
  plot(as.timeSeries(Accidents_TS$freq), 
       at="chic", plot.type="s", cex.axis=0.75, col="aquamarine4",
       xlab="Date", ylab="Number of Accidents")
  title(main = "Time Series of Number of Accidents 2005-2015")
  
 
  
  # ************************************************
  # Visualise the dataset
  # Seasonal: patterns that repeat with fixed period of time.
  # Trend: the underlying trend of the metrics.
  # Random: (also call "noise", "Irregular" or "Remainder") Is the residuals of the
  #time series.
  print("Time Series trend, seasonality and residuals")
  
  tsAcc = ts(Accidents_TS$freq, frequency = 365) 
  decomposeNumAcc= decompose(tsAcc, "additive")
  plot(decomposeNumAcc, col="aquamarine4")
  
  print("Begin Data Processing for Deep Learning")
  
  # Preprocess Data for Time Series Analysis
  dataset <- Hypothesis_2_Preprocess_DeepLearning(TimeSeries = Accidents_TS)
  
  
  print("End of pre-processing")
  
  ### End of Pre-processing
  
  # ************************************************
  # Using a deep NN
  
  N_DEEP_Initialise()
  
  # ************************************************
  # Train a deep neual network 
  
  # The  field we are predicting 
  outputFieldName=paste("V",TIME_SLOTS,sep="")
  
  # Remove the $date field, as we don't want this as an input!
  train<-subset(dataset$train,select=-date)
  test<-subset(dataset$test,select=-date)
  
  deep<-N_DEEP_Train(train=train,
                     fieldNameOutput=outputFieldName,
                     hidden=DEEP_HIDDEN,
                     stopping_rounds=DEEP_STOPPING,
                     stopping_tolerance=DEEP_TOLERANCE,
                     activation=DEEP_ACTIVATION,
                     reproducible=DEEP_REPRODUCABLE,
                     regression=TRUE)
  
  summary(deep)
  plot(deep)  # plots the scoring history
  
  # ************************************************
  predictions<-N_EVALUATE_DeepNeural(test=test,
                                     fieldNameOutput=outputFieldName,
                                     deep=deep,
                                     regression=TRUE)
  
  # ************************************************
  # Visualise the actual .v. predicted prices as a timeseries
  
  expected<-test[,outputFieldName]
  
  real_exp_pred<-N_visualiseTimeSeriesResults(expected=expected,
                                              predicted=predictions,
                                              means=dataset$testMeans[,outputFieldName],
                                              dates=dataset$test$date,
                                              realmin=dataset$min,
                                              realmax=dataset$max,
                                              title="Deep NN Windowed Test Dataset",
                                              yaxis_title=c("Expected","Predicted"),
                                              measuresFLag=TRUE)
  
  
  
  # "Vanilla" Recurrent Neural Network time series forecasting
  Num_accidents_byDate_VanillaRNN <- plyr::count(accidents$Date)
  Num_accidents_byDate_VanillaRNN <- Num_accidents_byDate_VanillaRNN[order(as.Date(Num_accidents_byDate_VanillaRNN$x, format="%d/%m/%Y")),]
  row.names(Num_accidents_byDate_VanillaRNN) <- Num_accidents_byDate_VanillaRNN[,1]
  Num_accidents_byDate_VanillaRNN$x <- NULL
  
  # Run the RNN
  predictedRNN<-N_timeSeriesRNNPredict(openStockPrice = Num_accidents_byDate_VanillaRNN) 
  
  
} #end of main()

gc() #release memory

# clear plots and other graphics
if(!is.null(dev.list())) dev.off()
graphics.off()

# This clears all warning messages
assign("last.warning", NULL, envir = baseenv())

# clears the console area
cat("\014")

print("STARTING Hypothesis 2 - Time Series Analysis for Number of Road Accidents")
start<-Sys.time()

library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

#Load additional R script files
source("Functions_Dataholics.R") # Main functions file for all Hypotheses

set.seed(123)

time_series()

end<-Sys.time()
print("Time Analysis was Done in:")
print(end - start)

readline(prompt = "Press [enter] to continue. Warning, that will delete the graphs of this section")

if(!is.null(dev.list())) dev.off()
graphics.off()

# This clears all warning messages
assign("last.warning", NULL, envir = baseenv())

# clears the console area
cat("\014")

print("STARTING Hypothesis 2 - Time Series Analysis for Number of Road Accidents")
start<-Sys.time()

library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

#Load additional R script files
source("Functions_Dataholics.R") # Main functions file for all Hypotheses


end<-Sys.time()
print("Time Analysis was Done in:")
print(end - start)

readline(prompt = "Press [enter] to continue. Warning, that will delete the graphs of this section")
graphics.off()







