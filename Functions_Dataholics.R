# **************************************************************
# 
# 2019 PRATICAL BUSINESS ANALYTICS COURSEWORK - UK ROAD ACCIDENTS
#
# Functions
#
# TEAM: DATAHOLICS
# 
# 20th November 2019
#
# UPDATE
# 1.00      20/11/2019    Initial Version - Aiden, Lux, Hannes
# 1.1       26/11/2019    Aiden - added preprocessing for Hypothesis 3
# 1.2       
# *************************************************************

# *************************************************************************
# *************************************************************************
# Self Written Functions

# *************************************************************
# read_csv() :
#
# Read csv file
#
# INPUT:  csv file from working directory
#
# OUTPUT: data frame - contents of the headed CSV file
# *************************************************************
read_csv<-function(csvFilename){
  
  dataset <- read.csv(csvFilename,encoding="UTF-8",stringsAsFactors = FALSE)
  print(paste("CSV dataset",csvFilename,"has been read. Records=",nrow(dataset)))
  
  return(dataset)
} #end of read_csv

# ************************************************************
# drop_and_format_date() :
#
# Clean any of the dataset
#
# INPUT:   data frame - the original data
#          column_names - the names of the column that should be dropped
#
# OUTPUT : data frame - dataset preprocessed without columns in column names and 
#                       correctly into date format transformed date column if dataframe contains one
# ***********************************************************
drop_and_format_date <- function(dataset, column_names){
  
  #Remove columns not selected for modelling
  dataset <- dataset[,!(names(dataset) %in% column_names)]
  
  #Format Dates 
  if ("Date" %in% names(dataset)){
    dataset$Date <- as.Date(dataset$Date, format = "%d/%m/%Y" )
  }
  
  return(dataset)
  
} #end of preprocess_accidents

# ***********************************************************
# drop_minus_one_nulls() :
#
# Drop every row that contains a -1
#
# INPUT:    data_frame - the original data
#           cols - list of columns in which -1 should be removed
#
# OUTPUT:   data-frame - the original data without the rows that contain -1
#
# **********************************************************
drop_minus_one_nulls <- function(dataset, cols){
  
  #Drop rows that contain -1
  for (name in cols){
    dataset <- dataset[!dataset[name] ==-1,]
  }
  
  return(dataset)
} #end of drop_nulls
# ***********************************************************
# drop_NA_nulls() :
#
# Drop every row that contains a -NA
#
# INPUT:    data_frame - the original data
#           cols - list of columns in which NA should be removed
#
# OUTPUT:   data-frame - the original data without the rows that contain NA
#
# **********************************************************
drop_NA_nulls <- function(dataset, cols){
  
  #Drop rows that contain NA
  for (name in cols){
    dataset <- dataset[is.na(dataset[name]) == FALSE,]
  }
  
  return(dataset)
} #end of drop_NA_nulls
# *************************************************************
# print_time_development() :
# print the timedevelopment of the 'parameter' with name 'name' and with 'value' in 'dataset
# 
# INPUT: dataset - the dataset that contains parameter
#        parameter - the parameter which timedeleopment should be printed
#        name - name of the parameter
#        value - the value of the dataset for which a timeanalysis should be printed
#
# OUTPUT: plots the graph of the timeanalysis of the parameter in dataset with name and value
print_time_development <- function(dataset, parameter, name, values){
  
  viewer<-aggregate(Accident_Index ~ parameter, data = dataset, FUN = function(x){NROW(x)})
  View(viewer)
  
  for (value in values){
    agg<-subset(dataset, parameter == value)
    agg<-aggregate(Accident_Index ~ Date, data = agg, FUN = function(x){NROW(x)})
    agg<-agg[order(agg$Date),]
    barplot(agg$Accident_Index,
            main = paste("Number of Accidents per Date where", name, "is", value, sep = " ", collapse = NULL),
            xlab = "Date",
            ylab = "No. of Accidents",
            names.arg = agg$Date,
            col = "darkred",
            horiz = FALSE)
  }
  
}
# ***********************************************************
# plot_parameters() :
#
# Print bar graph of alues for every parameter in a dataframe, sorted
# from high to low number of Accident_Index' for this parameter value
#
# INPUT:    data_frame - data frame, where first parameter
#                        is Accident_Index
#
# OUTPUT:   barplot of every parameter in the dataframe
#
# ***********************************************************
plot_parameters <- function(data_frame, main = ""){
  if (ncol(data_frame) == 2){
    agg<-aggregate(Accident_Index ~ data_frame[,2], data = data_frame, FUN = function(x){NROW(x)} / nrow(data_frame))
    names(agg)[names(agg) == "data_frame[, 2]"] <- names(data_frame)[2]
    agg<-agg[order(-agg$Accident_Index),]
    barplot(agg$Accident_Index,
            main = ifelse(main == "",paste("Number of Accidents per"),main),
            xlab = "Severity",
            ylab = "No. of Accidents",
            names.arg = agg$Accident_Severity,
            col = "darkred",
            horiz = FALSE,
            ylim = c(0,1))
  } else {
    for (name in names(data_frame[,c(2:ncol(data_frame))])){
      agg<-aggregate(Accident_Index ~ data_frame[,name], data = data_frame, FUN = function(x){NROW(x)} / nrow(data_frame))
      names(agg)[names(agg) == "data_frame[, name]"] <- name
      agg<-agg[order(-agg$Accident_Index),]
      barplot(agg$Accident_Index,
              main =ifelse(main == "",paste("Number of Accidents per",name,sep = " "),main),
              xlab = name,
              ylab = "No. of Accidents",
              names.arg = agg[,name],
              col = "darkred",
              horiz = FALSE,
              ylim = c(0,1))
    } 
  }
}
# ************************************************************
# Hypothesis_3_Preprocess_ordinal() :
#
# Preprocessing for K-means Clustering - Ordinal Columns
#
# INPUT:   data frame - clustering data without Police Force Column
#          
# OUTPUT : data frame - Ordinal columns processed for Clustering
# ***********************************************************

Hypothesis_3_Preprocess_ordinal <- function(Clustering_Datset) {
  
  # All our columns are categorical except for age and age of vehicle  
  ordinals <- Clustering_Datset[,9:10]
  zscaled<-apply(ordinals, MARGIN = 2,
                 FUN = function(X) (scale(X,center=TRUE,
                                          scale=TRUE)))
  
  # Scale in this case to be [0,1]
  ordinalReadyforML<-Nrescaleentireframe(as.data.frame(zscaled)) #Nick's code
  
  return(ordinalReadyforML)
} #end of Hypothesis_3_Preprocess_ordinal

# ************************************************************
# Hypothesis_3_Preprocess_categorical() :
# This code has been taken from Lab 6 of the module.
# Preprocessing for K-means Clustering - Categorical Columns
# Uses One-Hot-Encoding
#
# INPUT:   data frame - clustering data without Police Force Column
#          
# OUTPUT : data frame - Categorical columns processed for Clustering
# ***********************************************************
Hypothesis_3_Preprocess_categorical <- function(Clustering_Dataset){
  
  # Pre-process Categorical Column using one-hot-encoding
  categorical_columns <- Clustering_Dataset[,1:8]
  
  categorical<-data.frame(first=rep(NA,nrow(categorical_columns)),
                          stringsAsFactors=FALSE)
  
  #For every field in our dataset
  for(field in 1:(ncol(categorical_columns))){
    
    #Create a list of unique values in the field (each is a literal)
    literals<-as.vector(unique(categorical_columns[,field]))
    numberLiterals<-length(literals)
    
    #if there are just two literals in the field we can convert to 0 and 1
    if (numberLiterals==2){
      transformed<-ifelse (categorical_columns[,field]==literals[1],0.0,1.0)
      categorical<-cbind(categorical,transformed)
      colnames(categorical)[ncol(categorical)]<-colnames(categorical_columns)[field]
      
    } else
    {
      #We have now to one-hot encoding FOR SMALL NUMBER of literals
      if (numberLiterals<=MAX_LITERALS){
        for(num in 1:numberLiterals){
          nameOfLiteral<-literals[num]
          hotEncoding<-ifelse (categorical_columns[,field]==nameOfLiteral,1.0,0.0)
          
          # 5/3/2018 - do not convert the field if their are too few literals
          # Use log of number of recrods as the measure
          literalsActive<-sum(hotEncoding==1)
          if (literalsActive>log(length(hotEncoding))) {
            categorical<-cbind(categorical,hotEncoding)
            #060819 field name has the "_" seperator to make easier to read
            colnames(categorical)[ncol(categorical)]<-paste(colnames(categorical_columns)[field],
                                                            "_",
                                                            NPREPROCESSING_removePunctuation(nameOfLiteral),
                                                            sep="")
          }
          else {
            warning(paste("Ignoring in field:",
                          names(categorical_columns)[field],
                          "Literal:",nameOfLiteral,
                          "Too few=",literalsActive))
          }
        }
      } else {
        stop(paste("Too many literals in:",
                   names(categorical_columns)[field],
                   numberLiterals))
      }
      
    }
  }
  categorical <- categorical[,-1]
  
  return(categorical)
  
}# end of Hypothesis_3_Preprocess_CombinedDatasets

# ************************************************************
# Hypothesis_3_Preprocess_CombinedDatasets() :
#
# Preprocessing for K-means Clustering - Combine processed ordinal and categorical columns
#
# INPUT:   data frames - Ordinal and Categorical Columns processed
#          
# OUTPUT : data frame - Combined data frame of all data 
# ***********************************************************
Hypothesis_3_Preprocess_CombinedDatasets <- function(ordinal, categorical){
  combinedML<-cbind(ordinal,categorical)
  
  #Check for correlation between fields
  combinedML<-NPREPROCESSING_redundantFields(dataset=combinedML,cutoff=0.90) #Nick's code
  
  print(paste("After Pre-Processing Total Fields=",ncol(combinedML)))
  
  #Remove any punctuaton for ML
  names(combinedML)<-gsub(" ", "", names(combinedML), fixed = TRUE)
  
  return(combinedML)
} #end of Hypothesis_3_Preprocess_CombinedDatasets

# ************************************************
# NPREPROCESSING_redundantFields() :
# This code has been taken from Prof. Nick F Ryman-Tubb, PBA Lab 6
# Determine if an entire field is redundant
# Uses LINEAR correlation,
# so use with care as information will be lost
#
# INPUT: data frame - dataset - numeric values only
#        double     - cutoff  - Value above which is determined redundant [0,1]
#
# OUTPUT : Frame - dataset with any fields removed
# ************************************************
NPREPROCESSING_redundantFields<-function(dataset,cutoff){
  
  print(paste("Before redundancy check Fields=",ncol(dataset)))
  
  #Remove any fields that have a stdev of zero (i.e. they are all the same)
  xx<-which(apply(dataset, 2, function(x) sd(x, na.rm=TRUE))==0)+1
  
  if (length(xx)>0L)
    dataset<-dataset[,-xx]
  
  # "Kendall" is more robust for data do not necessarily come from a bivariate normal distribution.
  cr<-cor(dataset, use="everything")
  
  NPLOT_correlagram(cr)
  
  correlated<-which(abs(cr)>=cutoff,arr.ind = TRUE)
  list_fields_correlated<-correlated[which(correlated[,1]!=correlated[,2]),]
  
  if (length(list_fields_correlated)>0){
    
    print("Following fields are correlated")
    print(list_fields_correlated)
    
    #We have to check if one of these fields is correlated with another as cant remove both!
    v<-vector()
    numc<-nrow(list_fields_correlated)
    for (i in 1:numc){
      if (length(which(list_fields_correlated[i,1]==list_fields_correlated[i:numc,2]))==0) {
        v<-append(v,list_fields_correlated[i,1])
      }
    }
    print("Removing the following fields")
    print(names(dataset)[v])
    
    return(dataset[,-v]) #Remove the first field that is correlated with another
  }
  return(dataset)
}
# ************************************************
# Nrescaleentireframe() :
# This code has been taken from Prof. Nick F Ryman-Tubb, PBA Lab 6
# Rescle the entire dataframe to [0.0,1.0]
#
# INPUT:   data frame - dataset - numeric data frame
#
# OUTPUT : data frame - scaled numeric data frame
# ************************************************
Nrescaleentireframe<-function(dataset){
  
  scaled<-sapply(as.data.frame(dataset),Nrescale)
  return(scaled)
}
# ************************************************
# Nrescale() :
# This code has been taken from Prof. Nick F Ryman-Tubb, PBA Lab 6
# These are the real values, that we scale between 0-1
# i.e. x-min / (max-min)
#
# INPUT:   vector - input - values to scale
#
# OUTPUT : vector - scaled values to [0.0,1.0]
# ************************************************
Nrescale<-function(input){
  
  minv<-min(input)
  maxv<-max(input)
  return((input-minv)/(maxv-minv))
}
# ************************************************
# NPREPROCESSING_removePunctuation()
# INPUT: String - name of field with possible punctuation/spaces
# This code has been taken from Prof. Nick F Ryman-Tubb, PBA Lab 6
# OUTPUT : String - name of field with punctuation removed
# ************************************************
NPREPROCESSING_removePunctuation<-function(fieldName){
  return(gsub("[[:punct:][:blank:]]+", "", fieldName))
}
# ************************************************
# NPLOT_correlagram() :
#
# Plots PLOT_correlagram
## This code has been taken from Prof. Nick F Ryman-Tubb, PBA Lab 6
# INPUT: data frame - cr - n x n frame of correlation coefficients
#
# OUTPUT : None
# 221019 - plot absolute values only
# ************************************************
NPLOT_correlagram<-function(cr){
  #Defines the colour range
  col<-colorRampPalette(c("green", "red"))
  
  #To fir on screen, convert field names to a numeric
  rownames(cr)<-1:length(rownames(cr))
  colnames(cr)<-rownames(cr)
  
  corrplot::corrplot(abs(cr),method="square",
                     order="FPC",
                     cl.ratio=0.2,
                     cl.align="r",
                     tl.cex = 0.6,cl.cex = 0.6,
                     cl.lim = c(0, 1),
                     mar=c(1,1,1,1),bty="n")
}


# ************************************************
# Hypothesis_2_Preprocess_DeepLearning()
# Preprocess time series data by detrending and scaling the number of accidents
# Uses Sliding Window method
# Some aspects of the code adapted from PBA Lab 5
#
# INPUT:    Data Frame - Time Series of Number of Accidents (row names are dates)
#
# OUTPUT :  data frame - train dataset
#           data frame - test dataset
#           double - min value in realworld dataset
#           double - max value in realworld dataset
#
# ************************************************
Hypothesis_2_Preprocess_DeepLearning <- function(TimeSeries) {
  
  TimeSeries$means <- 0
  
  TimeSeries<-Ndetrend(openStockPrice = TimeSeries) #Ndetrend taken from lab 5
  
  realmin<-min(TimeSeries[,1])
  realmax<-max(TimeSeries[,1])
  TimeSeries[,1]<-Scale_0_1(TimeSeries[,1]) #scale time series [0,1]
  
  #Plot the detrend and scaled dataset
  print(paste("Detrended Time Series over",TIME_SLOTS,"days"))
  
  plot(as.timeSeries(TimeSeries[,1,drop=FALSE]),
       at="chic",
       plot.type="s",
       col = "aquamarine4",
       cex.axis=0.75,
       type="l",
       cex.pch=0.5,
       ylab="Number of Accidents")
  
  title(paste("Detrend the timeseries over",TIME_SLOTS,"days"))
  
  #Sliding Window Method from PBA lab 5
  res<-N_SlideWindow(timeSeriesData=TimeSeries,window=TIME_SLOTS) #rename prices as number of accidents
  
  r1 <- Nsplit(res)
  r1$min<-realmin
  r1$max<-realmax
  
  
  return(r1)  
  
} #end of Hypothesis_2_Preprocess_DeepLearning

# ************************************************
# Nsplit()
# Split data set as:
# train/test values, train/test mean values
#
# INPUT: Frame - dataset
#
# OUTPUT : Frame - test dataset
#          Frame - train dataset
#          Frame - test dataset as mean values
#          Frame - train dataset as mean values
# ************************************************

Nsplit <- function(res) {
  
  train <- 1:SPLIT_TRAIN
  test <- -train
  
  training_data <- res$prices[train,]
  testing_data = res$prices[test,]
  
  training_means <- res$means[train,]
  testing_means = res$means[test,]
  
  splitDate<-row.names(testing_data)[1]
  
  retList<-list(train=training_data,
                test=testing_data,
                trainMeans=training_means,
                testMeans=testing_means,
                splitDate=splitDate)
  
  return(retList)
  
} #end of Nsplit

# ************************************************
# N_visualiseTimeSeriesResults()
# Plot expected and predicted timeseries in two panels
# Most of this code has been adapted from PBA Lab 5
#
# INPUT:    double vector - expected     - expected values
#           double vector - predicted    - predicted values
#           double vector - means        - detrend means or 1
#           string vector - dates        - dates of timeseries
#           double        - realmin      - scale minimum value
#           double        - realmax      - scale  maximum value
#           string        - title        - title on graph
#           strings       - yaxis_title - (first plot, second plot)
#           Bool          - measuresFLag -TRUE to show RMSE and R2
#
# OUTPUT :  data frame - rows are dates
#                        [expected, predicted]
#
# ************************************************
N_visualiseTimeSeriesResults<-function(expected,
                                       predicted,
                                       means,
                                       dates,
                                       realmin,
                                       realmax,
                                       title,
                                       yaxis_title,
                                       measuresFLag){
  
  #Scale this back into the real values and multiply back detrend offset
  expected<-N_unscale0_to_1(expected,realmin,realmax)*means
  predicted<-N_unscale0_to_1(predicted,realmin,realmax)*means
  
  #Create a dataframe with the expected values and then the predictions with dates
  exp_pred<-data.frame(expected=expected,predicted=predicted)
  colnames(exp_pred)<-c("expected","predicted")
  row.names(exp_pred)<-dates
  
  #Uses the timeSeries library
  #Plot nice timeseries stock price chart
  #Show actual and predicted in seperate panels
  plot(as.timeSeries(exp_pred), at="chic", plot.type="m", cex.axis=0.75, type=c("l", "l"),ylab=yaxis_title)
  title(main = title)
  
  #Show test dataset with actual as black line and predicted overlaid as red points
  type<-ifelse(measuresFLag==TRUE,c("l", "p"),c("l", "l"))
  plot(as.timeSeries(exp_pred), at="chic", plot.type="s",
       cex.axis=0.75, type=type,cex.pch=0.5,ylab="Number of Accidents")
  legend("bottomleft", inset=c(0.05,-0.1), bty = "n", bg="transparent" ,horiz=TRUE, yaxis_title, col=1:ncol(exp_pred), lty=1, cex=.8)
  
  if (measuresFLag==TRUE){
    
    #Calculate measures
    RMSE<-Nrmse(exp_pred$expected,exp_pred$predicted)
    r2<-Nr2(exp_pred$expected,exp_pred$predicted)
    MAE<-Nmae(exp_pred$expected,exp_pred$predicted)
    title(main = paste(title,"- RMSE",round(RMSE,digits=2),
                       "MAE",round(MAE,digits=2),
                       "R2",round(r2,digits=4)))
  } else {
    title(main = title)
  }
  
  return(exp_pred)
}

# ************************************************
# N_timeSeriesRNNPredict()
# Recurrent neural; network model and predict
# Newest stock prices going back RNN_SLOTS to test
# Earlier stock prices to train
#
# This code has been taken from PBA Lab 5
#
# INPUT:    Frame - stock prices, rownames are dates
#
# OUTPUT :  data frame - dates and predicted stock prices
#
# ************************************************
N_timeSeriesRNNPredict<-function(openStockPrice){
  
  print("RNN - Training")
  
  # Select the newest stock price figures from the dataset
  # for two RNN_SLOTS
  latest<-tail(openStockPrice,((RNN_SLOTS*2)+1))
  block<-latest[1:((RNN_SLOTS*2)+1),,drop=FALSE]
  
  #scale the entire block [0,1]
  scaled<-Scale_0_1(block)
  
  # Input values to the NN as a sequence
  # Output values are +1 day ahead in the sequence
  ii<-scaled[1:(RNN_SLOTS*2),,drop=FALSE]
  oo<-scaled[2:((RNN_SLOTS*2)+1),,drop=FALSE]
  
  #Just show the inputs and outputs overlaid
  # N_visualiseTimeSeriesResults(expected=ii,
  #                              predicted=oo,
  #                              means=1,
  #                              dates=rownames(ii),
  #                              realmin=min(block),
  #                              realmax=max(block),
  #                              title="RNN Training Dataset",
  #                              yaxis_title=c("Input","Output"),
  #                              measuresFLag=FALSE)
  
  
  # Format variables for the RNN library model
  Y<-as.matrix(oo[1:RNN_SLOTS,])
  X<-as.matrix(ii[1:RNN_SLOTS,])
  
  #Train the basic RNN
  model <- trainr(Y = t(Y),
                  X = t(X),
                  network_type = "rnn",
                  sigmoid = c("logistic"),
                  learningrate = 0.05,
                  hidden_dim = RNN_NEURONS,
                  numepochs = RNN_EPOCHS,
                  epoch_function = c(Nnoprint))
  
  
  #Predict values for the entire dataset (so both train and test)
  whole<-as.matrix(ii)
  p <- t(predict_lstm(model, t(whole)))
  
  # Extract the train dataset and visualise prediction accuracy
  trainr<-1:RNN_SLOTS
  train_exp<-as.matrix(oo[trainr,,drop=FALSE])
  train_pre<-p[trainr,,drop=FALSE]
  
  N_visualiseTimeSeriesResults( expected=train_exp,
                                predicted=train_pre,
                                means=1,
                                dates=names(whole[trainr,]),
                                realmin=min(block),
                                realmax=max(block),
                                title="RNN Training Dataset",
                                yaxis_title=c("Expected","Predicted"),
                                measuresFLag=TRUE)
  
  
  print(paste("Train Period",row.names(train_exp)[1],tail(row.names(train_exp),1)))
  
  # Extract the test dataset and visualise prediction accuracy
  testr<-(RNN_SLOTS+1):(RNN_SLOTS*2)
  test_exp<-as.matrix(oo[testr,,drop=FALSE])
  
  test_pre<-p[testr,,drop=FALSE]
  real_exp_pred<-N_visualiseTimeSeriesResults( expected=test_exp,
                                               predicted=test_pre,
                                               means=1,
                                               dates=rownames(oo[testr,,drop=FALSE]),
                                               realmin=min(block),
                                               realmax=max(block),
                                               title="RNN Test Dataset",
                                               yaxis_title=c("Expected","Predicted"),
                                               measuresFLag=TRUE)
  
  print(paste("Test Period",row.names(test_exp)[1],tail(row.names(test_exp),1)))
  
  return(real_exp_pred)
}
# ************************************************
# Ndetrend()
# Detrend each timeslot in the dataset
#
# INPUT:    Data Frame - stock prices, rownames are dates
#
# OUTPUT :  Data Frame - detrened stock prices
#
# ************************************************
Ndetrend<-function(openStockPrice){
  
  # ************************************************
  # NdetrendEntry()
  # SCOPE: Ndetrend()
  # This code has been taken from PBA Lab 5
  # Detrend individual timeslot by dividing by its mean
  #
  # INPUT:    Data Frame - openStockPriceEntry - stock prices
  #
  # OUTPUT :  Data Frame - detrended and with $means added
  #
  # ************************************************
  NdetrendEntry<-function(openStockPriceEntry){
    
    openStockPriceEntry$means<-mean(openStockPriceEntry[,1])
    openStockPriceEntry[,1]<-openStockPriceEntry[,1]/openStockPriceEntry$means
    
    return(openStockPriceEntry)
  } #endof NdetrendEntry()
  
  # ************************************************
  # Start Ndetrend()
  
  for (eachSlot in 1:as.integer(nrow(openStockPrice)/TIME_SLOTS)){
    start<-((eachSlot-1)*TIME_SLOTS)+1
    end<-start+TIME_SLOTS-1
    openStockPrice[start:end,]<-NdetrendEntry(openStockPrice[start:end,])
  }
  
  # This does the last few entries as the dataset may not be multiple
  # of the TIME_SLOTS
  end<-nrow(openStockPrice)
  if ((end %% TIME_SLOTS)!=0){
    start<-(eachSlot*TIME_SLOTS)+1
    openStockPrice[start:end,]<-NdetrendEntry(openStockPrice[start:end,])
  }
  return(openStockPrice)
} # Ndetrend()

# ************************************************
# Scale_0_1()
# Scale values [0.0,1.0]
#
# INPUT:    vector double - value to scale
# OUTPUT :  vector double - scaled values [0.0,1.0]
#
# ************************************************
Scale_0_1 <- function(x){(x-min(x))/(max(x)-min(x))}

# ************************************************
# N_SlideWindow()
#
# Create a dataset with "window" columns that are
# shifted one place (i.e. one time period) to the left
# This function has been taken from PBA Lab 5
#
# INPUT:     dataframe - timeSeriesData - row names are dates
#            int       - window         - size of time window
#
# OUTPUT :   dataframe - $prices row names are dates, columns "V1","V2"...
#                      - $means mean of each time window
#
# ************************************************
N_SlideWindow<-function(timeSeriesData, window){
  
  # efficient to use a pre-allocated matrix
  calcRows<-nrow(timeSeriesData)
  prices<-matrix(NA, nrow = calcRows, ncol = window)
  means<-matrix(NA, nrow = calcRows, ncol = window)
  
  for (r in 1:(calcRows)){
    prices[r,]<-timeSeriesData[(r):(r+window-1),1]
    means[r,]<-timeSeriesData$means[(r):(r+window-1)]
  }
  
  # Remove possiblke part of df that cannot be filled with previous days
  prices<-prices[1:((calcRows-window)+1),]
  means<-means[1:((calcRows-window)+1),]
  
  pricesFrame<-as.data.frame(prices)
  meansFrame<-as.data.frame(means)
  
  # Save the date of stock price to a new3 column called $date
  # The date is the date for V10 (the value we are predicting)
  dates<-rownames(timeSeriesData)[window:(nrow(timeSeriesData))]
  pricesFrame$date<-dates
  rownames(pricesFrame)<-dates
  
  meansFrame$date<-dates
  
  retList<-list("prices"=pricesFrame,
                "means"=meansFrame)
  return(retList)
}
# ************************************************
# Calculate the r2 measure
#
# INPUT:      vector - double values for expected values
#             vector - double values of predicted values
# OUTPUT :    double - calculated N2
# ************************************************
Nr2<-function(actual,preds){
  
  rsq<-cor(preds, actual) ^ 2
  
  return(rsq)
}


# ************************************************
# N_DEEP_Initialise()
# Initialise the H2O server
#
# INPUT:
#         Bool       - reproducible       - TRUE if model must be reproducable each run
#
# OUTPUT : none
# ************************************************
N_DEEP_Initialise<-function(reproducible=TRUE){
  
  library(h2o)
  
  print("Initialise the H2O server")
  #Initialise the external h20 deep learning local server if needed
  #130517NRT - set nthreads to -1 to use maximum so fast, but set to 1 to get reproducable results
  #080819NRT - use reproducable parameter
  if (reproducible==TRUE)
    nthreads<-1
  else
    nthreads<- -1
  
  h2o.init(max_mem_size = "5g",nthreads = nthreads)
  
  h2o.removeAll() # 261019NRT clean slate - just in case the cluster was already running
  #h2o.no_progress()
}

# ************************************************
# N_DEEP_Train()
# This code has been taken from PBA Lab 5
# h2O NEURAL NETWORK : DEEP LEARNING CLASSIFIER TRAIN
#
# INPUT:  Frame      - train              - scaled [0.0,1.0], fields & rows
#         String     - fieldNameOutput    - Name of the field to classify
#         Int Vector - hidden             - Number of hidden layer neurons for each layer
#         int        - stopping_rounds    - Number of times no improvement before stop
#         double     - stopping_tolerance - Error threshold
#         String     - activation         - Name of activation function
#         Bool       - reproducible       - TRUE if model must be reproducable each run
#         Bool       - regression         - TRUE to predict values rather than classification
#
# OUTPUT: object     - trained neural network
# ************************************************
N_DEEP_Train<- function(train,
                        fieldNameOutput,
                        hidden,
                        stopping_rounds,
                        stopping_tolerance,
                        activation,
                        reproducible=TRUE,
                        regression=FALSE){
  
  #positionOutput<-which(names(test)==fieldNameOutput)
  
  #Creates the h2o training dataset
  if (regression==FALSE) {
    train[fieldNameOutput] <- lapply(train[fieldNameOutput] , factor) #Output class has to be a R "factor"
  }
  
  train_h2o <- as.h2o(train, destination_frame = "traindata")
  
  # Create validation dataset for early stopping
  splits <- h2o.splitFrame(train_h2o, 0.9, seed=1234)
  nntrain  <- h2o.assign(splits[[1]], "nntrain.hex") # 90%
  nnvalid  <- h2o.assign(splits[[2]], "nnvalid.hex") # 10%
  
  #This lists all the input field names ignoring the fieldNameOutput
  predictors <- setdiff(names(train_h2o), fieldNameOutput)
  
  # Deep training neural network
  # Updated 13/5/17 - set reproducible = TRUE so that the same random numbers are used to initalise
  # 281019NRT - added validation dataset for early stopping
  
  deep<-h2o::h2o.deeplearning(x=predictors,
                              y=fieldNameOutput,
                              training_frame = nntrain,
                              validation_frame=nnvalid,
                              epochs=BASICNN_EPOCHS,
                              hidden=hidden,
                              adaptive_rate=TRUE,
                              stopping_rounds=stopping_rounds,
                              stopping_tolerance=stopping_tolerance,
                              stopping_metric = "AUTO",
                              fast_mode=FALSE,
                              activation=activation,
                              seed=1234,
                              l1 = 1e-2,
                              l2 = 1e-2,
                              reproducible = TRUE)
  
  return(deep)
}


# ************************************************
# N_EVALUATE_DeepNeural() :
#
# Evaluate Deep Neural Network classifier
# Generates probabilities from the classifier
# This code has been taken from PBA Lab 5
# INPUT: Data Frame    -  test             - scaled [0.0,1.0], fields & rows
#        String        -  fieldNameOutput  - field to predict
#        Object        - deep              - trained NN
#
# OUTPUT :
#         double vector - predicted values
# ************************************************
# Uses   library(h2o)
N_EVALUATE_DeepNeural<-function(test,
                                fieldNameOutput,
                                deep,regression=FALSE) {
  
  #Creates the h2o test dataset
  if (regression==FALSE)
    # Output class has to be a R "factor" for classification
    test[fieldNameOutput] <- lapply(test[fieldNameOutput] , factor)
  
  test_h2o <- as.h2o(test, destination_frame = "testdata")
  
  pred <- as.vector(h2o::h2o.predict(deep, test_h2o))
  
  return(pred)
}
# ************************************************
# N_timeSeriesRNNPredict()
# Recurrent neural; network model and predict
# Newest stock prices going back RNN_SLOTS to test
# Earlier stock prices to train
#
# INPUT:    Frame - stock prices, rownames are dates
#
# OUTPUT :  data frame - dates and predicted stock prices
#
# ************************************************
N_timeSeriesRNNPredict<-function(openStockPrice){
  
  print("RNN - Training")
  
  # Select the newest stock price figures from the dataset
  # for two RNN_SLOTS
  latest<-tail(openStockPrice,((RNN_SLOTS*2)+1))
  block<-latest[1:((RNN_SLOTS*2)+1),,drop=FALSE]
  
  #scale the entire block [0,1]
  scaled<-Scale_0_1(block)
  
  # Input values to the NN as a sequence
  # Output values are +1 day ahead in the sequence
  ii<-scaled[1:(RNN_SLOTS*2),,drop=FALSE]
  oo<-scaled[2:((RNN_SLOTS*2)+1),,drop=FALSE]
  
  #Just show the inputs and outputs overlaid
  # N_visualiseTimeSeriesResults(expected=ii,
  #                              predicted=oo,
  #                              means=1,
  #                              dates=rownames(ii),
  #                              realmin=min(block),
  #                              realmax=max(block),
  #                              title="RNN Training Dataset",
  #                              yaxis_title=c("Input","Output"),
  #                              measuresFLag=FALSE)
  
  
  # Format variables for the RNN library model
  Y<-as.matrix(oo[1:RNN_SLOTS,])
  X<-as.matrix(ii[1:RNN_SLOTS,])
  
  #Train the basic RNN
  model <- trainr(Y = t(Y),
                  X = t(X),
                  network_type = "rnn",
                  sigmoid = c("logistic"),
                  learningrate = 0.05,
                  hidden_dim = RNN_NEURONS,
                  numepochs = RNN_EPOCHS,
                  epoch_function = c(Nnoprint))
  
  #Predict values for the entire dataset (so both train and test)
  whole<-as.matrix(ii)
  p <- t(predictr(model, t(whole)))
  
  # Extract the train dataset and visualise prediction accuracy
  trainr<-1:RNN_SLOTS
  train_exp<-as.matrix(oo[trainr,,drop=FALSE])
  train_pre<-p[trainr,,drop=FALSE]
  
  N_visualiseTimeSeriesResults( expected=train_exp,
                                predicted=train_pre,
                                means=1,
                                dates=names(whole[trainr,]),
                                realmin=min(block),
                                realmax=max(block),
                                title="RNN Training Dataset",
                                yaxis_title=c("Expected","Predicted"),
                                measuresFLag=TRUE)
  
  
  print(paste("Train Period",row.names(train_exp)[1],tail(row.names(train_exp),1)))
  
  # Extract the test dataset and visualise prediction accuracy
  testr<-(RNN_SLOTS+1):(RNN_SLOTS*2)
  test_exp<-as.matrix(oo[testr,,drop=FALSE])
  
  test_pre<-p[testr,,drop=FALSE]
  real_exp_pred<-N_visualiseTimeSeriesResults( expected=test_exp,
                                               predicted=test_pre,
                                               means=1,
                                               dates=rownames(oo[testr,,drop=FALSE]),
                                               realmin=min(block),
                                               realmax=max(block),
                                               title="RNN Test Dataset",
                                               yaxis_title=c("Expected","Predicted"),
                                               measuresFLag=TRUE)
  
  print(paste("Test Period",row.names(test_exp)[1],tail(row.names(test_exp),1)))
  
  return(real_exp_pred)
}

# ************************************************
# N_unscale0_to_1()
# Take values [0.0,1.0] and convert back to real values
# This function has been taken from PBA Lab 5
#
# INPUT:    vector double - value to unscale
#           double - min real value
#           double - max real value
# OUTPUT :  vector double - unscaled values [min,max]
#
# ************************************************
N_unscale0_to_1 <- function(x,minv,maxv){
  return(((maxv-minv)*x)+minv)
}
# ************************************************
# Nrmse() : Calculate the RMSE statistic
#
# INPUT: actual_y vector of real numbers indicating the known class
#        y_predicted vector of real numbers indicating the predicted class
#
# OUTPUT : double RMSE
# ************************************************
Nrmse<-function(actual_y,y_predicted){
  
  return(sqrt(mean((actual_y-y_predicted)^2)))
}
# ************************************************
# Nmae: Calculate the MAE statistic
#
# INPUT: actual_y vector of real numbers indicating the known class
#        y_predicted vector of real numbers indicating the predicted class
#
# OUTPUT : double - MAE
# ************************************************
Nmae<-function(actual_y,y_predicted){
  
  return((mean(abs(actual_y-y_predicted))))
}
# ************************************************
# Nnoprint()
# For RNN library to stop output to console
# as console output slows down training
#
#
#
# INPUT:    object - model
#
# OUTPUT :  object - model
#
# ************************************************
Nnoprint<-function(model){
  return(model)
}

