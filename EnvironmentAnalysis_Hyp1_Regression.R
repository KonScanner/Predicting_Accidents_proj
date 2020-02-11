# ********************************************************************************************
#  PRATICAL BUSINESS ANALYTICS 2019
#  Dataholics - cleaning.R
#
#
# EXECUTE THIS FILE FROM MAIN
#
# 21 NOVEMBER 2019
# 
# UPDATE
# 1.00      21/11/2019    Initial Version - Hannes, Aiden, Ben, Konstantinos, Lux, Victor, Ben
# 1.01      
# 1.02      Victor: added PATH_TO_DATASETS
# 1.03      Hannes: added plotting, added time taking, added Percentage output
# 1.10      Hannes: 30/11/2019  Merging with main
# *********************************************************************************************
# 
# *********************************************************************************************
print(paste("Start Regression on",OUTPUT_FIELD,sep = ""))
start<-Sys.time()


set.seed(123)

if (exists("accidents_clean_old_severity") | exists("accidents_clean_new_severity")){
  if (OUTPUT_FIELD == "Custom_Severity") {
  accidents <- accidents_clean_new_severity  
  } else {
    accidents <- accidents_clean_old_severity
  }
} else {
  accidents <- read.csv(DATASET_FILENAME)
} 


names(accidents)[names(accidents) == "Pedestrian_Crossing.Human_Control"] <- "Crossing_Human_Control"
names(accidents)[names(accidents) == "Pedestrian_Crossing.Physical_Facilities"] <- "Crossing_Physical_Facilities"

# randomize data
accidents<-accidents[order(runif(nrow(accidents))),]

accidents<-accidents[,c(which(colnames(accidents)==OUTPUT_FIELD),which(colnames(accidents)!=OUTPUT_FIELD))]

# First we split the data
if (whole == TRUE) {
  training_records<-round(nrow(accidents)*(70/100))
  training_data <- accidents[1:training_records,]
  testing_data = accidents[-(1:training_records),]
} else {
  accidents <- accidents[1:(nrow(accidents) * (1/5)),]
  training_records<-round(nrow(accidents)*(70/100))
  training_data <- accidents[1:training_records,]
  testing_data = accidents[-(1:training_records),]
}



# *********************************************************************************************************************************************
# We run a simple multivariate linear regression to determine if there are any 
# Parameters that have big influence on the output field
if(OUTPUT_FIELD == "Accident_Severity"){
  linear_regression <- lm(Accident_Severity ~ ., data = training_data)
} else {
  linear_regression <- lm(Custom_Severity ~ ., data = training_data)
}

print(linear_regression)
# To determine the importance of certain Variables we use the method introduced in the lab functions
par(mar = c(7,4.1,4.1,2.1))
importance<-as.data.frame(caret::varImp(linear_regression, scale = FALSE))
importance <- importance[order(importance$Overall),,drop=FALSE]
barplot(importance$Overall, cex.names = 0.65, las = 2, space=1, xlab = "", main = paste("Variable Importance, linear regression,",OUTPUT_FIELD, paste = " "))
end_point = 0.5 + nrow(importance) + nrow(importance)-1
text(seq(1.5,end_point,by=2), par("usr")[3]-0.25, 
     srt = 60, adj= 1, xpd = TRUE,
     labels = paste(row.names(importance)), cex=0.65)

#Calculate Metrics of that model for predicting OUTPUT_FIELD
#
# Extract predictor (input) values from TEST dataset into a data frame
predictorInput<-testing_data[,(names(testing_data) != OUTPUT_FIELD)]

# Get predictions from the model using the TEST dataset
y_predicted<-as.data.frame(predict(linear_regression, predictorInput))

# Extract the expected response (output) values from TEST dataset
# into a data frame
y_actual<-subset(testing_data,select=OUTPUT_FIELD)
y_actual[,OUTPUT_FIELD]<-as.numeric(y_actual[,OUTPUT_FIELD])

# Calculate the metrics using functions in lab2functions.R
RMSE<-round(sqrt(mean((y_actual[2:nrow(y_actual),] - y_predicted[2:nrow(y_predicted),])^2)),digits=2)
mae<-round((mean(abs(y_actual[2:nrow(y_actual),] - y_predicted[2:nrow(y_predicted),]))),digits=2)
r2<-round((cor(y_predicted[2:nrow(y_predicted),], y_actual[2:nrow(y_actual),]) ^ 2),digits=2)

print(paste("Values for simple multivariate linear regression on ",OUTPUT_FIELD,":  ","MAE = ",mae,", RMSE = ",RMSE, ", R2 = ",r2))


# ******************************************************************************************************************************************
# Next step is a more sophisticated regression approach. We go for a polynomial multivariate regression
#
# 

if(OUTPUT_FIELD == "Accident_Severity"){
  MARS <- earth(Accident_Severity ~ ., data = training_data, degree = 6)
} else {
  MARS <- earth(Custom_Severity ~ ., data = training_data, degree = 6)
}

print(MARS)
# To determine the importance of certain Variables we use the method introduce in the lab functions
importance<-as.data.frame(caret::varImp(MARS, scale = FALSE))
importance <- importance[order(importance$Overall),,drop=FALSE]
barplot(importance$Overall, cex.names = 0.65, las = 2, space=1, xlab = "", main = paste("Variable Importance, MARS,",OUTPUT_FIELD, paste = " "))
end_point = 0.5 + nrow(importance) + nrow(importance)-1
text(seq(1.5,end_point,by=2), par("usr")[3]-0.25, 
     srt = 60, adj= 1, xpd = TRUE,
     labels = paste(row.names(importance)), cex=0.65)
#Calculate Metrics of that model
#
# Extract predictor (input) values from TEST dataset into a data frame
predictorInput<-testing_data[,(names(testing_data) != OUTPUT_FIELD)]

# Get predictions from the model using the TEST dataset
y_predicted<-as.data.frame(predict(MARS, predictorInput))

# Extract the expected response (output) values from TEST dataset
# into a data frame
y_actual<-subset(testing_data,select=OUTPUT_FIELD)
y_actual[,OUTPUT_FIELD]<-as.numeric(y_actual[,OUTPUT_FIELD])

# Calculate the metrics using functions in lab2functions.R
RMSE<-round(sqrt(mean((y_actual[2:nrow(y_actual),] - y_predicted[2:nrow(y_predicted),])^2)),digits=2)
mae<-round((mean(abs(y_actual[2:nrow(y_actual),] - y_predicted[2:nrow(y_predicted),]))),digits=2)
r2<-round((cor(y_predicted[2:nrow(y_predicted),], y_actual[2:nrow(y_actual),]) ^ 2),digits=2)

print(paste("Values for MARS regression on ",OUTPUT_FIELD,":  ","MAE = ",mae,", RMSE = ",RMSE, ", R2 = ",r2))

# ***************************************************************************************************************
# Try a MARS model with cross validation
if(OUTPUT_FIELD == "Accident_Severity"){
  MARS_tune <- train(
    x = subset(training_data, select = -Accident_Severity),
    y = training_data$Accident_Severity,
    method = "earth",
    metric = "RMSE",
    trControl = trainControl(method = "cv", number = 10)
  )
} else {
  MARS_tune <- train(
    x = subset(training_data, select = -Custom_Severity),
    y = training_data$Custom_Severity,
    method = "earth",
    metric = "RMSE",
    trControl = trainControl(method = "cv", number = 10)
  )
}
# best model
MARS_tune$bestTune
print(MARS_tune)
ggplot(MARS_tune)
# plot variable importance
p1 <- vip(MARS_tune, num_features = 40, bar = FALSE) + ggtitle(paste("Variable Importance, MARS with Cross Validation",OUTPUT_FIELD, sep = " "))
gridExtra::grid.arrange(p1)

#Calculate Metrics of that model
#
# Extract predictor (input) values from TEST dataset into a data frame
predictorInput<-testing_data[,(names(testing_data) != OUTPUT_FIELD)]

# Get predictions from the model using the TEST dataset
y_predicted<-as.data.frame(predict(MARS_tune, predictorInput))

# Extract the expected response (output) values from TEST dataset
# into a data frame
y_actual<-subset(testing_data,select=OUTPUT_FIELD)
y_actual[,OUTPUT_FIELD]<-as.numeric(y_actual[,OUTPUT_FIELD])

# Calculate the metrics using functions in lab2functions.R
RMSE<-round(sqrt(mean((y_actual[2:nrow(y_actual),] - y_predicted[2:nrow(y_predicted),])^2)),digits=2)
mae<-round((mean(abs(y_actual[2:nrow(y_actual),] - y_predicted[2:nrow(y_predicted),]))),digits=2)
r2<-round((cor(y_predicted[2:nrow(y_predicted),], y_actual[2:nrow(y_actual),]) ^ 2),digits=2)

print(paste("Values for MARS regression with cross vaidation on ",OUTPUT_FIELD,":  ","MAE = ",mae,", RMSE = ",RMSE, ", R2 = ",r2))



end<-Sys.time()
print("Regression was Done in:")
print(end - start)

readline(prompt = "Press [enter] to continue")
graphics.off()
