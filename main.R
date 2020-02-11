# ********************************************************************************************
#  PRATICAL BUSINESS ANALYTICS 2019
#  Dataholics - main.R
#
# 'names'
#
# 21 NOVEMBER 2019
# 
# UPDATE
# 1.00      21/11/2019    Initial Version - Hannes, Aiden, Ben, Konstantinos, Lux, Victor, Ben
# 1.01      
# 1.02      Victor: - added some variables to comfortly use the script between the team members' different laptops
#                   - added pacman to load libraries
# 1.03      02/12/2019   Aiden   - Updated JAVA_HOME PATH to fix error
# *********************************************************************************************
# 
# *********************************************************************************************
rm(list = ls())
cat("\014")


FUNCTIONS <- "Functions_Dataholics.R"

CLEANING <- "Cleaning.R"
AGGREGATING <- "Combining_Aggregating.R"
PREDICTION_PARAMETER <- "Prediction_Parameter.R"

AGE_ANALYSIS <- "AgeAnalysis_Hyp3.R"
TIME_ANALYSIS <- "TimeAnalysis_Hyp2.R"
ENV_ANALYSIS_REGRESSION <- "EnvironmentAnalysis_Hyp1_Regression.R"
ENV_ANALYSIS_DL_DT <- "EnvironmentAnalysis_Hyp1_DL_DT.R"
ENV_ANALYSIS_SHOW_DL_DT <- "EnvironmentalAnalysis_Hyp1_Show_Results_DL_DT.R"

ACCIDENTS_ORIGINAL <- "Accidents0515.csv"
VEHICLES_ORIGINAL <- "Vehicles0515.csv"


MYLIBRARIES<-c("outliers","corrplot","MASS","pROC","formattable","timeSeries","rnn","stats",
               "caret","PerformanceAnalytics","latex2exp","chron","sqldf","quantmod","h2o",
               "plyr","ggplot2","rsample","earth","vip","pdp","stringr","partykit","sqldf", "ggplot2","plyr",
               "factoextra")


library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

# Set java path
#java_home <- readline(prompt = "Please enter your JAVA_HOME path: ")
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_161")


# Load in our custom functions and functions written by Professor Nick, presented in computer labs
source(FUNCTIONS)


# DATA PRE-PROCESSING
#   - Data Cleaning
#   - Data Combining and Aggregating
#   - Clustering, Creating Prediction Parameter
#
source(CLEANING)
source(AGGREGATING)
input <- readline(prompt="Do you want to skip plotting k-means cluster variances, then type nothing. If you want to plot them and create an own cluster, type y. Press [enter]: ")
if (input == "y"){
  execute <- TRUE  
} else {
  execute <- FALSE
}
source(PREDICTION_PARAMETER)


# Execute Time Analyis
source(TIME_ANALYSIS)

# Execute age analysis
input <- readline(prompt="Do you want to skip plotting k-means clustering, then type nothing. If you want to enable plotting k-means clustering, type y. Press [enter]: ")
if (input == "y"){
  Multi_k_Plot <- TRUE  
} else {
  Multi_k_Plot <- FALSE
}
source(AGE_ANALYSIS)


input <- readline(prompt="Do you want to run regression for only a portion of data, then type nothing. If you want to run it for the whole dataset, type y. Press [enter]: ")
if (input == "y"){
  whole <- TRUE  
} else {
  whole <- FALSE
}
# Execute Regression Analysis on Vanilla Accident_Severity
DATASET_FILENAME <- "accidents_clean_old_severity.csv"
OUTPUT_FIELD     <- "Accident_Severity"
source(ENV_ANALYSIS_REGRESSION)

# Execute Regression Analysis on New 
DATASET_FILENAME <- "accidents_clean_new_severity.csv"
OUTPUT_FIELD     <- "Custom_Severity"
source(ENV_ANALYSIS_REGRESSION)


print("Thank you")
print("End of main")
