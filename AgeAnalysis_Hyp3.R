# 2019 PRATICAL BUSINESS ANALYTICS COURSEWORK
# 
# 25th November 2019
#
# UPDATE
# 1.00      25/11/2019  Initial Version - Aiden
# 1.10      27/11/2019  Added global variable Multi_k_plot - Aiden
# 1.11      30/11/2019  Hannes - Merging with main
# ************************************************
# ************************************************
# clears all objects in "global environment"

#*************************************************
# Global variables

OUTLIER_CUTOFF    <- 0.99                 # Confidence p-value for outlier detection
CUTOFF_DISCREET   <- 5                    # Number of empty bins to determine discreet
CUTOFF_REDUNDANT  <- 0.95                 # Linear correlation coefficient cut-off
MAX_LITERALS      <- 50 
OUTPUT_FIELD      <- "Age_of_Driver"
NUMBER_OF_CLUSTER <- c(3,4,5,7,8)         # Values to print for k number of clusters (workaround as Elbow method / Silhouette method not working)
Multi_k_Plot      <- FALSE                # Set this to FALSE to disable plotting k-means clustering for different values of k and thus save runtime


MYLIBRARIES<-c("sqldf", "ggplot2","plyr")

# ************************************************
# main() :
# main entry point to execute ML data analytics
#
# INPUT: None
# OUTPUT :None
# ************************************************
main<-function(){
  
  #Load or Read Datasets
  if (exists("accidents_original")){
    accidents <- accidents_original
  } else {
    accidents <- read_csv(csvFilename = ACCIDENTS_ORIGINAL )
  } 
  if (exists("vehicles_original")){
    vehicles <- vehicles_original
  } else {
    vehicles <- read_csv(csvFilename = VEHICLES_ORIGINAL )
  }
  

  # Make Accident Index column names the same for each dataset 
  names(accidents)[1] <- "Accident_Index" 
  names(vehicles)[1] <- "Accident_Index"
  
  #Remove nulls from vehicles
  
  #Columns to keep from Vehicles
  vehicles <- subset(vehicles, select = c(Accident_Index,
                                          Age_of_Driver,
                                          Age_of_Vehicle))
  
 vehicles <- drop_minus_one_nulls(vehicles, cols = names(vehicles))
 
  #Merge datasets
  accidents_vehicles <- merge(accidents,vehicles,by="Accident_Index")
  
  
  # Remove Accident columns we do not need
    column_names <- c("Accident_Index","Longitude","Latitude","Location_Easting_OSGR","Location_Northing_OSGR",
                    "Date","Time","Day_of_Week","X1st_Road_Number","Did_Police_Officer_Attend_Scene_of_Accident",
                    "LSOA_of_Accident_Location","Local_Authority_.Highway.","X2nd_Road_Number",
                    "Local_Authority_.District.","Pedestrian_Crossing.Human_Control",
                    "Pedestrian_Crossing.Physical_Facilities","X2nd_Road_Class","X1st_Road_Class",
                    "Special_Conditions_at_Site","Carriageway_Hazards",
                    "Junction_Control", "Junction_Detail", "Number_of_Casualties")
  
    accidents_vehicles <- drop_and_format_date(accidents_vehicles, column_names = column_names)
  
  # Select data in London Only
  Clustering_Dataset_1 <- sqldf("select * from accidents_vehicles
                                           where Police_Force in (1,48)")
  
  
  #Drop Police column - use these dataframes for modelling!!
  Clustering_Dataset_1_clean <- Clustering_Dataset_1[,-1]
  
  
  # pre-process data
  # The first dataset analysis considers London 
  print("Starting Pre-processing for first Clustering Analysis (London)")
  
  Ordinal_ML_Dataset_1 <- Hypothesis_3_Preprocess_ordinal(Clustering_Dataset_1_clean)
  Categorical_ML_Dataset_1 <- Hypothesis_3_Preprocess_categorical(Clustering_Dataset_1_clean)
  Combined_ML_Dataset_1 <- Hypothesis_3_Preprocess_CombinedDatasets(Ordinal_ML_Dataset_1,
                                                                    Categorical_ML_Dataset_1)
  
  # Remove the target field
  Output_field <-which(names(Combined_ML_Dataset_1)==OUTPUT_FIELD)
  predictors_Dataset_1 <-Combined_ML_Dataset_1[,-Output_field]
  
  # ************************************************
  # Clustering k-means - plot clusters for k=1 to k=10
  # Due to computing power cannot run Elbow Method and Silhouette Method for
  # Combined_ML_Dataset_1
  # The for loop takes about 10 mins to run and is demonstrated below to show that the value for k
  # was not just randomly chosen! Please comment out if not needed
  set.seed(234)
  
  if (Multi_k_Plot == TRUE) {
  
   print("Plotting Clusters for different values of k for Clustering_Dataset_1")
   
    for (i in NUMBER_OF_CLUSTER) {
    
     modelKmeans <- kmeans(predictors_Dataset_1, centers = i, nstart=25)
     plot <- factoextra::fviz_cluster(modelKmeans, data = predictors_Dataset_1,geom = "point")
     print(plot)
    
    }
  } 
  
  # Select optimal K-means cluster
  
  modelKmeans_Dataset_1 <- kmeans(predictors_Dataset_1, centers=6, nstart=25)
  
  # Output k-means result
  print("K means Results")
  
  print(str(modelKmeans_Dataset_1))
  
  print("Optimal Number of Clusters = 6")
  kplot_Dataset_1 <- factoextra::fviz_cluster(modelKmeans_Dataset_1, data = predictors_Dataset_1,geom = "point")
  print(kplot_Dataset_1)
  
  
  #******************************************************************************
  # Kmeans clustering for second dataset 
  # This dataset considers London where Accident Severity is either Fatal or Serious
  print("Starting Preprocessing for Second Clustering Analysis (London with Accident Severity 1 and 2)")
  
  # Select data in London with Accident Severity 1 and 2  
  Clustering_Dataset_2 <- sqldf("select * from accidents_vehicles
                                           where Accident_Severity IN (1,2) and
                                                 Police_Force IN (1,48)")
  
  
  #Drop Police column - use these dataframes for modelling!!
  Clustering_Dataset_2_clean <- Clustering_Dataset_2[,-1]
  
  # pre-process data
  
  Ordinal_ML_Dataset_2 <- Hypothesis_3_Preprocess_ordinal(Clustering_Dataset_2_clean)
  Categorical_ML_Dataset_2 <- Hypothesis_3_Preprocess_categorical(Clustering_Dataset_2_clean)
  Combined_ML_Dataset_2 <- Hypothesis_3_Preprocess_CombinedDatasets(Ordinal_ML_Dataset_2,
                                                                    Categorical_ML_Dataset_2)
  
  # Remove the target field
  Output_field <-which(names(Combined_ML_Dataset_2)==OUTPUT_FIELD)
  predictors_Dataset_2 <-Combined_ML_Dataset_2[,-Output_field]
  
  # ************************************************
  # Clustering k-means - plot clusters for k=1 to k=10 
  # Due to computing power cannot run Elbow Method, but Silhouette Method works for 
  # Combined_ML_Dataset_2
  
  if (Multi_k_Plot == TRUE) {
    
    print("Plotting Clusters for different values of k for Clustering_Dataset_2")
  
   for (i in NUMBER_OF_CLUSTER) {
     
     modelKmeans <- kmeans(predictors_Dataset_2, centers = i, nstart=25)
     plot <- factoextra::fviz_cluster(modelKmeans, data = predictors_Dataset_2,geom = "point")
     print(plot)
     
   }
  }
  
  # Final kmeans plot to use 
  # Select optimal K-means cluster
  modelKmeans_Dataset_2 <- kmeans(predictors_Dataset_2, centers=3, nstart=25)
  
  # Output k-means result
  print(str(modelKmeans_Dataset_2))
  
  print("Optimal Number of Clusters = 3")
  
  kplot_Dataset_2 <- factoextra::fviz_cluster(modelKmeans_Dataset_2, 
                                              data = predictors_Dataset_2,
                                              geom = "point")
  print(kplot_Dataset_2)
  
 
} #endof main()

gc() #release memory

# clear plots and other graphics
if(!is.null(dev.list())) dev.off()
graphics.off()

# This clears all warning messages
assign("last.warning", NULL, envir = baseenv())

# clears the console area
cat("\014")

print("STARTING Hypothesis 3 - K-means Clustering for Age of Driver in Accidents")
start<-Sys.time()

library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)


set.seed(456)

main()

print("end")

warnings()

# 

end<-Sys.time()
print("Age Analysis was Done in:")
print(end - start)

readline(prompt = "Press [enter] to continue. Warning, that will delete the graphs of this section")
graphics.off()