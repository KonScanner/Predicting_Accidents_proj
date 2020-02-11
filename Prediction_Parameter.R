# ********************************************************************************************
#  PRATICAL BUSINESS ANALYTICS 2019
#  Dataholics - Prediction_Parameter.R
#
#
# 25 NOVEMBER 2019
# 
# UPDATE
# 1.00      25/11/2019    Initial Version - Hannes
# 1.01    
# 1.10      Hannes: 30/11/2019  Merging with main
# *********************************************************************************************
# 
# *********************************************************************************************
#
#
# PREDICTION-PARAMETER
#
# There are several fields hat might be of interest to predict:
#  - Accident_Severity
#  - Number_of_Casualties
#  - Number_of_Vehicles
#  - Number_of_Fatals
#  - Number_of_Serious
#  - Number_of_Slights
#
# Number_of_Vehicles, and Number_of_Casualties contain a lot of "Outliers" and is in general widely spread. In order to make it more usable for clustering
# we take a look at the values and bin them in self chosen categories
#
start<-Sys.time()
print("Start Clustering to create Prediction Parameter")


# We have many outliers in Number of Vehicles. In order to not loose or corrupt a lot of data we bin the Number_of_Vehicles
accidents_clean$Number_of_Vehicles<-ifelse(accidents_clean$Number_of_Vehicles == 1, 1, 
                                           ifelse(accidents_clean$Number_of_Vehicles == 2, 2, 
                                                  ifelse(accidents_clean$Number_of_Vehicles == 3 | accidents_clean$Number_of_Vehicles == 4, 3, 
                                                         ifelse(accidents_clean$Number_of_Vehicles >= 5 & accidents_clean$Number_of_Vehicles <= 15, 4, 
                                                                ifelse(accidents_clean$Number_of_Vehicles > 15, 5, -1)))))


# We have many outliers in Number of Casualties. In order to not loose or corrupt a lot of data we bin the Number_of_Vehicles
accidents_clean$Number_of_Casualties<-ifelse(accidents_clean$Number_of_Casualties == 1, 1, 
                                             ifelse(accidents_clean$Number_of_Casualties == 2 | accidents_clean$Number_of_Casualties == 3 | accidents_clean$Number_of_Casualties == 4, 2, 
                                                    ifelse(accidents_clean$Number_of_Casualties >= 5 & accidents_clean$Number_of_Casualties <= 10, 3, 
                                                           ifelse(accidents_clean$Number_of_Casualties > 10 & accidents_clean$Number_of_Casualties <= 15, 4, 
                                                                  ifelse(accidents_clean$Number_of_Casualties > 15, 5, -1)))))

# *********************************************************************************************************************************************
# As mentioned in the report, we tried a lot of different Combinations between Accident_Severity, Number_of_Casualties, Number_of_Vehicles
# The most promising results we got from the combination Accident_Severity and Number_of_Vehicles
# 
# Based on that findings we found that the optimal number of clusters is 8 and 
# we hardcoded one of those created clusters with an explanation in the data dictionary.
# The hardcoding of the cluster comes after this part
#
# !CODE!:
# If you want to check the number of clusters from 1 to 15, plot the results
# and create a cluster on your own, then enter y when asked: 
#
# WARNING: It will take a long time
#
# randomise data and pick parameters to combine

if (execute == TRUE){
  accidents_clean<-accidents_clean[order(runif(nrow(accidents_clean))),]
  prediction_parameters<-accidents_clean[,(names(accidents_clean) %in% c("Accident_Severity","Number_of_Vehicles"))]

  # plot three different kmeans for cluster numbers from 1 to 15 against their variances
  for (i in 1:3){
    accidents_clean<-accidents_clean[order(runif(nrow(accidents_clean))),]
    prediction_parameters<-accidents_clean[,(names(accidents_clean) %in% c("Accident_Severity","Number_of_Vehicles"))]
  
    set.seed(76964057)
    variances <- (nrow(prediction_parameters)-1)*sum(apply(prediction_parameters,2,var))
    for (i in 2:15){
      variances[i] <- sum(kmeans(prediction_parameters,centers=i,nstart = 10)$withinss)
    } 
    plot(1:15, variances, type="b", xlab="Number of Clusters",ylab=TeX("Within groups of $\\sigma$\'s"), main = "Variances Vs. Number of Clusters")
  }

  # Execute a kmeans model for 8 clusters and with 2500 starting points, save the new cluster
  accidents_clean<-accidents_clean[order(runif(nrow(accidents_clean))),]
  prediction_parameters<-accidents_clean[,(names(accidents_clean) %in% c("Accident_Severity","Number_of_Vehicles"))]
  set.seed(76964057)
  fit <- kmeans(prediction_parameters, 8, nstart = 2500) 
  new_cluster <- data.frame(accidents_clean, fit$cluster)

  # print out the newly created cluster distribution
  show_cluster<-aggregate(Accident_Index ~ fit.cluster, data = new_cluster, FUN = function(x){NROW(x)} / nrow(new_cluster))
  show_cluster<-show_cluster[order(-show_cluster$Accident_Index),]
  barplot(show_cluster$Accident_Index,
          main = "New Cluster distribution",
          xlab = "Cluster",
          ylab = "No. of Accidents",
          names.arg = show_cluster$fit.cluster,
          col = "darkred",
          horiz = FALSE,
          ylim = c(0,1))
  # End of the clustering
}


# applying our chosen clustering to create a custom_severity.
# Hardcoded to make the results reproducable. 
# To have a go at the clustering, uncomment the part above
accidents_clean$Custom_Severity<-ifelse(accidents_clean$Accident_Severity == 3 & accidents_clean$Number_of_Vehicles == 1, 1, 
                                        ifelse(accidents_clean$Accident_Severity == 3 & accidents_clean$Number_of_Vehicles == 2, 2, 
                                               ifelse(accidents_clean$Accident_Severity == 3 & accidents_clean$Number_of_Vehicles == 3, 3, 
                                                      ifelse(accidents_clean$Accident_Severity == 2 & accidents_clean$Number_of_Vehicles == 1, 4, 
                                                             ifelse(accidents_clean$Accident_Severity == 2 & accidents_clean$Number_of_Vehicles == 2, 5, 
                                                                    ifelse((accidents_clean$Accident_Severity == 3 & accidents_clean$Number_of_Vehicles == 4) |
                                                                           (accidents_clean$Accident_Severity == 2 & accidents_clean$Number_of_Vehicles == 5) | 
                                                                           (accidents_clean$Accident_Severity == 3 & accidents_clean$Number_of_Vehicles == 5), 6, 
                                                                           ifelse((accidents_clean$Accident_Severity == 1 & accidents_clean$Number_of_Vehicles == 1) |
                                                                                  (accidents_clean$Accident_Severity == 1 & accidents_clean$Number_of_Vehicles == 2), 7, 
                                                                                  ifelse((accidents_clean$Accident_Severity == 1 & accidents_clean$Number_of_Vehicles == 3) | 
                                                                                         (accidents_clean$Accident_Severity == 2 & accidents_clean$Number_of_Vehicles == 3) | 
                                                                                         (accidents_clean$Accident_Severity == 1 & accidents_clean$Number_of_Vehicles == 4) | 
                                                                                         (accidents_clean$Accident_Severity == 2 & accidents_clean$Number_of_Vehicles == 4) | 
                                                                                         (accidents_clean$Accident_Severity == 1 & accidents_clean$Number_of_Vehicles == 5), 8, -1))))))))


# Creating two data sets, one to predict the old Accident_Severity and one to predict the new Custom_Severity
#  - accidents_old_severity
#  - accidents_new_severity
#
# Show distribution of old and new severity
plot_parameters(accidents_clean[,(names(accidents_clean) %in% c("Accident_Index","Accident_Severity"))], main = "Accidents Clean Accident Severity")
show_cluster<-aggregate(Accident_Index ~ Custom_Severity, data = accidents_clean, FUN = function(x){NROW(x)} / nrow(accidents_clean))
show_cluster<-show_cluster[order(-show_cluster$Accident_Index),]
barplot(show_cluster$Accident_Index,
        main = "Number of Accidents per Custom_Severity",
        xlab = "Severity",
        ylab = "No. of Accidents",
        names.arg = show_cluster$Custom_Severity,
        col = "darkred",
        horiz = FALSE,
        ylim = c(0,1))

accidents_clean_old_severity<-accidents_clean[,!(names(accidents_clean) %in% c("Accident_Index","Number_of_Casualties","Number_of_Vehicles","Custom_Severity"))]
accidents_clean_new_severity<-accidents_clean[,!(names(accidents_clean) %in% c("Accident_Index","Number_of_Casualties","Number_of_Vehicles","Accident_Severity"))]

write.csv(accidents_clean_old_severity,'accidents_clean_old_severity.csv', row.names=FALSE)
write.csv(accidents_clean_new_severity,'accidents_clean_new_severity.csv', row.names=FALSE)
end<-Sys.time()
print("Combining and Aggregating was Done in:")
print(end - start)

readline(prompt = "Press [enter] to continue. Warning, that will delete the graphs of this section")
rm(accidents_clean)
graphics.off()
