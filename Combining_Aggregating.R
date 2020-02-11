# ********************************************************************************************
#  PRATICAL BUSINESS ANALYTICS 2019
#  Dataholics - combining_aggregating.R
#
#
# 21 NOVEMBER 2019
# 
# UPDATE
# 1.00      21/11/2019    Initial Version - Hannes, Aiden, Ben, Konstantinos, Lux, Victor, Ben
# 1.01      
# 1.02      Victor: added writing of final csv
# 1.03      Hannes
# 1.10      Hannes: 30/11/2019  Merging with main
# *********************************************************************************************
# 
# *********************************************************************************************
#
start<-Sys.time()
print("Start Data Aggregation")

# Aggregating Junction_Control in the intended way, build on an understanding of the data 
accidents_clean$Junction_Control<-ifelse(accidents_clean$Junction_Control == -1 & accidents_clean$Junction_Detail == 0, 0, accidents_clean$Junction_Control)
accidents_clean<-drop_minus_one_nulls(accidents_clean, names(accidents_clean))


# Aggregating Roadtypes
accidents_clean$Road_Type<-ifelse(accidents_clean$Road_Type == 1, 1,
                                  ifelse(accidents_clean$Road_Type == 2, 2,
                                         ifelse(accidents_clean$Road_Type == 3, 3,
                                                ifelse(accidents_clean$Road_Type == 6, 4,
                                                       ifelse(accidents_clean$Road_Type == 7, 5,
                                                              ifelse(accidents_clean$Road_Type == 12, 6,
                                                                     ifelse(accidents_clean$Road_Type == 9, 7,
                                                                            ifelse(accidents_clean$Road_Type == 1 & accidents_clean$Junction_Detail == 0, 7, -1))))))))


# Aggregating Speed_Limit
accidents_clean$Speed_limit<-ifelse(accidents_clean$Speed_limit == 10, 1, 
                                    ifelse(accidents_clean$Speed_limit == 15, 2, 
                                           ifelse(accidents_clean$Speed_limit == 20, 3, 
                                                  ifelse(accidents_clean$Speed_limit == 30, 4, 
                                                         ifelse(accidents_clean$Speed_limit == 40, 5, 
                                                                ifelse(accidents_clean$Speed_limit == 50, 6, 
                                                                       ifelse(accidents_clean$Speed_limit == 60, 7, 
                                                                              ifelse(accidents_clean$Speed_limit == 70, 8, -1))))))))


# Aggregating Junction_Detail
accidents_clean$Junction_Detail<-ifelse(accidents_clean$Junction_Detail == 0, 0, 
                                        ifelse(accidents_clean$Junction_Detail == 1, 1, 
                                               ifelse(accidents_clean$Junction_Detail == 2, 2, 
                                                      ifelse(accidents_clean$Junction_Detail == 3, 3, 
                                                             ifelse(accidents_clean$Junction_Detail == 5, 4, 
                                                                    ifelse(accidents_clean$Junction_Detail == 6, 5, 
                                                                           ifelse(accidents_clean$Junction_Detail == 7, 6, 
                                                                                  ifelse(accidents_clean$Junction_Detail == 8, 7, -1))))))))


# Aggregatin Pedestrian_Crossing.Physical_Facilities
accidents_clean$Pedestrian_Crossing.Physical_Facilities<-ifelse(accidents_clean$Pedestrian_Crossing.Physical_Facilities == 0, 0,
                                                                ifelse(accidents_clean$Pedestrian_Crossing.Physical_Facilities == 1, 1,
                                                                       ifelse(accidents_clean$Pedestrian_Crossing.Physical_Facilities == 4, 2,
                                                                              ifelse(accidents_clean$Pedestrian_Crossing.Physical_Facilities == 5, 3,
                                                                                     ifelse(accidents_clean$Pedestrian_Crossing.Physical_Facilities == 7, 4,
                                                                                            ifelse(accidents_clean$Pedestrian_Crossing.Physical_Facilities == 8, 5, -1))))))


# Aggregating Light_Conditions
accidents_clean$Light_Conditions<-ifelse(accidents_clean$Light_Conditions == 1, 1, 
                                         ifelse(accidents_clean$Light_Conditions == 4, 2, 
                                                ifelse(accidents_clean$Light_Conditions == 5 | accidents_clean$Light_Conditions == 6 , 3, 
                                                       ifelse(accidents_clean$Light_Conditions == 7, 4, -1))))


# Aggregating Special_Conditions_at_Site
accidents_clean$Special_Conditions_at_Site<-ifelse(accidents_clean$Special_Conditions_at_Site == 0, 0, 
                                                   ifelse(accidents_clean$Special_Conditions_at_Site == 1 | accidents_clean$Special_Conditions_at_Site == 2, 1, 
                                                          ifelse(accidents_clean$Special_Conditions_at_Site == 3, 2, 
                                                                 ifelse(accidents_clean$Special_Conditions_at_Site == 4, 3, 
                                                                        ifelse(accidents_clean$Special_Conditions_at_Site == 5, 4, 
                                                                               ifelse(accidents_clean$Special_Conditions_at_Site == 6 | accidents_clean$Special_Conditions_at_Site == 7, 5, -1))))))


# Aggregating Carriageway_Hazards
accidents_clean$Carriageway_Hazards<-ifelse(accidents_clean$Carriageway_Hazards == 0, 0, 
                                            ifelse(accidents_clean$Carriageway_Hazards == 1 | accidents_clean$Carriageway_Hazards == 2, 1, 
                                                   ifelse(accidents_clean$Carriageway_Hazards == 3 , 2, 
                                                          ifelse(accidents_clean$Carriageway_Hazards == 4 | accidents_clean$Carriageway_Hazards == 5 | accidents_clean$Carriageway_Hazards == 7, 3, 
                                                                 ifelse(accidents_clean$Carriageway_Hazards == 6, 4, -1)))))


print(paste("After the aggregating we keep",nrow(accidents_clean),"Accidents of the original",nrow(accidents_original),"Accidents.", sep = " "), main = "Accidents Original")
print(paste("That is",((nrow(accidents_clean) / nrow(accidents_original))*100),"%",sep = " "))

plot_parameters(accidents_original[,(names(accidents_original) %in% c("Accident_Index","Accident_Severity"))], main = "Accidents Original")
plot_parameters(accidents_clean[,c("Accident_Index","Accident_Severity")], main = "Accidents Clean")


end<-Sys.time()
print("Combining and Aggregating was Done in:")
print(end - start)

readline(prompt = "Press [enter] to continue. Warning, that will delete the graphs of this section")
graphics.off()
