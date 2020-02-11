# ********************************************************************************************
#  PRATICAL BUSINESS ANALYTICS 2019
#  Dataholics - cleaning.R
#
# 'names'
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
# 1.04      Hannes: 30/11/2019  Merging with main
# *********************************************************************************************
# 
# *********************************************************************************************
start<-Sys.time()
print("Start Data Cleaning")


# Load in the original data set
accidents_original<-read_csv(ACCIDENTS_ORIGINAL)
vehicles_original<-read_csv(VEHICLES_ORIGINAL)
names(accidents_original)[1] <- "Accident_Index"

# Columns we do not use in our analysis and therefore we want to drop in different data sets
remove_cols_time <- c("Location_Easting_OSGR", "Location_Northing_OSGR", "Local_Authority_.Highway.", "X1st_Road_Class","X1st_Road_Number", 
                      "X2nd_Road_Class", "X2nd_Road_Number", "Road_Type", "Speed_limit","Did_Police_Officer_Attend_Scene_of_Accident", 
                      "LSOA_of_Accident_Location", "Longitude", "Latitude","Junction_Detail","Junction_Control","Pedestrian_Crossing.Human_Control",
                      "Pedestrian_Crossing.Physical_Facilities","Light_Conditions","Weather_Conditions","Road_Surface_Conditions","Special_Conditions_at_Site",
                      "Carriageway_Hazards","Urban_or_Rural_Area")

# Preparing and Cleaning data set for time analysis
accidents_clean_time<-drop_and_format_date(accidents_original, remove_cols_time)
accidents_clean_time<-subset(accidents_clean_time , accidents_clean_time$Time != '')
accidents_clean_time$timestamp<-paste(accidents_clean_time$Date, accidents_clean_time$Time)
accidents_clean_time$timestamp<-as.chron(accidents_clean_time$timestamp, format = "%Y-%m-%d %H:%M")


# Columns we do not use in our analysis and therefore we want to drop in different data sets
remove_cols_accidents <- c("Location_Easting_OSGR", "Location_Northing_OSGR", "Local_Authority_.Highway.", "X1st_Road_Class", 
                           "X1st_Road_Number", "X2nd_Road_Class", "X2nd_Road_Number", "Police_Force", 
                           "Did_Police_Officer_Attend_Scene_of_Accident", "LSOA_of_Accident_Location", "Date","Day_of_Week",
                           "Time", "Longitude", "Latitude", "Local_Authority_.District.")


# Droppping the previously stated columns in our data set, transferring Date column into date format if data set has a date column
accidents_clean<-drop_and_format_date(accidents_original, remove_cols_accidents)


# Explore the Data Set
plot_parameters(accidents_clean)


# Dropping null rows in accidents, apart from Junction_Control (we got that from a deeper analysis of the data presented to us)
accidents_clean <- drop_minus_one_nulls(accidents_clean, c("Accident_Index","Accident_Severity","Number_of_Vehicles","Number_of_Casualties","Road_Type",
                                                           "Speed_limit","Junction_Detail","Pedestrian_Crossing.Human_Control","Pedestrian_Crossing.Physical_Facilities",
                                                           "Light_Conditions","Weather_Conditions","Road_Surface_Conditions","Special_Conditions_at_Site",
                                                           "Carriageway_Hazards","Urban_or_Rural_Area"))

# From a deeper look at the data we decided to drop certain values since their meaning is something related to unknown
#and the proportion of data they display is really small
#
# Dropping rows with 'Unknown' as Road_Type
accidents_clean <- subset(accidents_clean, accidents_clean$Road_Type != 9)

# Dropping rows with 'Unallocated' as Urban_or_Rural_Area
accidents_clean <- subset(accidents_clean, accidents_clean$Urban_or_Rural_Area != 3)

# Dropping rows with 'Other' and 'Unknown' and Weather_Condition
accidents_clean <- subset(accidents_clean, accidents_clean$Weather_Conditions != 9)
accidents_clean <- subset(accidents_clean, accidents_clean$Weather_Conditions != 8)

# Dropping rows with 0 as Speed_Limit
accidents_clean <- subset(accidents_clean, accidents_clean$Speed_limit != 0)

# Dropping rows with 'Other Junction' as Junction_Detail
accidents_clean <- subset(accidents_clean, accidents_clean$Junction_Detail != 9)


# Cleaning is Done
#
# Take a look at the cleaned data
print(paste("After the cleaning we keep",nrow(accidents_clean),"Accidents of the original",nrow(accidents_original),"Accidents.", sep = " "), main = "Accidents Original")
print(paste("That is",((nrow(accidents_clean) / nrow(accidents_original))*100),"%",sep = " "))

plot_parameters(accidents_original[,(names(accidents_original) %in% c("Accident_Index","Accident_Severity"))], main = "Accidents Original")
plot_parameters(accidents_clean[,c("Accident_Index","Accident_Severity")], main = "Accidents Clean")


write.csv(accidents_clean_time, 'accidents_clean_time.csv',row.names=FALSE)

end<-Sys.time()
print("Cleaning was Done in:")
print(end - start)

readline(prompt = "Press [enter] to continue. Warning, that will delete the graphs of this section")
graphics.off()
