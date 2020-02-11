# Project Aim:
- The aim of this project was to analyse car accidents that have occurred in the UK in the
period 2005-2015 to give insight into the question: "How can the UK government use
its collected data to improve road safety?"
- This analysis has been conducted in a group format (Dataholics) for Practical Business Analytics, a module provided by the University of Surrey. The project was conducted within the span of 3 weeks.

# Description of files included:
- Predicting_Accidents_Report.pdf
    - The comprehensive report produced by the analysis made via the following R-scripts.
- Functions_Dataholics.R
  - Contains mainly own written functions. Also contains some of the functions written by Prof. Nick F Ryman-Tubb, presented to us in the computer labs.

- main.R
    - Executes the program. Has each needed filename as a global variable. Reads in the functions file, installs the needed packages and executes the different parts of the program.
You will be asked to enter your JAVA_HOME path. On Windows there was a slight issue regarding the file path. Changing the path to “C:/path_to/Java/jre_version” fixes the issue should it occur. 

- Cleaning.R
    - This file reads in the original data files, drops not needed columns and drops entries with NULL values or certain, pre discussed values. Delivers a bar graph for each parameter and compares the distribution of original and cleaned data set’s Accident_Severity. (Steps are described inside the report.)

- Combining_Aggregating.R
    - Changes Junction_Control value based on the value of Junction_Detail. Combines some values of certain columns. Will also deliver a comparison of the Accident_Severity between the original and the cleaned data sets. (Steps are described inside the report.)

- Prediction_Parameter.R
    - You will be asked if you want to execute your own clustering or if you just want to execute the hard-coded clustering we prepared. By typing “y” and pressing enter, you will execute your own k-means cluster calculation for 2 to 15 clusters, 3 times, to plot the variances for the different cluster numbers. After that a k-means cluster will be performed on the local minimum of 8 clusters with 2500 starting points. This will produce a similar cluster to the one we decided for and hard-coded. That will be executed afterwards.
- TimeAnalysis_Hyp2.R
    - Executes the analysis on the time data (Hypothesis 2 of the report)

- AgeAnalysis_Hyp3.R
    - To run k-means clustering for different values of k which increases run time significantly (total runtime ~10 mins) first ensure the global variable “Multi_k_Plot” is set to TRUE. Then type “y” when prompted. To disable this set “Multi_k_plot” to FALSE please press ENTER when prompted.

-  EnvironmentAnalysis_Hyp1_Regression.R
    - Executes a multivariate linear regression, a mars regression and a mars regression with cross validation of both the Accident_Severity and the Custom_Severity, created in Prediction_Parameter.R
