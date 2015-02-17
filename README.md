# Getting-and-Cleaning-Data-Project

#Author: Stephen Dimig
#Description:
One of the most exciting areas in all of data science right now is 
wearable computing - see for example this article . Companies like Fitbit, 
Nike, and Jawbone Up are racing to develop the most advanced algorithms 
to attract new users. The data linked to from the course website represent
data collected from the accelerometers from the Samsung Galaxy S 
smartphone.

The run_analysis.R scipt examines data collected from a Samsung smartphone
while people were exercising and does the following. 
1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for 
    each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data 
   set with the average of each variable for each activity and each 
   subject.

#How To
The first thing you need to do to run the run_analysis() scriptt is to source the file into your R environment:

> source("run_analysis.R")

You can run the script without downloading or unzipping the data if you choose to run_analysis() will do that automatically if the data directory does not exist.

> df <- run_analysis()

The first time you run it you will see out out from the download and uncompressing of the data. After you run it, df will contain the tidy data. You can also get a summary of the data by subject using the data.frame object from abve.

> bysub <- summary_by_subject(df)

This will return the average of all the variables for each subject. You can also get a summary by activity (ie; 1 WALKING, WALKING_UPSTAIRS, 
WALKING_DOWNSTAIRS, SITTING, STANDING, or LAYING).

> byact <- summary_by_activity(df)


