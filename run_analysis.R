#################################################################
##
## File:   run_analysis.R
## Author: Stephen Dimig
## Description:
## One of the most exciting areas in all of data science right now is 
## wearable computing - see for example this article . Companies like Fitbit, 
## Nike, and Jawbone Up are racing to develop the most advanced algorithms 
## to attract new users. The data linked to from the course website represent
## data collected from the accelerometers from the Samsung Galaxy S 
## smartphone.
##
## The run_analysis.R scipt examines data collected from a Samsung smartphone
## while people were exercising and does the following. 
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for 
##    each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data 
##    set with the average of each variable for each activity and each 
##    subject.
##
#################################################################

#################################################################
##
## Description:
## This method is the to level method for the run_analysis() script.
## It invokes verious helper methods in order to create a tidy data set from 
## the Samsung phone data.
##
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for 
##    each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
##
## Parameters:
## directory [in] - This optional parameter is used to specify directory
## where the Samsung phone data is stored. If the directory does not exist, 
## the data is downloaded and unzipped from the provided url.
##
## Return:
## The tidy data set as described above is returned in a dataframe.
##
#################################################################
run_analysis <- function(directory = "UCI HAR Dataset")
{
    # Constant declarations
    featuresSetName <- "features.txt"
    trainingSetName <- "train/X_train.txt"
    testSetName <- "test/X_test.txt"
    trainingActivityName <- "train/Y_train.txt"
    testActivityName <- "test/Y_test.txt"
    activityLabelsName <- "activity_labels.txt"
    trainingSubjectName <- "train/subject_train.txt"
    testSubjectName <- "test/subject_test.txt"
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    zipFileName <- "UCIHARDataset.zip"
    
    # Initializations
    finalSet <- data.frame()
    current = getwd()
    
    # Check to see if phone data directory exists.
    if(!file.exists(directory))
    {
        # If not, download and unzip the phone data.
        download.file(url, zipFileName, method="curl")
        unzip(zipFileName)
    }
    
    # Phone data should exist now, but check just in case of failuere
    if(file.exists(directory))
    {
        # Change to phone data directory
        setwd(directory)
        
        # Get the vector of column names derived from the features.txt file.
        columnVec <- getColumnVector(featuresSetName)
        if(is.null(columnVec))
        {
            message(sprintf("Error: could not process feature set; name=%s", featuresSetName))
        }
        else
        {
            # Get the merged training and test set data
            mergedSet <- getMergedSet(columnVec, trainingSetName, testSetName)
            if(is.null(mergedSet))
            {
                message(sprintf("Error: could not process training and test sets; trainingSet=%s; testSet=%s", trainingSetName, testSetName))
            }
            else
            {
                # Get the activities column data
                activities <- getActivities(activityLabelsName, trainingActivityName, testActivityName)
                if(is.null(activities))
                {
                    message(sprintf("Error: could not process training and test sets; trainingActivity=%s; testActivity=%s; activityLabels=%s", trainingActivityName, testActivityName, activityLabelsName))
                }
                else
                {
                    # Get the subject column data
                    subjects <- getSubjects(trainingSubjectName, testSubjectName)
                    if(is.null(subjects))
                    {
                        message(sprintf("Error: could not process training subject and test subject sets; trainingSubject=%s; testSubject=%s", trainingSubjectName, testSubjectName))
                    }
                    else
                    {
                        # Finally merge the subjects, activities and merged data.
                        finalSet <- cbind(subjects, activities, mergedSet)
                    }
                }
            }
        }
    }
    # Set the working directory back to where it was.
    setwd(current)
    
    # Return the final set of data
    finalSet
}

#################################################################
##
## Description:
## This method gets a vector of column names based on the features.txt file.
## 1. All variable names are converted to lower case.
## 2. All "-" are removed from variable names to make it easier to manipulate
##    the data in R. A "-" is interpreted as a minus sign. An underbar is
##    used instead.
## 3. All parenthesis are removed from the variable names.
## 4. All spaces are removed from the variable names.
##
## Parameters:
## featureSetName [in] - This optional parameter specifies the name of the 
# file that is used to derive column names.
##
## Return:
## A vector of column names is returned 
##
#################################################################
getColumnVector <- function(featuresSetName = "features.txt")
{
    columnVec <- NULL
    if(file.exists(featuresSetName))
    {
        columnVec < vector()
        columnNames <- read.table(featuresSetName)
        columnVec <- as.vector(columnNames[, 2])
        columnVec <- tolower(columnVec)
        columnVec <- gsub(" ", "", columnVec)
        columnVec <- gsub("-", "_", columnVec)
        columnVec <- gsub("\\(\\)", "", columnVec)
    }
    columnVec
}

#################################################################
##
## Description:
## This method returns the merged training and test data sets with only the
## variables that have to do with mean or standard deviation. The angle()
## columns have been removed because they do not actually measure a mean
## or standard deviation.
##
## Parameters:
## columnVec [in] - A vector of column names
## trainingSetName [in] - The name of the file that contains the training set
## data.
## testSetName [in] - The name of the file that contains the test set data.
##
## Return:
## A data.frame that includes the merged data with only measures of mean or
## standard deviation is returned.
##
#################################################################
getMergedSet <- function(columnVec, trainingSetName = "train/Y_train.txt", testSetName = "test/X_test.txt")
{
    mergedSet = NULL
    if(file.exists(trainingSetName) && file.exists(testSetName))
    {
        mergedSet <- data.frame()
        trainingSet <- read.table(trainingSetName)
        testSet <- read.table(testSetName)
        mergedSet <- rbind(trainingSet, testSet)
        names(mergedSet) <- columnVec
        vec <- columnVec
        mergedSet <- mergedSet[, grepl("std|mean", columnVec)]
        vec <- columnVec[grepl("std|mean", columnVec)]
        mergedSet <- mergedSet[, !grepl("^angle.*$", vec)]
    }
    mergedSet
}

#################################################################
##
## Description:
## This method returns the column of activities as a factor with the values 
## for 1-6 replaced by meaningful text desriptions.
##
## Parameters:
## activityLabelsName [in] - The name of the file that contains the labels 
## for the activities. This file is a mapbetween a number and a description.
## trainingActivityName [in] - This is the name of the file that contains the 
## list of activities for each row of the training data.
## testActivityName [in] - This is the name of the file that contains the 
## list of activities for each row of the test data.
## Return:
## A data.frame that includes the column data of the activity for each row of
## the merged test/training data. The integer values of 1-6 are replaced with
## meaningful descriptions from the activity labels file.
##
#################################################################
getActivities <- function(activityLabelsName = "activity_labels.txt", trainingActivityName = "train/Y_train.txt", testActivityName = "test/Y_test.txt")
{
    xfactor <- NULL
    if(file.exists(activityLabelsName) &&
       file.exists(trainingActivityName) && 
       file.exists(testActivityName))
    {
        activities <- data.frame()
        activityLabels <- read.table(activityLabelsName)
        trainingActivity <- read.table(trainingActivityName)
        testActivity <- read.table(testActivityName)
        
        # I could not figure out how to do this in a more R way so this
        # looks like c++. It works though.
        for(i in 1:nrow(trainingActivity)) 
        {
            activity <- activityLabels[trainingActivity[i, 1], 2]
            activities[i, 1] <- tolower(activity)
        }
        
        for(i in 1:nrow(testActivity)) 
        {
            activity <- activityLabels[testActivity[i, 1], 2]
            activities[i + nrow(trainingActivity), 1] <- tolower(activity)
        }
        
        # The factor is created from the activity strings from each row and
        # their labels from the activity labels file.
        xfactor <- factor(activities[, 1], tolower(activityLabels[,2]))
    }
    xfactor
}

#################################################################
##
## Description:
## This method returns the column of subjects. Since there is no mapping to
## anything more descriptive. The values are left as integers 1-30.
##
## Parameters:
## trainingSubjectName [i] - The name of the file that contains subject for each
## entry of the training set data.
## testSubjectName [i] - The name of the file that contains subject for each
## entry of the test set data.
## Return:
## A data.frame that includes the column data of the subject for each row of
## the merged test/training data. 
##
#################################################################
getSubjects <- function(trainingSubjectName = "train/subject_train.txt", testSubjectName = "test/subject_test.txt") 
{
    subjects <- NULL
    if(file.exists(trainingSubjectName) && file.exists(testSubjectName))
    {
        subjects <- data.frame()
        trainingSubjects <- read.table(trainingSubjectName)
        testSubjects <- read.table(testSubjectName)
        subjects <- rbind(trainingSubjects, testSubjects)
        names(subjects) <- c("subject")
    }
    subjects
}

#################################################################
##
## Description:
## This method returns a summary of the average of all of the different 
## varibales by subject. 
##
## Parameters:
## mydf [in] - The data frame that contains the final output of the tidy
## data returned by run_analysis().
##
## Return:
## A data.frame that includes a summary of the average of all of the different 
## varibales by subject. 
##
#################################################################
summary_by_subject <- function(mydf = data.frame())
{
    summary <- data.frame()
    summary <- mydf[, !names(mydf) %in% c("activities")] %>% group_by(subject) %>% summarise_each(funs(mean))
    summary
}

#################################################################
##
## Description:
## This method returns a summary of the average of all of the different 
## varibales by activity. 
##
## Parameters:
## mydf [in] - The data frame that contains the final output of the tidy
## data returned by run_analysis().
##
## Return:
## A data.frame that includes a summary of the average of all of the different 
## varibales by activity. 
##
#################################################################
summary_by_activity <- function(mydf = data.frame())
{
    summary <- data.frame()
    summary <- mydf[, !names(mydf) %in% c("subject")] %>% group_by(activities) %>% summarise_each(funs(mean))
    summary
}

#################################################################
##
## Description:
## This method is a small helper method that writes a data.frame() to
## a file.
##
## Parameters:
## mydf [in] - The data frame to write to a file.
##
## Return:
## None 
##
#################################################################
write_data<- function(fileName, mydf = data.frame())
{
    write.table(mydf, file=fileName, row.name=FALSE)
}

