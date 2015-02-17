
run_analysis <- function(directory = "UCI HAR Dataset")
{
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
    
    finalSet <- data.frame()
    current = getwd()
    if(!file.exists(directory))
    {
        download.file(url, zipFileName, method="curl")
        unzip(zipFileName)
    }
    
    if(file.exists(directory))
    {
        setwd(directory)
        columnVec <- getColumnVector(featuresSetName)
        if(is.null(columnVec))
        {
            message(sprintf("Error: could not process feature set; name=%s", featuresSetName))
        }
        else
        {
            mergedSet <- getMergedSet(columnVec, trainingSetName, testSetName)
            if(is.null(mergedSet))
            {
                message(sprintf("Error: could not process training and test sets; trainingSet=%s; testSet=%s", trainingSetName, testSetName))
            }
            else
            {
                activities <- getActivities(activityLabelsName, trainingActivityName, testActivityName)
                if(is.null(activities))
                {
                    message(sprintf("Error: could not process training and test sets; trainingActivity=%s; testActivity=%s; activityLabels=%s", trainingActivityName, testActivityName, activityLabelsName))
                }
                else
                {
                    subjects <- getSubjects(trainingSubjectName, testSubjectName)
                    if(is.null(subjects))
                    {
                        message(sprintf("Error: could not process training subject and test subject sets; trainingSubject=%s; testSubject=%s", trainingSubjectName, testSubjectName))
                    }
                    else
                    {
                        finalSet <- cbind(subjects, activities, mergedSet)
                    }
                }
            }
        }
    }
    setwd(current)
    finalSet
}

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
        xfactor <- factor(activities[, 1], tolower(activityLabels[,2]))
    }
    xfactor
}

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

summary_by_subject <- function(mydf = data.frame())
{
    summary <- data.frame()
    summary <- mydf[, !names(mydf) %in% c("activities")] %>% group_by(subject) %>% summarise_each(funs(mean))
    summary
}

summary_by_activity <- function(mydf = data.frame())
{
    summary <- data.frame()
    summary <- mydf[, !names(mydf) %in% c("subject")] %>% group_by(activities) %>% summarise_each(funs(mean))
    summary
}

write_data<- function(fileName, mydf = data.frame())
{
    write.table(mydf, file=fileName, row.name=FALSE)
}

