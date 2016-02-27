run_analysis <- function() {
     ##
     ##   The function run_analysis executes all the steps required to analysis and merge the Test and Training
     ##   datasets and convert them into a tidy dataset
     ##
     ##   Course assignment 4
     ##   18 February 2016
     ##   Paul van der Kooy
     ##
     ##   Load relevant libraries and set working directory
     ##
     setwd("/Users/paul/DataScience/Getting and Cleaning data/Assignment4")
     library(data.table)
     library(stringr)
     library(dplyr)
     ##   Set limit of rows to read for development and testing (-1 is read all)
     scope <- -1
     ##
     ##   Copy over the input data to a local folder
     ##
     if(!file.exists("./data"))    {dir.create("./data")}
     fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
     download.file(fileUrl, destfile="./data/datapack.zip")
     ##   Unzip manual
     ##   Register date of download  
     sysdate()
     ##
     ##   Read relevant input data
     ##        Read the Subject (person doing the activity)
     subjectTrainData <- fread("./UCI HAR Dataset/train/subject_train.txt", nrows = scope, header = FALSE, col.names = c("subject"))
     subjectTestData <- fread("./UCI HAR Dataset/test/subject_test.txt", nrows = scope, header = FALSE, col.names = c("subject"))
     ##        Read the movement (registered movements of the subject while doing an activity)
     movementTrainData <- fread("./UCI HAR Dataset/train/x_train.txt", nrows = scope, header = FALSE)
     movementTestData <- fread("./UCI HAR Dataset/test/x_test.txt", nrows = scope, header = FALSE)
     ##        Read the Activity (type of activity the subject is doing)
     activityTrainData <- fread("./UCI HAR Dataset/train/y_train.txt", nrows = scope, header = FALSE, col.names = c("activity"))
     activityTestData <- fread("./UCI HAR Dataset/test/y_test.txt", nrows = scope, header = FALSE, col.names = c("activity"))
     ##
     ##   1. Merge the training and the test sets to create one data set
     ##
     subjectData <- rbind(subjectTrainData, subjectTestData)
     movementData <- rbind(movementTrainData, movementTestData)
     activityData <- rbind(activityTrainData, activityTestData)
     ##
     ##   3. Uses descriptive activity names to name the activities in the data set
     ##        Replace activity codes by activity descriptions
     ##
     activityLabels <- data.frame(activity = character())
     for (i in seq_along(activityData$activity)) {
          if      (activityData[i] == 1)   {activityLabels <- rbind(activityLabels, data.frame(activity = "walking"))}
          else if (activityData[i] == 2)   {activityLabels <- rbind(activityLabels, data.frame(activity = "walking_upstairs"))}
          else if (activityData[i] == 3)   {activityLabels <- rbind(activityLabels, data.frame(activity = "walking_downstairs"))}
          else if (activityData[i] == 4)   {activityLabels <- rbind(activityLabels, data.frame(activity = "sitting"))}
          else if (activityData[i] == 5)   {activityLabels <- rbind(activityLabels, data.frame(activity = "standing"))}
          else if (activityData[i] == 6)   {activityLabels <- rbind(activityLabels, data.frame(activity = "laying"))}
          else                             {activityLabels <- rbind(activityLabels, data.frame(activity = NA))}
     }
     ##   2. Extract only the measurements on the mean and standard deviation for each measurement
     ##        Read features.txt to determine the columns required (mean and std)
     ##
     featureData <- fread("./UCI HAR Dataset/features.txt", header = FALSE, col.names = c("columnid", "feature"))
     ##        Filter the feature data to only those columns that calulate mean() or std()
     filteredFeatureColumns <- grep("mean\\(\\)|std\\(\\)",featureData$feature)
     ##
     ##   4. Appropriately labels the data set with descriptive variable names.
     ##        Create list of tidy varable names for filtered columns
     tidyColumnsNames <- data.frame(columnID = integer(), columnName = character())
     for (i in seq_along(filteredFeatureColumns)) {
          columnID   <- filteredFeatureColumns[i]
          columnName <- featureData$feature[filteredFeatureColumns[i]]
          tidyColumnsNames <- rbind(tidyColumnsNames, data.frame(columnID = columnID, columnName = columnName))
     }
     ##        Remove parenthesis and hyphen characters and move columnnames to lowercase
     tidyColumnsNames$columnName <- lapply(tidyColumnsNames$columnName, str_replace, "\\(\\)", "")
     tidyColumnsNames$columnName <- lapply(tidyColumnsNames$columnName, str_replace_all, "-", "")
     tidyColumnsNames$columnName <- lapply(tidyColumnsNames$columnName, tolower)
     ##
     ##   2. Extract only the measurements on the mean and standard deviation for each measurement
     ##        Strip movement data to required columns only (note: create 1st dummy column to set the dimension)
     ##   
     selectedMovementData <- data.frame(movementData$V1)
     for (i in seq_along(filteredFeatureColumns)) {
          ##   Copy column to dataset with selected columns
          selectedMovementData <- cbind(selectedMovementData, movementData[[filteredFeatureColumns[i]]])
     }
     ##   Remove the dummy column and give reduced dataset tidy column names
     selectedMovementData$movementData.V1 <- NULL
     names(selectedMovementData) <- tidyColumnsNames$columnName
     ##   Merge Subject, Activity and Movement data into one table
     tidyMovementData <- cbind(activityLabels, selectedMovementData)
     tidyMovementData <- cbind(subjectData, tidyMovementData)
     ##
     ##   Write the tidy dataset into an output file "tidy-movement-data.csv"
     ##
     write.csv(tidyMovementData, file = "tidy-movement-data.csv")
     ##
     ##   5. Create a second, independent tidy data set with the average of each variable for each activity and each subject
     ##
     ##        group the full tidy data set by subject and activity
     groupedMovementData <- group_by(tidyMovementData, subject, activity)
     ##        Select all columns besides the 2 grouping columns
     filteredMovementData <- select(groupedMovementData, -(subject:activity))
     ##        Take the mean for each selected and grouped column
     tidyMeanMovements <- summarise_each(filteredMovementData, funs(mean))
     ##        Write output to file
     write.csv(tidyMeanMovements, file = "tidy-mean-movements.csv")
}     