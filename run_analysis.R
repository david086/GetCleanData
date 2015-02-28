
## Course project - Getting and Cleaning Data
library(plyr)

## download the data from the website
setwd("C:/Dropbox/DataScience/Getting_and_Cleaning_Data/Project"); getwd()


# download file
download <- function(fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                     zipfile="data/projectData.zip"
                     ) {
      # check if a data folder exists; if not then create one
      if (!file.exists("data")){dir.create("data")}      
      
      download.file(fileUrl, destfile = zipfile)
      unzip(zipfile, exdir="data")
}
download()

## 1. Merges the training and the test sets to create one data set.
test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
test_Label <- read.table("./data/UCI HAR Dataset/test/Y_test.txt")
test_Subject <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

training <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
training_Label <- read.table("./data/UCI HAR Dataset/train/Y_train.txt")
training_Subject <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

activity_Label <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
features <- read.table("./data/UCI HAR Dataset/features.txt")


merged_Table <- rbind(training, test); names(merged_Table) <- features$V2
merged_Label <- rbind(training_Label, test_Label); names(merged_Label) <- "activity"
merged_Subject <- rbind(training_Subject, test_Subject); names(merged_Subject) <- "participant"
merged <- list(x=merged_Table, y=merged_Label, subject=merged_Subject)

## str(merged)

## 2. Extracts only the measurements on the mean and 
##    standard deviation for each measurement. 

mean_std = function(dataset) {
      # Given the dataset (x values), calcualate 
      # the mean and standard deviation for each measurement.
      
      # Read the feature list file
      features <- read.table("data/UCI HAR Dataset/features.txt")
      # Find the mean and std columns
      mean.col <- sapply(features[,2], function(x) grepl("mean()", x, fixed=T))
      std.col <- sapply(features[,2], function(x) grepl("std()", x, fixed=T))
      # Extract them from the data
      edf <- df[, (mean.col | std.col)]
      colnames(edf) <- features[(mean.col | std.col), 2]
      edf
}
## ?grepl()


## 3. Uses descriptive activity names to name the activities in the data set
assign.activities = function(dataset) {
      # Use descriptive activity names to name the activities in the dataset
      colnames(df) <- "activity"
      df$activity[df$activity == 1] = "WALKING"
      df$activity[df$activity == 2] = "WALKING_UPSTAIRS"
      df$activity[df$activity == 3] = "WALKING_DOWNSTAIRS"
      df$activity[df$activity == 4] = "SITTING"
      df$activity[df$activity == 5] = "STANDING"
      df$activity[df$activity == 6] = "LAYING"
      df
}
activity_Label$V1
activity_Label$V2

## 4. Appropriately labels the data set with descriptive variable names. 



## 5. From the data set in step 4, creates a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.

