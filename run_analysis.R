library(dplyr)
library(plyr)

# Download the data from the hosts

if(!file.exists("./projectdata")){dir.create(".projectdata")}
zipURL <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(zipURL, destfile = "./projectdata/Dataset.zip", method = "curl")

# Unzip data file

unzip("./projectdata/Dataset.zip", exdir = "./projectdata")

# File path

dataPath <- file.path("./projectdata", "UCI HAR Dataset")
files <- list.files(dataPath, recursive = TRUE)

# Read data from the files 

subject_train <- read.table(file.path(dataPath, "train", "subject_train.txt"), header = FALSE)
subject_test <- read.table(file.path(dataPath, "test", "subject_test.txt"), header = FALSE)

feature_train <- read.table(file.path(dataPath, "train", "X_train.txt"), header = FALSE)
feature_test <- read.table(file.path(dataPath, "test", "X_test.txt"), header = FALSE)

activity_train <- read.table(file.path(dataPath, "train", "y_train.txt"), header = FALSE)
activity_test <- read.table(file.path(dataPath, "test", "y_test.txt"), header = FALSE)

features <- read.table(file.path(dataPath, "features.txt"), header = FALSE)
activities <- read.table(file.path(dataPath, "activity_labels.txt"), header = FALSE, col.names = c("activityID", "activityLabel"))

#######################################
#             Step 1                  #
#######################################

# Combine the data

subject_data <- rbind(subject_train, subject_test)
feature_data <- rbind(feature_train, feature_test)
activity_data <- rbind(activity_train, activity_test)

# Name the variables in the data

names(subject_data) <- c("subject")
names(activity_data) <- c("activity")
names(feature_data) <- features$V2

# Combine data for complete dataset.

subActData <- cbind(subject_data, activity_data)
completeData <- cbind(feature_data, subActData)

#######################################
#             Step 2                  #
#######################################

# Extract measurements of mean and std deviation

measureToKeep <- grepl("subject|activity|mean\\(\\)|std\\(\\)", colnames(completeData))
filteredData <- completeData[, measureToKeep]

#######################################
#             Step 3                  #
#######################################

filteredData$activity <- factor(filteredData$activity, levels = activities[, 1], labels = activities[, 2])

#######################################
#             Step 4                  #
#######################################

names(filteredData) <- gsub("^t", "timeDomain", names(filteredData))
names(filteredData) <- gsub("^f", "frequencyDomain", names(filteredData))
names(filteredData) <- gsub("Acc", "Acceleration", names(filteredData))
names(filteredData) <- gsub("Gyro", "Gyroscope", names(filteredData))
names(filteredData) <- gsub("Mag", "Magnitude", names(filteredData))
names(filteredData) <- gsub("BodyBody", "Body", names(filteredData))
names(filteredData) <- gsub("[\\(\\)-]", " ", names(filteredData))
names(filteredData) <- gsub("mean", "Mean", names(filteredData))
names(filteredData) <- gsub("std", "Standard Deviation", names(filteredData))

#######################################
#             Step 5                  #
#######################################

filteredDataAvg <- filteredData %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
  
write.table(filteredDataAvg, file = "tidy_data.txt", row.names = FALSE)
