# Step 1: Prepare the files
# Data sourced from following URL
# here new directory created and file downloaded
urlSource <-
    "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if (!dir.exists("./data"))
    dir.create("./data")

if (!file.exists("./data/UCI HAR Dataset.zip"))
    download.file(urlSource, "./data/UCI HAR Dataset.zip", method = "curl")
# Data was unzipped to the directory manually using 7zip (unknown number of directories required)

# Step 2: Prepare the descriptive names
# Variable names are read
Col_Data <-
    read.delim(
        "./data/UCI HAR Dataset/features.txt",
        sep = " ",
        header = FALSE,
        as.is = 2
    )
# There are duplicate column names. repeated values are suffixed with "Y" and "Z" to avoid duplicates
Col_Data[c(317:330, 396:409, 475:488), 2] <-
    paste0(Col_Data[c(317:330, 396:409, 475:488), 2] , "Y")
Col_Data[c(331:344, 410:423, 489:502), 2] <-
    paste0(Col_Data[c(331:344, 410:423, 489:502), 2] , "Z")
#  avoid special characters in variable names.
DataColNames <-
    gsub(",", "c", gsub("\\)", "", gsub("\\(", "", gsub("-", "", Col_Data$V2))))
# Read descriptive activity names 
activityLabels <-
    read.csv(
        "./data/UCI HAR Dataset/activity_labels.txt",
        header = FALSE,
        sep = " ",
        col.names = c("activity", "activityname")
    )

# Step 3 : Prepare Training data. Read and add columns.
# Read the training set which is in fixed width format
X_trainData <-
    read.fwf(
        "./data/UCI HAR Dataset/train/X_train.txt",
        widths = rep(16, 561),
        header = FALSE
    )
# per 4. Appropriately labels the data set with descriptive variable names.
colnames(X_trainData) <- DataColNames
# Read corresponding Activity codes 
trainLabelCodes <-
    read.csv(
        "./data/UCI HAR Dataset/train/y_train.txt",
        header = FALSE,
        as.is = 1,
        col.names = "act"
    )
# Extend the training data with Activity codes
X_trainData["act"] <- trainLabelCodes
# Read corresponding subject codes
trainSubjectData <-
    read.csv(
        "./data/UCI HAR Dataset/train/subject_train.txt",
        header = FALSE,
        as.is = 1
    )
# Extend the training data with subject codes
X_trainData["subject"] <- trainSubjectData


# Step 4 : Prepare test data. Read and add columns.
# Read the test set which is in fixed width format
X_testData <-
    read.fwf(
        "./data/UCI HAR Dataset/test/X_test.txt",
        widths = rep(16, 561),
        header = FALSE
    )
# per 4. Appropriately labels the data set with descriptive variable names.
colnames(X_testData) <- DataColNames
# Read corresponding Activity codes 
testLabelCodes <-
    read.csv(
        "./data/UCI HAR Dataset/test/y_test.txt",
        header = FALSE,
        as.is = 1,
        col.names = "act"
    )
# Extend the training data with Activity codes
X_testData["act"] <- testLabelCodes
# Read corresponding subject codes
testSubjectData <-
    read.csv(
        "./data/UCI HAR Dataset/test/subject_test.txt",
        header = FALSE,
        as.is = 1
    )
# Extend the training data with subject codes
X_testData["subject"] <- testSubjectData

# Step 5: Merge training and test data
# Per 1: Merges the training and the test sets to create one data set.
# there could be duplicates, so using union_all which is available in dplyr
library(dplyr)
X_totalData <- union_all(X_testData, X_trainData)

# Step 5: Extract required columns
# read original column names so that correct columns can be read
# example: -mean can be read instead of mean (could be part of another word)
Col_Data <-
    read.delim(
        "./data/UCI HAR Dataset/features.txt",
        sep = " ",
        header = FALSE,
        as.is = 2
    )
# per 2. Extracts only the measurements on the mean and standard deviation for each measurement.
extData <-
    X_totalData[, c(562:563, grep("\\-mean|\\-std", Col_Data$V2))]

# Step 6: use descriptive activity names
# per 3. Uses descriptive activity names to name the activities in the data set
# resulting data set has descriptive variable names as 4. is addressed earlier in Step 3
extNamedData <-
    merge(
        activityLabels,
        extData,
        by.y = "act",
        by.x = "activity",
        all.y = TRUE
    )

#Step 8: create independed tidy data set
# cound number of columns 
numCols <- dim(extNamedData)[2]
# prepare tidy data that has mean of all columns starting from 4
# Per 5. From the data set in step 4, creates a second, independent tidy data
# set with the average of each variable for each activity and each subject.
tidyData <-
    aggregate(
        extNamedData[, 4:numCols],
        by = list(extNamedData$activityname, extNamedData$subject),
        FUN = mean,
        simplify = TRUE
    )
# name first two columns of tidy data set appropriately
colnames(tidyData)[1:2] <- c("activityname", "subject")
