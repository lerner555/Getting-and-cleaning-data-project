library(dplyr)

# Read data

zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}


dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}

trainingSubjects <- read.table(file.path(dataPath, "train/subject_train.txt"))
trainingValues <- read.table(file.path(dataPath, "train/X_train.txt"))
trainingActivity <- read.table(file.path(dataPath, "train/y_train.txt"))

testSubjects <- read.table(file.path(dataPath, "test/subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test/X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test/y_test.txt"))

features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

# Step 1 - Merge the data sets

humanActivity <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

colnames(humanActivity) <- c("subject", features[, 2], "activity")

# Step 2 - Extracts only the measurements on the mean and stardard deviation

columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
humanActivity <- humanActivity[, columnsToKeep]

# Step 3 - Descriptive activity names

humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

# Step 4 - Appropriately labels

humanActivityCols <- colnames(humanActivity)

humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

colnames(humanActivity) <- humanActivityCols

# Step 5 - Creating independend tidy data set

humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# output to "tidy_data.text"

write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
