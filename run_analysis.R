# load dependency
library(dplyr)

# uncomment if zip file is not already downloaded
# download.file("http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "dataset.zip")

# uncomment if zip file is not already extracted
# unzip("dataset.zip")

## Beginning of Question 1. 
## the following section of the script merges the training and the test sets to create one data set:

# load in features
features <- read.table("./UCI HAR Dataset/features.txt", as.is = TRUE, header = FALSE)

# fixes features that have errors in 'features.txt'
features[556,2] <- "angle(tBodyAccJerkMean,gravityMean)"
features[512,2] <- "fBodyAccMag-maxInds()"
features[449,2] <- "fBodyGyro-maxInds()-X"
features[450,2] <- "fBodyGyro-maxInds()-Y"
features[451,2] <- "fBodyGyro-maxInds()-Z"
features[370,2] <- "fBodyAccJerk-maxInds()-X"
features[371,2] <- "fBodyAccJerk-maxInds()-Y"
features[372,2] <- "fBodyAccJerk-maxInds()-Z"
features[291,2] <- "fBodyAcc-maxInds()-X"
features[292,2] <- "fBodyAcc-maxInds()-Y"
features[293,2] <- "fBodyAcc-maxInds()-Z"

# load in x_test with column names from features.txt
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE, col.names = features[,2])

# load in y_test, name  column as 'activity_label'
y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt", header = FALSE, col.names = "activity_label")

# load in subject_test, name column as 'subject'
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE, col.names = "subject")

# bind subject test and y_test to x_test, save as test
test <- cbind(subject_test, y_test, x_test)

####

# load in x_train with column names from features.txt
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE, col.names = features[,2])

# load in y_test, name  column as 'activity_label'
y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt", header = FALSE, col.names = "activity_label")

# load in subject_test, name column as 'subject'
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE, col.names = "subject")

# bind subject test and y_test to x_test, save as test
train <- cbind(subject_train, y_train, x_train)

# bind test and train data sets into one data set
merged <- rbind(test, train)

## End of Question 1.
## the above section of the script merges the training and the test sets to
## create one data set.

## Beginning of Question 2.
## the below section extracts only the measurements on the mean and standard 
## deviation for each measurement. 

# selects columns 'subject' and 'activity_label' from the merged data
merged_id <- merged[,1:2]

# selects columns with only measurements on the mean
merged_mean <- select(merged, contains("mean"))

# selects columns with only measurements on the standard deviation
merged_std <- select(merged, contains("std"))

# combines the 'subject' and 'activity_label' identifiers with the 
# measurements for only mean and standard deviation
id_mean_std <- cbind(merged_id, merged_mean, merged_std)

## End of Question 2.
## the above section extracts only the measurements on the mean and standard 
## deviation for each measurement. 

## Beginning of Question 3.
## the below section uses descriptive activity names to name the activities
## in the data set

# the second column, containing the 'activity_label' levels 1 through 6 are
# assigned labels as defined in the file 'activity_labels.txt' using factor()
# which are the descriptive activity names the question is asking for
id_mean_std[,2] <- factor(id_mean_std[,2], c(1,2,3,4,5,6), c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))

## End of Question 3.
## the above section uses descriptive activity names to name the activities 
## in the data set

## Beginning of Question 4.
## the below section appropriately labels the data set with descriptive 
## variable names

colnames(id_mean_std) <- c("subject",
  "activity_label",
  "tBodyAcc.mean.X",
  "tBodyAcc.mean.Y",
  "tBodyAcc.mean.Z",
  "tGravityAcc.mean.X",
  "tGravityAcc.mean.Y",
  "tGravityAcc.mean.Z",
  "tBodyAccJerk.mean.X",
  "tBodyAccJerk.mean.Y",
  "tBodyAccJerk.mean.Z",
  "tBodyGyro.mean.X",
  "tBodyGyro.mean.Y",
  "tBodyGyro.mean.Z",
  "tBodyGyroJerk.mean.X",
  "tBodyGyroJerk.mean.Y",
  "tBodyGyroJerk.mean.Z",
  "tBodyAccMag.mean",
  "tGravityAccMag.mean",
  "tBodyAccJerkMag.mean",
  "tBodyGyroMag.mean",
  "tBodyGyroJerkMag.mean",
  "fBodyAcc.mean.X",
  "fBodyAcc.mean.Y",
  "fBodyAcc.mean.Z",
  "fBodyAcc.meanFreq.X",
  "fBodyAcc.meanFreq.Y",
  "fBodyAcc.meanFreq.Z",
  "fBodyAccJerk.mean.X",
  "fBodyAccJerk.mean.Y",
  "fBodyAccJerk.mean.Z",
  "fBodyAccJerk.meanFreq.X",
  "fBodyAccJerk.meanFreq.Y",
  "fBodyAccJerk.meanFreq.Z",
  "fBodyGyro.mean.X",
  "fBodyGyro.mean.Y",
  "fBodyGyro.mean.Z",
  "fBodyGyro.meanFreq.X",
  "fBodyGyro.meanFreq.Y",
  "fBodyGyro.meanFreq.Z",
  "fBodyAccMag.mean",
  "fBodyAccMag.meanFreq",
  "fBodyAccJerkMag.mean",
  "fBodyAccJerkMag.meanFreq",
  "fBodyGyroMag.mean",
  "fBodyGyroMag.meanFreq",
  "fBodyGyroJerkMag.mean",
  "fBodyGyroJerkMag.meanFreq",
  "angle.tBodyAccMean.gravityMean",
  "angle.tBodyAccJerkMean.gravityMean",
  "angle.tBodyGyroMean.gravityMean",
  "angle.tBodyGyroJerkMean.gravityMean",
  "angle.XMean.gravityMean",
  "angle.YMean.gravityMean",
  "angle.ZMean.gravityMean",
  "tBodyAcc.std.X",
  "tBodyAcc.std.Y",
  "tBodyAcc.std.Z",
  "tGravityAcc.std.X",
  "tGravityAcc.std.Y",
  "tGravityAcc.std.Z",
  "tBodyAccJerk.std.X",
  "tBodyAccJerk.std.Y",
  "tBodyAccJerk.std.Z",
  "tBodyGyro.std.X",
  "tBodyGyro.std.Y",
  "tBodyGyro.std.Z",
  "tBodyGyroJerk.std.X",
  "tBodyGyroJerk.std.Y",
  "tBodyGyroJerk.std.Z",
  "tBodyAccMag.std",
  "tGravityAccMag.std",
  "tBodyAccJerkMag.std",
  "tBodyGyroMag.std",
  "tBodyGyroJerkMag.std",
  "fBodyAcc.std.X",
  "fBodyAcc.std.Y",
  "fBodyAcc.std.Z",
  "fBodyAccJerk.std.X",
  "fBodyAccJerk.std.Y",
  "fBodyAccJerk.std.Z",
  "fBodyGyro.std.X",
  "fBodyGyro.std.Y",
  "fBodyGyro.std.Z",
  "fBodyAccMag.std",
  "fBodyAccJerkMag.std",
  "fBodyGyroMag.std",
  "fBodyGyroJerkMag.std")

# this was also satisfied on lines 14, 17-27, 30, 33, and 36 with the labels applied
# to the data import on line 30. use summary(id_mean_std) to check the labels

## End of Question 4.
## the above section references how the script appropriately labels the data 
## set with descriptive variable names

## Beginning of Question 5.
## the below section uses the data set completed by step 4 to create a second,
## independent tidy data set with the average of each variable for each 
## activity and each subject.

# group the tidy data by 'subject' and 'activity_label'
by_part <- id_mean_std %>% group_by(subject, activity_label)
# summarise the grouped data by calculating the means for all variables 
# for each of the 'subject' and 'activity_label' combinations (there are 180)
tidy_means <- by_part %>% summarise_each(funs(mean))

# writes a file containing the tidy_means table to the './UCI HAR Dataset'
# directory. this file contains an independent tidy data set with the average
# of each variable for each activity and each subject
write.table(tidy_means, "./tidy_means.txt", quote = FALSE, row.names = FALSE)

## End of Question 5.
## from the data set in step 4, created a second, independent tidy data set 
## with the average of each variable for each activity and each subject.