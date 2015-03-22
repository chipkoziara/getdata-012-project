# Tidy Sensor Data 
## Readme
Included is an R script called run_analysis.R that does the following (pursuant to the [Getting and Cleaning Data Coursera class's class project](https://class.coursera.org/getdata-012/human_grading/view/courses/973499/assessments/3/submissions):
1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

I've included an explanation of run_analysis.R's inner workings in greater detail below. The script itself is also heavily documented.

Please refer to [codebook.md](link_here) for descriptions of each of the variables present in the data set generated by the run_analysis.R script.

## On being tidy
To verify that the script generates a second, independent tidy data set with the average of each variable for each activity and each subject, please:
1. Run the R script run_analysis.R 
2. Load in the resulting 'tidy_means.txt' file: '''tidy_means <- read.table("./tidy_means.txt", header = TRUE)'''
3. View the resulting file to verify it is tidy: '''View(tidy_means)'''

The 'tidy_means' table is tidy due to the following characteristics, as outlined in Hadley Wickham's [Tidy Data paper](http://vita.had.co.nz/papers/tidy-data.pdf) and David Hood's [thread on tidy data](https://class.coursera.org/getdata-012/forum/thread?thread_id=234) on the Getting and Cleaning Data Coursera class Class Discussion forum:
1. Each variable forms a column
2. Each observation forms a row
3. Each observational unit forms a a table

Each column in 'tidy_means' corresponds to a measurement variable from a sensor within a Samsung Galaxy S II, a variable that identifies which activity a participant was engaging in, or a variable that identifies which subject was engaging in an activity. This satisfies the first condition.

Each observation forms a row within the data set, with each observation being the average of specified sensor variables for every subject and activity combination. There are 180 observations in this data set. This satisfies the second condition.

There is only one observational unit, which is the average of specified sensor data for each activity and each subject. This satisfies the third condition.

### On being wide
There are no requirements about the specific number of columns or rows which would dictate whether the data must be wide or narrow.

The data is wide because each measurement variable (and activity and subject) forms a column, while the values of each unique activity and subject variable dictate the number of rows. This is discussed in greater depth in David Hood's post referenced above.

### On selection of columns to include in 'means' condition
As there are no specific requirements around how to select means and stds, I decided to include all measurements that included 'mean' or 'std' in the name of the measurement. All of the selections are not means in the strictest sense, but they are calculated from means (like in the angle measurements). I decided to err on the side of including this data because they fit into the description outlined in step 3 of the assignment ("measurements on the mean and standard deviation") as they are composed of measurements on the mean.

# How to use the script

Please use RStudio (untested in standard R). This script assumes that the dataset is stored in the directory 'UCI HAR Dataset' which is stored in your working directory ('YOUR_WORKING_DIRECTORY/UCI HAR Dataset/').

If you do not have the dataset located here, please move it to this location or uncomment lines 5 and 8 in the script to download the dataset and unzip it to your working directory.

This script also assumes you have installed the 'dplyr' package. If you have not installed 'dplyr' please do so before running the script. The script does not assume you have loaded 'dplyr' so there is no need to load 'dplyr' before running the script, as it will do that automatically.

## How the script works
The script loads in 'features.txt' from the dataset and stores it as a vector, 'features'.

Naming errors in 'features.txt' are corrected in 'features' before 'read.table' loads the 'X_test.txt' and 'X_train.txt' as 'X_test' and 'X_train', respectively, with the parameter 'col.names' set to the updated 'features' vector.

This begins to satisfy Step 4's prompt to appropriately label the data set with descriptive variable names.

'Y_test.txt' and 'subject_test.txt' are read in with 'read.table' with the descriptive names 'activity_label' and 'subject' applied with the 'col.name' parameter in 'read.table'.

This further satisfies Step 4's prompt to appropriately label the data set with descriptive variable names.

'cbind' is then used to combine them with the 'X_test' table, creating the 'test' table.

'Y_train.txt' and 'subject_train.txt' are read in with 'read.table' with the descriptive names 'activity_label' and 'subject' applied with the 'col.name' parameter in 'read.table'.

This further satisfies Step 4's prompt to appropriately label the data set with descriptive variable names.

'cbind' is then used to combine them with the 'X_train' table, creating the 'train' table.

'rbind' is then used to combine the 'test' and 'train' tables into one table called 'merged'.

This satisfies Step 1's prompt to merge the training and the test sets to
create one data set.

The 'subject' and 'activity_label' fields are subset into a new table called 'merged_id'.

'select' is then used to extract columns containing measurements with mean values, storing the result as a table called 'merged_means'.

'select' is then used to extract columns containing measurements with std values, storing the result as a table called 'merged_std'.

'cbind' is then used to combine the 'merged_id', 'merged_means', and 'merged_std' tables into a new table called 'id_mean_std'.

This satisfies Step 2's prompt to extract only the measurements on the mean and standard deviation for each measurement.

To meet Step 3's prompt to use descriptive activity names to name the activities in the data set, the second column ('activity_label') of 'id_mean_std' is assigned labels as defined in the file 'activity_labels.txt' using 'factor', with levels 1 through 6, which are the descriptive activity names the question is asking for.

Step 4's prompt was satisfied in the steps outlined earlier, applying appropriate labels to the data set with descriptive variable names.

To satisfy Step 5's prompt, the script uses the data set completed by Step 4 to create a second, independent tidy data set with the average of each variable for each activity and each subject.

First, the 'id_mean_std' is grouped by 'activity_label' and 'subject' using the 'group_by' function. This result is stored in 'by_part'.

The function 'summarise_each' is then used to calculate the means for the sensors variables for each combination of 'activity_lavel' and 'subject' by passing 'funs(mean)' into 'summarise_each'. The resulting tidy dataset is then stored as 'tidy_means'.

Finally, 'write.table' is used to save the 'tidy_means' table as 'tidy_means.txt'.

An explanation of why this data is tidy is found above in the "On being tidy" section.


