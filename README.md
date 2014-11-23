Getting-and-Cleaning-Data-Course_Project
========================================
Original Data Set Information:

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. 

===Tidy dataset generated based on the original dataset from=== https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

===Attribute Information:===
- There are 6 records (orderd by LAYING, SITTING, STANDING, WALKING, WALKING_DOWNSTAIRS, WALKING_UPSTAIRS) listed in tidy dataset. Each record indicates a specific type of activity.
- Each record in the tidy dataset shows the average of each variable for a specific activity and each subject.

There are total 79 variables in tidy dataset listed below (same order listed in the output.txt). The terms 'X', 'Y' and 'Z' in the varialbel name indicate the movement axis in space. The term "mean" in the name of variable indicates the averaged value and the term "std" indicates the standard deviation generated based on the measured data.

1. tBodyAcc-mean()-X
2. tBodyAcc-mean()-Y
3. tBodyAcc-mean()-Z
4. tBodyAcc-std()-X
5. tBodyAcc-std()-Y
6. tBodyAcc-std()-Z
7. tGravityAcc-mean()-X
8. tGravityAcc-mean()-Y
9. tGravityAcc-mean()-Z
10. tGravityAcc-std()-X
11. tGravityAcc-std()-Y
12. tGravityAcc-std()-Z
13. tBodyAccJerk-mean()-X
14. tBodyAccJerk-mean()-Y
15. tBodyAccJerk-mean()-Z
16. tBodyAccJerk-std()-X
17. tBodyAccJerk-std()-Y
18. tBodyAccJerk-std()-Z
19. tBodyGyro-mean()-X
20. tBodyGyro-mean()-Y
21. tBodyGyro-mean()-Z
22. tBodyGyro-std()-X
23. tBodyGyro-std()-Y
24. tBodyGyro-std()-Z
25. tBodyGyroJerk-mean()-X
26. tBodyGyroJerk-mean()-Y
27. tBodyGyroJerk-mean()-Z
28. tBodyGyroJerk-std()-X
29. tBodyGyroJerk-std()-Y
30. tBodyGyroJerk-std()-Z
31. tBodyAccMag-mean()
32. tBodyAccMag-std()
33. tGravityAccMag-mean()
34. tGravityAccMag-std()
35. tBodyAccJerkMag-mean()
36. tBodyAccJerkMag-std()
37. tBodyGyroMag-mean()
38. tBodyGyroMag-std()
39. tBodyGyroJerkMag-mean()
40. tBodyGyroJerkMag-std()
41. fBodyAcc-mean()-X
42. fBodyAcc-mean()-Y
43. fBodyAcc-mean()-Z
44. fBodyAcc-std()-X
45. fBodyAcc-std()-Y
46. fBodyAcc-std()-Z
47. fBodyAcc-meanFreq()-X
48. fBodyAcc-meanFreq()-Y
49. fBodyAcc-meanFreq()-Z
50. fBodyAccJerk-mean()-X
51. fBodyAccJerk-mean()-Y
52. fBodyAccJerk-mean()-Z
53. fBodyAccJerk-std()-X
54. fBodyAccJerk-std()-Y
55. fBodyAccJerk-std()-Z
56. fBodyAccJerk-meanFreq()-X
57. fBodyAccJerk-meanFreq()-Y
58. fBodyAccJerk-meanFreq()-Z
59. fBodyGyro-mean()-X
60. fBodyGyro-mean()-Y
61. fBodyGyro-mean()-Z
62. fBodyGyro-std()-X
63. fBodyGyro-std()-Y
64. fBodyGyro-std()-Z
65. fBodyGyro-meanFreq()-X
66. fBodyGyro-meanFreq()-Y
67. fBodyGyro-meanFreq()-Z
68. fBodyAccMag-mean()
69. fBodyAccMag-std()
70. fBodyAccMag-meanFreq()
71. fBodyBodyAccJerkMag-mean()
72. fBodyBodyAccJerkMag-std()
73. fBodyBodyAccJerkMag-meanFreq()
74. fBodyBodyGyroMag-mean()
75. fBodyBodyGyroMag-std()
76. fBodyBodyGyroMag-meanFreq()
77. fBodyBodyGyroJerkMag-mean()
78. fBodyBodyGyroJerkMag-std()
79. fBodyBodyGyroJerkMag-meanFreq()


===R code explaination===

- Before running the script please make sure that the working directory is set to the original data folder, "UCI HAR Dataset".

Here give the descriptions for what the analysis files did. 

# Get data from working directory
my_activity_labels = read.table("activity_labels.txt")
my_features = read.table("features.txt")

# Merges the training and the test set to create one data set.
my_te_data = read.table(".\\test\\X_test.txt") # data
my_te_activities = read.table(".\\test\\Y_test.txt") # label
my_tr_data = read.table(".\\train\\X_train.txt") # data
my_tr_activities = read.table(".\\train\\Y_train.txt") # label
my_data_set = rbind(my_tr_data, my_te_data)
my_activities = rbind(my_tr_activities, my_te_activities)

# Extracts the only measurements on the mean and standard deviation for each measurement
v_mean_std_indices<-c(grep('mean', my_features$V2), grep('std', my_features$V2))
v_mean_std_indices<-sort(v_mean_std_indices)
my_data_set <- my_data_set[, v_mean_std_indices]
my_data_set <- cbind(my_data_set, my_activities)

# Appropriately labels the data set with descriptive variable names
colnames(my_data_set)<-c(as.character(my_features$V2[v_mean_std_indices]), "activity")

# Uses descriptive activity names to name the activities in the data set
my_data_set$activity<-my_activity_labels$V2[my_data_set$activity]

# creates a second, independent tidy data set with the average of each variable for each activity and each subject
result<-tapply(my_data_set[,1], my_data_set$activity, mean)
for ( i in 2:(ncol(my_data_set)-1))
{
  result<-cbind(result, tapply(my_data_set[,i], my_data_set$activity, mean))
}
colnames(result)<-as.character(my_features$V2[v_mean_std_indices]);
write.table(result, 'output.txt', row.name=FALSE)
