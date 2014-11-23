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
