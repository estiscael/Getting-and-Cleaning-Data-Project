## Merge the training and the test sets to create one data set.

### Read in the data from files
# train data
features     = read.table('./UCI HAR Dataset/features.txt', sep=" ", header=FALSE);
activityType = read.table('./UCI HAR Dataset/activity_labels.txt',  sep=" ", header=FALSE);
subjectTrain = read.table('./UCI HAR Dataset/train/subject_train.txt', header=FALSE);
xTrain       = read.table('./UCI HAR Dataset/train/X_train.txt', header=FALSE);
yTrain       = read.table('./UCI HAR Dataset/train/y_train.txt', header=FALSE);
# test data
subjectTest  = read.table('./UCI HAR Dataset/test/subject_test.txt', header=FALSE);
xTest        = read.table('./UCI HAR Dataset/test/X_test.txt', header=FALSE);
yTest        = read.table('./UCI HAR Dataset/test/y_test.txt', header=FALSE);

### Assigin column names
# train data
colnames(activityType) = c('activityId','activityType');
colnames(subjectTrain) = "subjectId";
colnames(xTrain) = features[,2];
colnames(yTrain) = "activityId";
# test data
colnames(subjectTest) = "subjectId";
colnames(xTest) = features[,2];
colnames(yTest) = "activityId";

### Merge data
trainData = cbind(yTrain,subjectTrain,xTrain);
testData = cbind(yTest,subjectTest,xTest);
# Combine training and test data to create a final data set
finalData = rbind(trainData,testData);


## Extract only the measurements on the mean and standard deviation for each measurement.

### Subset finalData table based on the logicalVector to keep only desired columns
colNames = colnames(finalData);
logicalVector = (grepl("activity..",colNames) 
                 | grepl("subject..",colNames) 
                 | grepl("-mean..",colNames) 
                 & !grepl("-meanFreq..",colNames) 
                 & !grepl("mean..-",colNames) 
                 | grepl("-std..",colNames) 
                 & !grepl("-std()..-",colNames));

finalData = finalData[logicalVector==TRUE];

## Use descriptive activity names to name the activities in the data set

### Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);


## Appropriately label the data set with descriptive activity names.

### Cleaning up the variable names
colNames = colnames(finalData);
for (i in 1:length(colNames))
{
    colNames[i] = gsub("\\()","",colNames[i])
    colNames[i] = gsub("-std$","StdDev",colNames[i])
    colNames[i] = gsub("-mean","Mean",colNames[i])
    colNames[i] = gsub("^(t)","time",colNames[i])
    colNames[i] = gsub("^(f)","freq",colNames[i])
    colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
    colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
    colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
    colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
    colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
    colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
    colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};
colnames(finalData) = colNames;


## Create a second, independent tidy data set with the average of each variable for each activity and each subject.

### Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType = finalData[,names(finalData) != 'activityType'];

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],
                     by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),
                     mean);
tidyData = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');