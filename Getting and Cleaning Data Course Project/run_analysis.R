library(data.table)

## Read the files
subjectTrain <- fread(file.path('UCI HAR Dataset', "train", "subject_train.txt"))
subjectTest  <- fread(file.path('UCI HAR Dataset', "test" , "subject_test.txt" ))

activityTrain <- fread(file.path('UCI HAR Dataset', "train", "Y_train.txt"))
activityTest  <- fread(file.path('UCI HAR Dataset', "test" , "Y_test.txt" ))

fileToDataTable <- function (f) {
  df <- read.table(f)
  dt <- data.table(df)
}
train <- fileToDataTable(file.path('UCI HAR Dataset', "train", "X_train.txt"))
test  <- fileToDataTable(file.path('UCI HAR Dataset', "test" , "X_test.txt" ))

## Merge the training- and testset
subject <- rbind(subjectTrain, subjectTest)
setnames(subject, "V1", "subject")
activity <- rbind(activityTrain, activityTest)
setnames(activity, "V1", "activityNum")
dt <- rbind(train, test)

subject <- cbind(subject, activity)
dt <- cbind(subject, dt)

setkey(dt, subject, activityNum)

## Extracts only the measurements on the mean and standard deviation
features <- fread(file.path('UCI HAR Dataset', "features.txt"))
setnames(features, names(features), c("featureNum", "featureName"))

features <- features[grepl("mean\\(\\)|std\\(\\)", featureName)]
# datatable with featureNum and featureName (mean and sd only)
# add featureCode to featues
features$featureCode <- features[, paste0("V", featureNum)]

select <- c(key(dt), features$featureCode)
#only grab colums with same code as featureCode (=mean and sd)
dt <- dt[, select, with=FALSE]

## Uses descriptive activity names to name the activities in the data set
activities <- fread(file.path('UCI HAR Dataset', "activity_labels.txt"))
setnames(activities, names(activities), c("activityNum", "activityName"))
dt <- merge(dt, activities, by.x="activityNum", by.y="activityNum", all.x=TRUE)

## Uses descriptive activity names to name the activities in the data set
setkey(dt, subject, activityNum, activityName)
dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))

dt <- merge(dt, features[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)
dt$activityName <- factor(dt$activityName)
dt$featureName <- factor(dt$featureName)

## create a second, independent tidy data set with the average of each variable for each activity and each subject
tidyData <- aggregate(dt[, 5], list(dt$subject, dt$activityName, dt$featureName), mean)
tidyData <- setnames(tidyData, c('subject', 'activity', 'feature', 'meanValue'))

## Make codebook

knit("makeCodebook.Rmd", output="codebook.md")
