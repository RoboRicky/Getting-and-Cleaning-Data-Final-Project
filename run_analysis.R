## In this assignment, we will (1) gather data from disparate files into a single
## dataset. (2) Extract only the mean and standard deviation for each measurement
## (3) Use descriptive activity names for the data set. (4) Appropriately label
## dataset with descriptive variable names. (5) From the data in step 4, create
## a second, independent tidy data set with the averages of each variable for each
## activity and each subject.

## Additionally, I've decided not to use any external packages for this script,
## to ensure that it is functional and readable for anyone with only the basics
## of R.

## PART 1 : Import & combine data ##

## Get test and train data. Put activity number and subject number as first 2 columns. 
totaldata<-rbind(cbind(read.table("UCI HAR Dataset\\test\\y_test.txt"),
                       read.table("UCI HAR Dataset\\test\\subject_test.txt"),
                       read.table("UCI HAR Dataset\\test\\X_test.txt")), 
                 cbind(read.table("UCI HAR Dataset\\train\\y_train.txt"),
                      read.table("UCI HAR Dataset\\train\\subject_train.txt"),
                      read.table("UCI HAR Dataset\\train\\X_train.txt"))
)


##Get "features" as a vector - the features are column names
features<-t(read.table("UCI HAR Dataset\\features.txt", 
                       stringsAsFactors = FALSE)[,2])

## set column names for activity, subject, and feature columns.
features<- c("activity_number", "subject_number", features)

colnames(totaldata)<- features

## Preserve our collected data for future perusal and use by saving it as a csv 
## (Optional, uncomment to use)
## write.csv(totaldata, "UCI_HAR_test_&_train_data_combined.csv")

## Now we have our original test & train data gathered into one table with
## the original labels, and the option to save it back into a csv for safekeeping.

## However, going forward from here we are only interested in certain fields - 
## the mean and standard deviation for each measurement. The descriptiveness of 
## the original labels also still leaves something to be desired, and the
## activities are still only identified by number.


## PART 2 : Select desired fields ##

## In deciding which fields are desired: I am only including columns that are
## the mean or standard deviation of a measurement, and not the set of angles
## between the body and gravity means listed at the end, as the angles are a
## measurement of the means rather than the mean of a measurement and could be
## easily recreated if needed from the mean values themselves.

## Use grep to figure out which columns contain mean or standard deviation
## values and create an ordered list of desired columns. 
colselect<-c(1, 2, grep("mean\\(\\)|meanFreq\\(\\)|std\\(\\)", colnames(totaldata)))

## Now create a dataframe with just the desired data. There should be 68 columns.
mydata<- totaldata[,colselect]

## So now we have the data we want, but we still need to fix these unhelpful labels.


## PART 3 : Descriptive activity labels ##

##Get the activity labels key
activity_labels<- t(read.table("UCI HAR Dataset\\activity_labels.txt", 
                               stringsAsFactors = FALSE)[,2])

## Convert to lowercase
activity_labels<-tolower(activity_labels)

## Change "walking" to "walking_flat" to be more distinct from upstairs/downstairs
activity_labels[1]<-"walking_flat"

## Make an activity labels column and add to the front of mydata
activity_column<- sapply(mydata$activity_number, 
                             function(x){activity_labels[x]})

mydata<-cbind("activity_name"=activity_column, mydata)

## The activities now have descriptive labels. Next is fixing the column names.


## PART 4 : Descriptive column names ##

## The selected feature names all use a formula: 
## (t|f)(body|gravity)(acc|gyro)(?jerk)(?mag)(-mean()|-meanFreq()|-std())(?-X|Y|Z)
## (t|f): time domain or frequency domain
## (body|gravity): signal from body or signal from gravity. BodyBody is a typo.
## (acc|gyro): signal from accelerometer (acceleration) or from gyroscope (rotation)
## (?jerk): if measure of jerk
## (?mag): if measure of magnitude
## (-mean()|-meanFreq()|-std()): measurement of mean or standard deviation
## (?-X|Y|Z): axis of signal, if relevant - X, Y, or Z.

## This naming scheme is not very readable.

## Get the column names
columns<-colnames(mydata)

## Make a vector of patterns to replace, and a vector of corresponding replacements
patterns<-c("^t", "^f", "BodyBody", "Acc", "Gyro", "Mag", "std", "meanFreq", "\\(\\)")

replacements<-c("time", "frequency","Body", "Accelerometer", "Gyroscope", 
                "Magnitude", "standardDeviation", "meanFrequency", "")

## Make all replacements for each column name (with 82 columns).
for(i in 1:82){
        for(j in 1:9){
                columns[i]<-sub(patterns[j], replacements[j], columns[i], fixed=FALSE)
        }
}

## Set the column names to the improved list of names
colnames(mydata)<-columns

## Now the column names are human-readable and reasonably descriptive.

## Preserve nicely-labeled mydata for future perusal and use by saving it as a csv 
## (Optional, uncomment to use)
## write.csv(mydata, "UCI_HAR_mean_meanFreq_and_std_values.csv")


## PART 5: Create an independent tidy data set ##

## The last thing needed is to average the values for each subject-activity
## combination and use them to create an independent tidy data set. We know 
## that there are 30 subjects who each performed 6 activities, so our final data 
## set should be 180 rows.


## Use aggregate to retrieve mean for each subject-activity pair.The activity 
## number column is dropped, so final data set will be 81 columns.
tidyavgs<-aggregate(mydata[4:82], by=list(subject = mydata$subject_number, 
                                         activity = mydata$activity_name), mean)

## Tidy data set complete. Output to file.
write.table(tidyavgs,"UCI_HAR_Average_Mean_and_Std_Values_for_Subject-Activity_Pairs.txt", row.names=FALSE)
