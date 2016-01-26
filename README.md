Wearable-computing - Data science specialization by Coursera - Getting and cleaning data course project - Create a tidy set for human activity recognition analysis based on data collected from Samsung galaxy smartphones  

One of the most exciting areas in all of data science right now is wearable computing.Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
Here are the data for the project:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The R script "run_analysis.R" does the following :

1-Merges the training and the test sets to create one data set.
2-Extracts only the measurements on the mean and standard deviation for each measurement.
3-Uses descriptive activity names to name the activities in the data set
4-Appropriately labels the data set with descriptive variable names.
5-From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Find below detailed guide that explains the code :

I - Part1 : Merge the training and the test sets to create one data set
-----------------------------------------------------------------------

1-Reading the 6 files containing both training and testing data
***************************************************************

testdata <- read.table("D:/RWSPACE/project3/test/X_test.txt")
traindata <- read.table("D:/RWSPACE/project3/train/X_train.txt")
ytest <- read.table("D:/RWSPACE/Project3/test/y_test.txt")
subtest <- read.table("D:/RWSPACE/Project3/test/subject_test.txt")
ytrain <- read.table("D:/RWSPACE/Project3/train/y_train.txt")
subtrain <- read.table("D:/RWSPACE/Project3/train/subject_train.txt")

2-Rename columns of the sets containing activities and subjects
***************************************************************

colnames(ytest)[1] <- "activity"
colnames(subtest)[1] <- "subject"
colnames(ytrain)[1] <- "activity"
colnames(subtrain)[1] <- "subject"


3-Merge the data 563vriables , 10299 entries
********************************************

testingdata <- cbind.data.frame(testdata,ytest,subtest)
trainingdata <- cbind.data.frame(traindata,ytrain,subtrain)
merged <- merge(testingdata,trainingdata,all = TRUE)



II - Part2 : Extract only the measurements on the mean and standard deviation for each measurement
--------------------------------------------------------------------------------------------------

1- Read the features data
*************************

features <- read.table("D:/RWSPACE/project3/features.txt")

2- Positions is a list of indexes related to variables containing mean()/std()
******************************************************************************

positions <- grep("mean()|std()",features$V2)


3- Add the positions of activity_label and subject and assign to "pos"
*********************************************************************

pos <- append(positions,562:563)


4- Extract the variables with mean() and std(), the new datset is called "dfinal"
*********************************************************************************

dfinal <- merged[,pos]


III- Part3 : Use descriptive activity names to name the activities in the data set
----------------------------------------------------------------------------------

1-Read the "activity_labels" file mapping the activity name with label number
*****************************************************************************

activset <- read.table("D:/RWSPACE/project3/activity_labels.txt")


2-Write a function matchit that matches each label to its activity name taking for argumets:
ds : table mapping label/activity_name
vect : vector of labels to be renamed with descriptive activity names
result ; the vector resulting (renamed vect)
********************************************************************************************

matchit <- function(ds,vect, result="meaningful"){
      for(i in seq_along(vect)){
            element <- as.numeric(vect[i])
            result[i] <- as.character(ds[element,2])
      }
      result
}


3-Rename the activity column values with descriptive activity names
*******************************************************************

dfinal$activity <- matchit(activset,dfinal$activity)


IV- Part4 : Appropriately labels the data set with descriptive variable names
-----------------------------------------------------------------------------

activnames <- grep("mean()|std()",features$V2,value = TRUE)
names(dfinal)[1:79] <- activnames


V- Part5 : From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
---------------------------------------------------------------------------------------------------------------------------------------------------------

1-remove special characters "()" and "-" from names
***************************************************

names(dfinal) <- gsub("-",".",names(dfinal))
names(dfinal) <- gsub("[(][)]","",names(dfinal))


2-Grouping the final data set by activity and subject
*****************************************************

dfinal <- group_by(dfinal,subject,activity)

3-create a new tidy set "tidyset" (the output of the code" with the average of each measurement,each activity and each subject
******************************************************************************************************************************

tidyset <- summarise_each(dfinal,funs(mean))
tidyset

