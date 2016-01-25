##Loading libraries
library(dplyr)

##Reading both training and testing data
testdata <- read.table("D:/RWSPACE/project3/test/X_test.txt")
traindata <- read.table("D:/RWSPACE/project3/train/X_train.txt")
ytest <- read.table("D:/RWSPACE/Project3/test/y_test.txt")
subtest <- read.table("D:/RWSPACE/Project3/test/subject_test.txt")
ytrain <- read.table("D:/RWSPACE/Project3/train/y_train.txt")
subtrain <- read.table("D:/RWSPACE/Project3/train/subject_train.txt")
##Rename columns of the sets containing activities and subjects
colnames(ytest)[1] <- "activity"
colnames(subtest)[1] <- "subject"
colnames(ytrain)[1] <- "activity"
colnames(subtrain)[1] <- "subject"

##Merge the data 563vriables , 10299 entries
testingdata <- cbind.data.frame(testdata,ytest,subtest)
trainingdata <- cbind.data.frame(traindata,ytrain,subtrain)
merged <- merge(testingdata,trainingdata,all = TRUE)

##Read the features data
features <- read.table("D:/RWSPACE/project3/features.txt")
##Positions is a list ofindexes related to variables containing mean()/std()
positions <- grep("mean()|std()",features$V2)
##Add the positions of activity_label and subject
pos <- append(positions,562:563)

##Extract the variables with mean() and std() 
dfinal <- merged[,pos]

##name the activities in the data set with descriptive names
activset <- read.table("D:/RWSPACE/project3/activity_labels.txt")
##Function matchit that matches each label to its activity name
##taking for argumets 
##ds : table mapping label/activity_name
##vect : vector of labels to be renamed with descriptive activity names
##result ; the vector resulting (renamed vect)
matchit <- function(ds,vect, result="meaningful"){
      for(i in seq_along(vect)){
            element <- as.numeric(vect[i])
            result[i] <- as.character(ds[element,2])
      }
      result
}
dfinal$activity <- matchit(activset,dfinal$activity)

##label the data set with descriptive variables names
activnames <- grep("mean()|std()",features$V2,value = TRUE)
names(dfinal)[1:79] <- activnames

##remove special characters "()" and "-" from names
names(dfinal) <- gsub("-",".",names(dfinal))
names(dfinal) <- gsub("[(][)]","",names(dfinal))

##Grouping the final data set by activity and subject
## then create a new tidy with the average of each
##measurement,each activity and each subject
dfinal <- group_by(dfinal,subject,activity)
tidyset <- summarise_each(dfinal,funs(mean))
write.table(tidyset,"D:/RWSPACE/project3/samsung_tidyset.txt",row.names = FALSE)
