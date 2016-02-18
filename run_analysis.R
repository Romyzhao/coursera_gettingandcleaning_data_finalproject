##########################################################################################################

## Coursera Getting and Cleaning Data Course Project - 2016-02-18
## Xuanyi Zhao

#Question
#One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#Here are the data for the project:

#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names.
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##########################################################################################################


#Download the data
if(!file.exists("./data")){dir("./data")}
fileURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL,destfile="./data/finalproject.zip")

#unzip files
unzip(zipfile="./data/finalproject.zip",exdir="./data")

#List the files in the unzipped folder
dir<-file.path("./data", "UCI HAR Dataset")
file_list<-list.files(dir,recursive = TRUE)
file_list

#Read the X data - files with feature information
setwd("C:/Users/Administrator/Documents/data/UCI HAR Dataset")
x_Train<-read.table("./train/x_train.txt",header=FALSE)
x_Test<-read.table("./test/x_test.txt",header=FALSE)

#check the data's variable and variable feature 
names(x_Train)
str(x_Train)
names(x_Test)
str(x_Test)



#2) read the Y data - files with activity information 
y_Train<-read.table("./train/y_train.txt",header=FALSE)
y_Test<-read.table("./test/y_test.txt",header=FALSE)

#check the data's variable and variable feature
names(y_Train)
str(y_Train)
names(y_Test)
str(y_Test)



#read the subject data - train and test
subject_train<-read.table("./train/subject_train.txt",header=FALSE)
subject_test<-read.table("./test/subject_test.txt",header=FALSE)

#check the data's variable and variable feature
str(subject_train)
str(subject_test)


#read the other description data
features<-read.table("./features.txt",header=FALSE)
str(features)
count(features$V2)





#1. Merges the training and the testing sets to create one data set.

#1) Combine the "activity","feature" and "subject" data by rows
X_Feature<-rbind(x_Train,x_Test)
names(X_Feature)
str(X_Feature)

Y_Activity<-rbind(y_Test,y_Train)
str(Y_Activity)

Subject<-rbind(subject_train,subject_test)
str(Subject)

#2) Assign the variable name to the combined data

# X_Feature variable name
names(X_Feature)<-features$V2
names(X_Feature)

#For Y_Activity, just named as activity
names(Y_Activity)<-c("activity")
str(Y_Activity)

#For subject, name the variable as "subject"
names(Subject)<-c("subject")
str(Subject)

#3) merge the 3 files

#merge the subject and Y_Activity by column and then merged with X_Features
Combine_Data1<-cbind(Y_Activity,Subject)
str(Combine_Data1)
#merge the "Combine_Data1" with X_Feature
Merged_Data<-cbind(X_Feature, Combine_Data1)
str(Merged_Data)


#2. Extracts only the measurements on the mean and standard deviation for each measurement.
#subset the data by the variable name with "mean" and "standard"

Subset_DataNames<-features$V2[grep(".*mean.*|.*std.*",features$V2,ignore.case=TRUE)]


#Subset the Merged_Data, only select the names of Subset_DataNames with "subjest" field and "activity" field

#create data frame with the selected columns name
Subset_Data<-c(as.character(Subset_DataNames),"subject","activity")
str(Subset_Data)
#Subset the "Merged_Data" by only selecting the data field names stored in "Subset_Data"
Merged_Data1<-subset(Merged_Data,select=Subset_Data)

#Check the subset data, there are 35 fields in the subset data, which match the count of Subset_Data
str(Merged_Data1)

#3. Uses descriptive activity names to name the activities in the data set

#read activity_lable data
activity_label<-read.table("./activity_labels.txt",header=FALSE)

#Name the columns
colnames(activity_label)<-c("activity","activity_name")
str(activity_label)
str(Merged_Data1)

#merge the result table from step1 - "merge" with the activity_label, and pull all the "merge" table's recrods 
Merged_Data2<-merge(Merged_Data1,activity_label,by='activity',all.x=TRUE)
str(Merged_Data2)
head(Merged_Data2$activity_name)

head(Merged_Data2$activity)
head(activity_label)

#remove the activity field
Merged_Data2$activity<-NULL
str(Merged_Data2)
table(Merged_Data2$activity_name)

#rename the "activity_name" to "activity"
colnames(Merged_Data2)[which(names(Merged_Data2)=="activity_name")]<-"activity"
str(Merged_Data2$activity)
str(Merged_Data1)
str(Merged_Data2)


#4 Appropriately labels the data set with descriptive variable names.

names(Merged_Data2)<-gsub("Acc", "Accelerometer", names(Merged_Data2))
names(Merged_Data2)<-gsub("Gyro", "Gyroscope", names(Merged_Data2))
names(Merged_Data2)<-gsub("BodyBody", "Body", names(Merged_Data2))
names(Merged_Data2)<-gsub("Mag", "Magnitude", names(Merged_Data2))
names(Merged_Data2)<-gsub("^t", "Time", names(Merged_Data2))
names(Merged_Data2)<-gsub("^f", "Frequency", names(Merged_Data2))
names(Merged_Data2)<-gsub("tBody", "TimeBody", names(Merged_Data2))
names(Merged_Data2)<-gsub("-mean()", "Mean", names(Merged_Data2), ignore.case = TRUE)
names(Merged_Data2)<-gsub("-std()", "STD", names(Merged_Data2), ignore.case = TRUE)
names(Merged_Data2)<-gsub("-freq()", "Frequency", names(Merged_Data2), ignore.case = TRUE)
names(Merged_Data2)<-gsub("angle", "Angle", names(Merged_Data2))
names(Merged_Data2)<-gsub("gravity", "Gravity", names(Merged_Data2))
names(Merged_Data2)

#5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Merged_Data2$subject<-as.factor(Merged_Data2$subject)

Tidy_Data <- aggregate(. ~subject + activity, Merged_Data2, mean)
str(Tidy_Data)
head(Tidy_Data)
tail(Tidy_Data)

write.table(Tidy_Data,"./tidydata.txt",row.names=TRUE)
