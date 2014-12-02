# This R script has been created by Lizel Greyling in November 2014 in South Africa 
# as part of the required course work for the Coursera Data Science Specialization,
# specifically the Getting and Cleaning Data module.
# Approximately 5 liters of coffee and some wine was consumed in the process.

# The script creates a tidy dataset from obeservations collected from a number
# of Samsung Galaxy S smartphones. A full description of the data is available at: 
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones  

# The tidy data that is returned is a WIDE dataset. This is in line with the assignment
# rubric ("Has the student submitted a tidy data set? Either a wide or a long form of 
# the data is acceptable"), and I personally find wide datasets more user friendly.

# This script assumes that the Samsung dataset has been downloaded into the working directory
# of the computer that is running the script, per the assignment instructions.
# Specifically, it assumes that the working directory contains a folder called
# "UCI HAR Dataset" with subfolders "train" and "test".
# Although it would be more sensible in real life to put in better checks to ensure 
# that this assumption is in fact true, it was stated as a fact in the assignment
# instructions.

####################################################################################
## **        1: READ FILES AND MERGE TRAINING AND TEST DATASETS **                ##
####################################################################################
  
  library(data.table)   
  # data.table is needed in the final step to unable the use of lapply to summarize
  # data abd create tidy dataset.

  # Read the test and train observations and merge them into combined datasets: 
  # observations:
  x.train <- read.table("./UCI HAR Dataset/train/X_train.txt") 
  x.test <- read.table("./UCI HAR Dataset/test/X_test.txt")
  x.combined <- rbind(x.train, x.test) 
  
  # subjects:
  subject.train <- read.table("./UCI HAR Dataset/train/subject_train.txt") 
  subject.test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
  subject.combined <- rbind(subject.train, subject.test) 
  names(subject.combined) <- "Subject"   #Change the column name to "Subject"
  
  # activities:
  activity.train <- read.table("./UCI HAR Dataset/train/y_train.txt") 
  activity.test <- read.table("./UCI HAR Dataset/test/y_test.txt")
  activity.combined <- rbind(activity.train, activity.test)   
  names(activity.combined) <- "Activity"  #Change the column name to "Activity"
  activity.labels <- read.table("./UCI HAR Dataset/activity_labels.txt") 
  
  # read the file containing the variable names and add them to the dataset x.combined:
  features <- read.table("./UCI HAR Dataset/features.txt") 
  names(x.combined) <- features[,2]

####################################################################################  
## ** 2. Extract only the measurements on the mean and standard deviation for     ##
##       each measurement.                                                        ##
####################################################################################  
  # I decided to include only columns with the strings "mean()" or "std()" instead
  # of "mean" or "std", leaving me with 66 variables. There seems to be various 
  # opinions on this, but my reasoning was that these variables are the only ones 
  # that actually contains the calculation of either a mean or a standard deviation
  # from the data. 

  # get the index numbers of all variables containing the strings "mean()" or "std()":
  all.cols <- grep("mean\\()|std\\()", features[,2], ignore.case = F) 
  
  # select only the columns numbers contained in all.cols:
  x.mean.std.only <- x.combined[,all.cols]
  
  # Add columns for subject and activity to the main dataset (x.mean.std.only).
  # These columns are added now rather than previously, else they would have been
  # removed in the previous step, since they don't contain "mean" or "std".
  x.mean.std.only <- cbind(subject.combined, activity.combined, x.mean.std.only)
  
####################################################################################
## ** 3. Add descriptive activity names to name the activities in the data set,   ##
## by reading the activity descriptions from the activity_labels.txt file that    ##
## is supplied with the data and replacing the activity numbers in the dataset    ##
## with the descriptive labels.                                                   ##
####################################################################################

  x.mean.std.only$Activity <- factor(x.mean.std.only$Activity,levels=activity.labels$V1,labels=activity.labels$V2)
  
####################################################################################
## ** 4.   Appropriately label the data set with descriptive variable names.      ##
####################################################################################
  # Here I went all out trying to make the variable names look as nice as possible. 
  # Please agree that they do...

  # Remove "()"
  names(x.mean.std.only) <- gsub('\\(|\\)',"",names(x.mean.std.only), perl = TRUE)

  # Make names syntactically valid and ensure there are no duplicate column names
  names(x.mean.std.only) <- make.names(names(x.mean.std.only),unique=TRUE,allow_=TRUE)

  # Remove untidy repetitions 
  names(x.mean.std.only) <- gsub("BodyBody","Body",names(x.mean.std.only))
  
  # Add "." between name portions
  names(x.mean.std.only) <- gsub("mean","Mean",names(x.mean.std.only))
  names(x.mean.std.only) <- gsub("std","StdDev",names(x.mean.std.only))  
  names(x.mean.std.only) <- gsub("tBody","t.Body",names(x.mean.std.only))
  names(x.mean.std.only) <- gsub("tGravity","t.Gravity",names(x.mean.std.only))
  names(x.mean.std.only) <- gsub("fBody","f.Body",names(x.mean.std.only))
  names(x.mean.std.only) <- gsub("Gravity","Gravity.",names(x.mean.std.only))
  names(x.mean.std.only) <- gsub("Body","Body.",names(x.mean.std.only))
  names(x.mean.std.only) <- gsub("Acc","Acc.",names(x.mean.std.only))
  names(x.mean.std.only) <- gsub("Jerk","Jerk.",names(x.mean.std.only))
  names(x.mean.std.only) <- gsub("Gyro","Gyro.",names(x.mean.std.only))
  names(x.mean.std.only) <- gsub("Mag","Mag.",names(x.mean.std.only))
  names(x.mean.std.only) <- gsub("Mean\\.X","X.Mean",names(x.mean.std.only))
  names(x.mean.std.only) <- gsub("Mean\\.Y","Y.Mean",names(x.mean.std.only))
  names(x.mean.std.only) <- gsub("Mean\\.Z","Z.Mean",names(x.mean.std.only))
  names(x.mean.std.only) <- gsub("StdDev\\.X","X.StdDev",names(x.mean.std.only))
  names(x.mean.std.only) <- gsub("StdDev\\.Y","Y.StdDev",names(x.mean.std.only))
  names(x.mean.std.only) <- gsub("StdDev\\.Z","Z.StdDev",names(x.mean.std.only))
  names(x.mean.std.only) <- gsub("\\.\\.",".",names(x.mean.std.only))

####################################################################################
## ** 5. Create an independent tidy data set with the average of each variable    ##
##       for each activity and each subject.                                      ##
####################################################################################

  DT <- data.table(x.mean.std.only)
  # I used data.table because it makes the summarization so much easier.

  tidy.data <- DT[,lapply(.SD,mean),by="Subject,Activity"]
  write.table(tidy.data,file="./UCI HAR Dataset/tidy_data.txt",sep=",",row.names = FALSE)
  msg <- paste("SUCCESS! tidy_data.txt saved in ",getwd(),"/UCI HAR Dataset/",sep = "")  
  message(msg)

