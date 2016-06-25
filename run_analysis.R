run <- function()
{
  #Reading the data and naming column IDs
  
  sub_test <- read.table("getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
  sub_train <- read.table("getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
  x_train <- read.table("getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
  y_train <- read.table("getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
  x_test<- read.table("getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
  y_test <- read.table("getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
  feature <- read.table("getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/features.txt", col.names = c("feature_id","featurelabel"))
  activities <- read.table("getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/activity_labels.txt", col.names = c("activityid","activitylabel"))

  # Merging the data
  subject <- rbind(sub_test,sub_train)
  names(subject) <- c("SubjectID")
  joinData <- rbind(x_train,x_test)
  values <- rbind(y_test,y_train)
  names(values) <- c("activityid")
  dim(joinData)
  
  
  head(activities)
 # Cleaning data and only taking values with mean and STD
  Indices <- grep("mean\\(\\)|std\\(\\)", feature[, 2])
  length(Indices) # 66
  joinData <- joinData[, Indices]

  names(joinData) <- gsub("\\(\\)", "", feature[Indices, 2]) # remove "()"
  names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
  names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
  names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names 
#Creating and writing merged data
  combineddata <- cbind(subject,joinData,values)
  cdat <- merge(activities,combineddata,by="activityid")
  cdat <- cdat[,-1]
  write.table(cdat, "merged_data.txt")
 
   #Creating and writing TIdy Data
  tidyData <- aggregate(. ~SubjectID + activitylabel, cdat, mean)
  tidyData <- tidyData[order(tidyData$SubjectID,tidyData$activitylabel),]
  head(tidyData)
  write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
}
  
