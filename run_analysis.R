#Code for Human Activity Recognition Dataset analysis

library(data.table)

#download data
fileurl = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'

if (!file.exists('./Human Activity Recognition Dataset.zip')){
  download.file(fileurl,'./Human Activity Recognition Dataset.zip', mode = 'wb')
  unzip("Human Activity Recognition Dataset.zip", exdir = getwd())
}

#load dataset into data frame
features <- read.csv('./Human Activity Recognition Dataset/features.txt', header = FALSE, sep = ' ')
features <- as.character(features[,2])

#training set preparation
data.train.x <- read.table('./Human Activity Recognition Dataset/train/X_train.txt')
data.train.activity <- read.csv('./Human Activity Recognition Dataset/train/y_train.txt', header = FALSE, sep = ' ')
data.train.subject <- read.csv('./Human Activity Recognition Dataset/train/subject_train.txt',header = FALSE, sep = ' ')
data.train <-  data.frame(data.train.subject, data.train.activity, data.train.x)
names(data.train) <- c(c('subject', 'activity'), features)

#testing set preparation
data.test.x <- read.table('./Human Activity Recognition Dataset/test/X_test.txt')
data.test.activity <- read.csv('./Human Activity Recognition Dataset/test/y_test.txt', header = FALSE, sep = ' ')
data.test.subject <- read.csv('./Human Activity Recognition Dataset/test/subject_test.txt', header = FALSE, sep = ' ')
data.test <-  data.frame(data.test.subject, data.test.activity, data.test.x)
names(data.test) <- c(c('subject', 'activity'), features)

#merge training set and testset
data.all <- rbind(data.train, data.test)

#extract mean and std
mean_std.select <- grep('mean|std', features)
data.sub <- data.all[,c(1,2,mean_std.select + 2)]

#names the activities
activity.labels <- read.table('./Human Activity Recognition Dataset/activity_labels.txt', header = FALSE)
activity.labels <- as.character(activity.labels[,2])
data.sub$activity <- activity.labels[data.sub$activity]

#labels the data set with descriptive variable names
name.new <- names(data.sub)
name.new <- gsub("[(][)]", "", name.new)
name.new <- gsub("^t", "TimeDomain_", name.new)
name.new <- gsub("^f", "FrequencyDomain_", name.new)
name.new <- gsub("Acc", "Accelerometer", name.new)
name.new <- gsub("Gyro", "Gyroscope", name.new)
name.new <- gsub("Mag", "Magnitude", name.new)
name.new <- gsub("-mean-", "_Mean_", name.new)
name.new <- gsub("-std-", "_StandardDeviation_", name.new)
name.new <- gsub("-", "_", name.new)
names(data.sub) <- name.new

#Create independent tidy data set
data.tidy <- aggregate(data.sub[,3:81], by = list(activity = data.sub$activity, subject = data.sub$subject),FUN = mean)
write.table(x = data.tidy, file = "data_tidy.txt", row.names = FALSE)
