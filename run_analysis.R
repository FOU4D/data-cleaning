# Data Cleaning course
# Final Project

# getting all variables ready 
vars <- ls()
rm(list = vars)

# a function to deal with directory path
datapath <- function(...) { paste(..., sep = "/") }

# donwloading data set and set directories

# working data directories
datadir <- datapath("./data")
datazipfile <- datapath(datadir, "Dataset.zip")

# URL to download data
originurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# the unzipped directory name
dataunzipped <- datapath(datadir, "UCI HAR Dataset")

# create a data directory if not found
if (!file.exists(datadir)) {dir.create(datapath("./data"))}
# check if there's a data-set downloaded or to download it
if (!file.exists(datazipfile)) {download.file(originurl, destfile = datazipfile, method = "curl")}
# unzip the zipped file if not unzipped already
if (!file.exists(dataunzipped)) {unzip(datazipfile, exdir = datadir)}

# framing x_train and x_test files
# train directories
datatrainpath <- datapath(dataunzipped, "train")
# Data Train
datatrainx <- read.table(datapath(datatrainpath, "X_train.txt"))
datatrainy <- read.table(datapath(datatrainpath, "y_train.txt"))
datatrainsubject <- read.table(datapath(datatrainpath, "subject_train.txt"))

# Data test
# test directories
datatestpath <- datapath(dataunzipped, "test")
datatestx <- read.table(datapath(datatestpath, "X_test.txt"))
datatesty <- read.table(datapath(datatestpath, "y_test.txt"))
datatestsubject <- read.table(datapath(datatestpath, "subject_test.txt"))

# Data Features
datafeaturesx <- read.table(datapath(dataunzipped, "features.txt"))


# preparing data
# features
datafeaturesx <- as.character(datafeaturesx[,2])
# train
datatrain <-  data.frame(datatrainsubject, datatrainy, datatrainx)
names(datatrain) <- c(c('subject', 'activity'), datafeaturesx)
# test
datatest <-  data.frame(datatestsubject, datatesty, datatestx)
names(datatest) <- c(c('subject', 'activity'), datafeaturesx)


# 1. Merges the training and the test sets to create one data set.

alldata <- rbind(datatest, datatrain)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
alldata.meanstd <- grep('mean|std', names(alldata))
alldata.sub <- alldata[,c(1,2,alldata.meanstd + 2)]


# 3. Uses descriptive activity names to name the activities in the data set
activitylabels <- read.table(datapath(dataunzipped,"activity_labels.txt"), header = FALSE)
activitylabels <- as.character(activitylabels[,2])
alldata.sub$activity <- activitylabels[alldata.sub$activity]

# 4. Appropriately labels the data set with descriptive variable names. 

name.new <- names(alldata.sub)
name.new <- gsub("[(][)]", "", name.new)
name.new <- gsub("^t", "TimeDomain_", name.new)
name.new <- gsub("^f", "FrequencyDomain_", name.new)
name.new <- gsub("Acc", "Accelerometer", name.new)
name.new <- gsub("Gyro", "Gyroscope", name.new)
name.new <- gsub("Mag", "Magnitude", name.new)
name.new <- gsub("-mean-", "_Mean_", name.new)
name.new <- gsub("-std-", "_StandardDeviation_", name.new)
name.new <- gsub("-", "_", name.new)
names(alldata.sub) <- name.new              



# 5. from the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

alldata.tidy <- aggregate(alldata.sub[,3:81], by = list(activity = alldata.sub$activity, subject = alldata.sub$subject),FUN = mean)
write.table(x = alldata.tidy, file = "alldata_tidy.txt", row.names = FALSE)

