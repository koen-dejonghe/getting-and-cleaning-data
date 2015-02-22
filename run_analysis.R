
read_features <- function (fname){
  t <- read.table(file = fname, sep = ' ')
  colnames(t) <- c('id', 'feature')
  # filter out mean and std features
  s <- subset(t, grepl('(?:mean|std)\\(\\)', feature))
}

read_labels <- function(fname){
  t <- read.table(file = fname, sep = ' ')
  colnames(t) <- c('id', 'activity')
  t
}

read_measurements <- function(fname, features) {
  # read file
  m <- read.table(file = fname, sep = '', encoding = 'UTF-8')
  # View(m)
  
  # only interested in mean and std features
  mf <- m[, features$id]
  
  # assign column labels
  colnames(mf) <- features$feature
  
  mf
}

read_response <- function(fname, labels){
  t <- read.table(file = fname)
  colnames(t) <- c('activity')
  t$activity <- factor(t$activity, levels = c (1 : max(labels$id)), labels = labels$activity)
  t
}

read_subjects <- function(fname){
  t <- read.table(file = fname)
  colnames(t) <- c('subject')
  t
}

read_observations <- function(measurement_file, response_file, subject_file, features, labels){
  measurements <- read_measurements(measurement_file, features)
  responses <- read_response(response_file, labels)
  subjects <- read_subjects(subject_file)
  # join the 3
  cbind(measurements, responses, subjects)
}


#---------------------------------------------------------------------
# main
#---------------------------------------------------------------------
features <- read_features('UCI HAR Dataset/features.txt')
# print(features)

labels <- read_labels('UCI HAR Dataset/activity_labels.txt')
# print (labels)

# read training set
training_set <- read_observations(
  'UCI HAR Dataset/train/X_train.txt.100', 
  'UCI HAR Dataset/train/y_train.txt.100', 
  'UCI HAR Dataset/train/subject_train.txt.100',
  features, labels)

# read test set
test_set <- read_observations(
  'UCI HAR Dataset/test/X_test.txt', 
  'UCI HAR Dataset/test/y_test.txt', 
  'UCI HAR Dataset/test/subject_test.txt',
  features, labels)

full_set <- rbind(training_set, test_set)
# View(full_set)

tidy_set <- aggregate (full_set[, 1:66], list(activity = full_set$activity, subject = full_set$subject), mean)
View(tidy_set)

