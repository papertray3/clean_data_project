getData <- function(dir="./UCI HAR Dataset/") {
  labels <- read.table(file.path(dir, "features.txt"))

  test <- read.table(file.path(dir, "test", "X_test.txt"))
  names(test) <- labels[,2]
  stest <- read.table(file.path(dir, "test", "subject_test.txt"))
  names(stest) <- c("subject")
  vtest <- read.table(file.path(dir,"test", "y_test.txt"))
  names(vtest) <- c("activity")
  
  test_final <- cbind(test, stest, vtest)
  
  
  train <- read.table(file.path(dir, "train", "X_train.txt"))
  names(train) <- labels[,2]
  strain <- read.table(file.path(dir, "train", "subject_train.txt"))
  names(strain) <- c("subject")
  vtrain <- read.table(file.path(dir,"train", "y_train.txt"))
  names(vtrain) <- c("activity")

  train_final <- cbind(train, strain, vtrain)
  
  final <- rbind(test_final, train_final)
  
  
  features <- labels_of_interest <- grepl("-mean\\(\\)|-std\\(\\)", labels[,2])
  #retrieve the subject and activity
  append(features, c(T,T))
  activity <- read.table(file.path(dir, "activity_labels.txt"))
  
  final$activity <- factor(final$activity, labels=activity$V2)
  
  final[,features]
  
  
}

getTidy <- function(data) {
  subjects <- unique(data$subject)
  activities <- unique(data$activity)
  rows = length(subjects) * length(activities)
  num_features <- length(names(data))
  
  m <- matrix(nrow=rows,ncol=num_features)
  num_features <- num_features - 2
  
  idx <- 1
  for(subject in subjects) {
    
    for(activity in activities) {
      d <- colMeans(data[(data$subject == subject & data$activity == activity), 1:num_features])
      d<-append(d, c(subject, activity))
      m[idx,] <- d
      idx <- idx + 1
    }
  }
  
  ret <- data.frame(m)
  names(ret) <- names(data)
  ret
}

run_analysis <- function(dir="./UCI HAR Dataset/") {
  d <- getData(dir)
  getTidy(d)
}