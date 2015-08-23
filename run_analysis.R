## Porject
    # The purpose of this project is to demonstrate your ability to collect, work 
    # with, and clean a data set. The goal is to prepare tidy data that can be used 
    # for later analysis. You will be graded by your peers on a series of yes/no 
    # questions related to the project. You will be required to submit: 1) a tidy 
    # data set as described below, 2) a link to a Github repository with your script 
    # for performing the analysis, and 3) a code book that describes the variables, 
    # the data, and any transformations or work that you performed to clean up the 
    # data called CodeBook.md. You should also include a README.md in the repo with 
    # your scripts. This repo explains how all of the scripts work and how they are 
    # connected. 
    
    # One of the most exciting areas in all of data science right now is wearable 
    # computing - see for example this article . Companies like Fitbit, Nike, and 
    # Jawbone Up are racing to develop the most advanced algorithms to attract new 
    # users. The data linked to from the course website represent data collected 
    # from the accelerometers from the Samsung Galaxy S smartphone. A full 
    # description is available at the site where the data was obtained:
    # http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
    
    # Here are the data for the project:
    # https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
    
    # You should create one R script called run_analysis.R that does the following. 
        # Merges the training and the test sets to create one data set.
        # Extracts only the measurements on the mean and standard deviation for 
            # each measurement. 
        # Uses descriptive activity names to name the activities in the data set
        # Appropriately labels the data set with descriptive variable names. 
    
    # From the data set in step 4, creates a second, independent tidy data set with 
    # the average of each variable for each activity and each subject.


## Script
# Set path - See step 1 of readme
    # setwd("~/Repositories/Coursera/GettingAndCleaningData/Project")
    path<-getwd()

# Get the data (executed at 23-08-2015)
    # Download the zip file
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    if (!file.exists(path)) {dir.create(path)}
    download.file(url, file.path(path, "Dataset.zip"))
    
    # Unzip the files
    dirzip <- file.path("C:", "Program Files", "7-Zip", "7z.exe")
    parameters <- "x"
    cmd <- paste(paste0("\"", dirzip, "\""), parameters, paste0("\"", file.path(path, "Dataset.zip"), "\""))
    system(cmd)
    
    # Set working directory to the map containing the data
    setwd(file.path(path, "UCI HAR Dataset"))

# Merges the training and the test sets to create one data set
    # General data sets
    features = read.table("features.txt")
    activities <- read.table("activity_labels.txt")
    
    # Read training data set
    x_train <- read.table("train/X_train.txt")
    y_train <- read.table("train/y_train.txt")
    subject_train <- read.table("train/subject_train.txt")
    
    # Assign column names
    colnames(subject_train)  = "subject";
    colnames(x_train)        = features[,2]; 
    colnames(y_train)        = "activity";
    
    # make the training data set
    train_data = cbind(y_train ,subject_train,x_train );
    
    # Read test data set
    x_test <- read.table("test/X_test.txt")
    y_test <- read.table("test/y_test.txt")
    subject_test <- read.table("test/subject_test.txt")
    
    # Assign column names
    colnames(subject_test) = "subject";
    colnames(x_test)       = features[,2]; 
    colnames(y_test)       = "activity";
    
    # make the test data set
    test_data = cbind(y_test,subject_test,x_test);

    # Merge training and test set
    all_data <- rbind(train_data,test_data)

# Extracts only the measurements on the mean and standard deviation for each measurement
    # get only columns with mean() or std() in their names
    mean_and_std_features <-(grepl("activity",names(all_data)) | grepl("subject",names(all_data)) | grepl("-(mean|std)\\(\\)", names(all_data)))
    
    # Subset the complete data set to keep the desired columns
    all_data <- all_data[, mean_and_std_features]

# Uses descriptive activity names to name the activities in the data set
    all_data[, 1] <- activities[all_data[, 1], 2]

# Appropriately labels the data set with descriptive variable names
    names(all_data)<-gsub("^t", "time", names(all_data))
    names(all_data)<-gsub("^f", "frequency", names(all_data))
    names(all_data)<-gsub("Acc", "Accelerometer", names(all_data))
    names(all_data)<-gsub("Gyro", "Gyroscope", names(all_data))
    names(all_data)<-gsub("Mag", "Magnitude", names(all_data))
    names(all_data)<-gsub("BodyBody", "Body", names(all_data))
    
    
    # From the data set in step 4, creates a second, independent tidy data set with 
    # the average of each variable for each activity and each subject  
    tidy_data    = aggregate(all_data[,names(all_data) != c('activity','subject')],by=list(activity=all_data$activity,subject = all_data$subject),mean)

    write.table(tidy_data, file.path(path,"./tidyData.txt"),row.names=TRUE,sep='\t');
    
