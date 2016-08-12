# Getting and Cleaning Data Course Project

You should create one R script called *run_analysis.R* that does the following:

1) Merges the training and the test sets to create one data set.
2) Extracts only the measurements on the mean and standard deviation for each measurement.
3) Uses descriptive activity names to name the activities in the data set
4) Appropriately labels the data set with descriptive variable names.
5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

In the following subsections, I explained how step by step procedure of the above tasks. 

### Read training data including training set, training labels, and training subjects
    train_set <- read.table(file.path(dataset_path, "train","X_train.txt"))
    train_label <- read.table(file.path(dataset_path, "train","Y_train.txt"))
    train_subjects <- read.table(file.path(dataset_path, "train", "subject_train.txt"))

### Read test data including test set, test labels, and test subjects
    test_set <- read.table(file.path(dataset_path, "test","X_test.txt") )
    test_label <- read.table(file.path(dataset_path, "test","Y_test.txt"))
    test_subjects <- read.table(file.path(dataset_path, "test", "subject_test.txt"))

### Read variables and activity labels
    variables <- read.table(file.path(dataset_path, "features.txt"))
    activity_labels <- read.table(file.path(dataset_path, "activity_labels.txt"))


### Add subject and labels to the training and test set and re-arranging columns
    train_set2 <- train_set %>%
    mutate(subject = as.numeric(train_subjects$V1))  %>%
    mutate(activity_label = as.numeric(train_label$V1)) %>%
    select(subject, V1:V561, activity_label)

    test_set2 <- test_set %>%
    mutate(subject = as.numeric(test_subjects$V1))  %>%
    mutate(activity_label = as.numeric(test_label$V1)) %>%
    select(subject, V1:V561, activity_label)


### Merge training and test data
    merged_data <- rbind(train_set2, test_set2)


### Rename variables and use meaningful names
    names(merged_data) <- c("subject",  as.character(variables$V2), "activity_label")


### Extract mean and std of measurements
    mean_std_idx <- grep("mean|Std", names(merged_data), ignore.case=TRUE)
    mean_std_data <- merged_data[,c(1,mean_std_idx, 563)]


### Use descriptive activity names
    mean_std_data$activity <- activity_labels[mean_std_data[,"activity_label"],2]
    mean_std_data$activity <- as.factor(mean_std_data$activity)


### Create new data set
    new_dataset <- mean_std_data %>%
    group_by(activity, subject) %>%
    summarise_each(funs(mean))

   print(new_dataset)
   write.table(new_dataset, file = file.path(dataset_path, "new_tidy_dataset.txt"))


    
        


