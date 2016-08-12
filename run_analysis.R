library(dplyr)
library(tidyr)
library(data.table)

dataset_path <- "./UCI HAR Dataset/"

## Read training data including training set, training labels, and training subjects
train_set <- read.table(file.path(dataset_path, "train","X_train.txt"))
train_label <- read.table(file.path(dataset_path, "train","Y_train.txt"))
train_subjects <- read.table(file.path(dataset_path, "train", "subject_train.txt"))

## Read test data including test set, test labels, and test subjects
test_set <- read.table(file.path(dataset_path, "test","X_test.txt") )
test_label <- read.table(file.path(dataset_path, "test","Y_test.txt"))
test_subjects <- read.table(file.path(dataset_path, "test", "subject_test.txt"))

## Read variables and activity labels
variables <- read.table(file.path(dataset_path, "features.txt"))
activity_labels <- read.table(file.path(dataset_path, "activity_labels.txt"))


## Add subject and labels to the training and test set and re-arranging columns
train_set2 <- train_set %>%
    mutate(subject = as.numeric(train_subjects$V1))  %>%
    mutate(activity_label = as.numeric(train_label$V1)) %>%
    select(subject, V1:V561, activity_label)

test_set2 <- test_set %>%
    mutate(subject = as.numeric(test_subjects$V1))  %>%
    mutate(activity_label = as.numeric(test_label$V1)) %>%
    select(subject, V1:V561, activity_label)


## merge training and test data
merged_data <- rbind(train_set2, test_set2)


## Rename variables and use meaningful names
names(merged_data) <- c("subject",  as.character(variables$V2), "activity_label")


## Extract mean and std of measurements
mean_std_idx <- grep("mean|Std", names(merged_data), ignore.case=TRUE)
mean_std_data <- merged_data[,c(1,mean_std_idx, 563)]


## Uses descriptive activity names
mean_std_data$activity <- activity_labels[mean_std_data[,"activity_label"],2]
mean_std_data$activity <- as.factor(mean_std_data$activity)


new_dataset <- mean_std_data %>%
    group_by(activity, subject) %>%
    summarise_each(funs(mean))

print(new_dataset)
write.table(new_dataset, file = file.path(dataset_path, "new_tidy_dataset.txt"), row.name = FALSE)


    
        

