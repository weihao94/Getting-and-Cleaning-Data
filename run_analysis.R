#Title: Run Analysis
#Author: Wei Hao Khoong
#Date: 28 May 2018

library(dplyr)

#read train data
X_train <- read.table("X_train.txt")
Y_train <- read.table("Y_train.txt")
Sub_train <- read.table("subject_train.txt")

#read test data
X_test <- read.table("X_test.txt")
Y_test <- read.table("Y_test.txt")
Sub_test <- read.table("subject_test.txt")

# Obtaining the Variable Descriptions & activity levels
variable_names <- read.table("features.txt")
activity_labels <- read.table("activity_labels.txt")

# Merging the data
X_total <- rbind(X_train, X_test)
Y_total <- rbind(Y_train, Y_test)
Sub_total <- rbind(Sub_train, Sub_test)

# Extracting only the Measurements on the Mean and Standard Deviation for each Measurement
selected_var <- variable_names[grep("mean\\(\\)|std\\(\\)",variable_names[,2]),]
X_total <- X_total[,selected_var[,1]]

# Using Descriptive Activity Names to Name the Activities in the Data Set
colnames(Y_total) <- "activity"
Y_total$label <- factor(Y_total$activity, 
                        labels = as.character(activity_labels[,2]))
Y_total$activitylabel <- factor(Y_total$activity, 
                                labels = as.character(activity_labels[,2]))
activitylabel <- Y_total[,-1]

# Appropriately Labeling the Dataset with Descriptive Variable Names
colnames(X_total) <- variable_names[selected_var[,1],2]

# Independent Tidy Dataset
total <- cbind(X_total, Y_total)
total_mean <- total %>% group_by(label) %>% summarize_each(funs(mean))
write.table(total_mean, file = "tidydata.txt", row.names = FALSE, col.names = FALSE)
colnames(Sub_total) <- "subject"
total <- cbind(X_total, activitylabel, Sub_total)
total_mean <- total %>% group_by(activitylabel, subject) %>% summarize_each(funs(mean))
write.table(total_mean, file = "tidydata.txt", row.names = FALSE, col.names = TRUE)