runAnalysis <- function(filepath="Documents/data"){
    library(plyr)
    
    # Gather and merge data for "test" group, including activity labels, subject ids, and group type
    test_data <- read.table(paste(filepath, "/test/X_test.txt", sep=""))
    test_data$label <- as.numeric(read.table(paste(filepath, "/test/Y_test.txt", sep=""))[,1])
    test_data$subject <- as.numeric(read.table(paste(filepath, "/test/subject_test.txt", sep=""))[,1])
    test_data$group <- "Test"
    
    # Gather and merge data for "train" group, including activity labels, subject ids, and group type
    train_data <- read.table(paste(filepath, "/train/X_train.txt", sep=""))
    train_data$label <- as.numeric(read.table(paste(filepath, "/train/Y_train.txt", sep=""))[,1])
    train_data$subject <- as.numeric(read.table(paste(filepath, "/train/subject_train.txt", sep=""))[,1])
    train_data$group <- "Train"
    
    #Bind test and train data into single data frame.
    full_data <- rbind(train_data, test_data)
    
    #Name columns with descriptive names from features.txt file, plus the variables defined above.
    headers <- c(as.vector(read.table(paste(filepath, "/features.txt", sep=""))[,2]), "label", "subject", "group")
    names(full_data) <- headers

    # Use REGEX to identify variables concerning mean and standard deviation. "()" appended to names ensures
    # that only the mean and std on original data are used, rather than means of derivate data such as variance
    means <- grep("mean()", names(full_data))
    stds <- grep("std()", names(full_data))
    
    #Extract data concerning mean, std, and the identifier fields above.
    extracted_data <- full_data[c(headers[sort(c(means, stds))], "label", "subject", "group")]

    #Use descriptive activity names identified in activity_labels.txt file to name the activities in the data set.
    activities <- c(1:6)
    activity_names <- c("walking", "walking upstairs", "walking downstairs", "sitting", "standing", "laying")
    for (i in activities) {
      extracted_data$activity_label[extracted_data$label == i] <- activity_names[i]
    }

    #Create second independent data set with the average of each variable for each activity and each subject
    new_data <- ddply(extracted_data, c("subject","activity_label"), numcolwise(mean))
    write.table(new_data, file = paste(filepath, "/new_data.txt", sep=""), row.name=FALSE)
}
