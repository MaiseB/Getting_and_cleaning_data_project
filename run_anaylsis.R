run_analysis.R <- function(){
        ## Function with aim of:
        ## 1. Merges the training and the test sets to create one data set.
        ## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
        ## 3. Uses descriptive activity names to name the activities in the data set
        ## 4. Appropriately labels the data set with descriptive variable names.
        ## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
        
        ## Parameters: none
        ## assumes working directory set in UCI HAR Datase
##***************************************************************************************************************************************************************        
        ## 1. Merging the training and the test sets to create one data set.
        
        ## read all test data & merge into 1 table. 
        testsubjno <- read.table("./test/subject_test.txt")
        testactivity <- read.table ("./test/y_test.txt")
        testdata <- read.table("./test/X_test.txt")
        mergedtest <- cbind(testsubjno, testactivity, testdata)
        
        ## read all train data & merge into 1 table
        trainsubjno <- read.table("./train/subject_train.txt")
        trainactivity <- read.table("./train/y_train.txt")
        traindata <-read.table("./train/X_train.txt")
        mergedtrain <- cbind(trainsubjno, trainactivity, traindata)
        
        ## merge test and train data into 1 table
        trainandtest <- rbind(mergedtest, mergedtrain)
        
        ## label columnns
        features <- read.table("./features.txt")
        featvector <- features[,2]
        featvector <- as.character(featvector)
        coltitles <- c("subjectnumber","activity", featvector)
        names(trainandtest) <- coltitles
##***************************************************************************************************************************************************************        
        ## 2. Extracting only mean and std for each measurmement
        
        matches <- grep("subjectnumber|activity|mean|std", names(trainandtest))
        meanandstddata <- trainandtest[,matches]
        
##***************************************************************************************************************************************************************  
       ## 3. Uses descriptive activity names to name the activities in the data set 

        activitynames <- read.table("./activity_labels.txt")
        meanandstddata$activity <- factor(meanandstddata$activity, levels = c(1,2,3,4,5,6), labels = activitynames[,2])
##***************************************************************************************************************************************************************
        ## 4. Rename columns
        names(meanandstddata)<-gsub("^t", "time", names(meanandstddata))
        names(meanandstddata)<-gsub("Mag", "Magnitude", names(meanandstddata))
        names(meanandstddata)<-gsub("Acc", "Accelerometer", names(meanandstddata))
        names(meanandstddata)<-gsub("Gyro", "Gyroscope", names(meanandstddata))
        names(meanandstddata)<-gsub("BodyBody", "Body", names(meanandstddata))
        names(meanandstddata)<-gsub("^f", "frequency", names(meanandstddata))
        
##***************************************************************************************************************************************************************
        ## 5. create independent tidy data set with the average of each variable for each activity and each subject
        meanandstddata$subjectnumber <- as.factor(meanandstddata$subjectnumber)
        tidyaveragedata <- aggregate(meanandstddata, by=list(meanandstddata$subjectnumber, meanandstddata$activity), FUN=mean)
        names(tidyaveragedata)[1:2] <-c("subject", "activity")
        tidyaveragedata <- tidyaveragedata[order(tidyaveragedata$subject, tidyaveragedata$activity),]
        tidyaveragedata <- tidyaveragedata[ , -(3:4)]
        write.table(tidyaveragedata, file = "tidydata.txt",row.name=FALSE, col.names=TRUE)
##***************************************************************************************************************************************************************
}