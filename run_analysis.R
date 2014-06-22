fetchData <- function (url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip") {
    file = basename(URLdecode(url))
    if (!file.exists(file)) {
        message(paste("Downloading",file,"..."))
        download.file(url, destfile=file, method = "curl")
    }
    # use globally scoped variable to cache zip file name
    dataZipFile <<- file
}

getRealColNames <- function (file = "UCI HAR Dataset/features.txt",
                             force = FALSE) {
    if (!exists("dataZipFile"))
        fetchData()
    # use globally scoped variable to cache names
    if (!exists("featuresColNames") | force)
        featuresColNames <<- read.table(
            unz(dataZipFile, filename = file),
            header = FALSE, row.names = 1, stringsAsFactors = FALSE,
            col.names = c("idx", "names")
        )
    invisible(featuresColNames$names)
}

getColNames <- function() {
    # replace some column names with more descriptive column names
    realColNames <- getRealColNames()
    for (i in 1:length(realColNames))
        if (realColNames[i] %in% names(betterVariableNameMap))
            realColNames[i] <- betterVariableNameMap[[realColNames[i]]]
    colnames <- gsub("[()-]", "", realColNames)
    invisible(colnames)
}

getActivityLabels <- function(file = "UCI HAR Dataset/activity_labels.txt", 
                              force = FALSE) {
    if (!exists("DataZipFile"))
        fetchData()
    # use globally scoped variable to cache labels
    if (!exists("activityLabels") | force)
        activityLabels <<- read.table(
            unz(dataZipFile, filename=file),
            header = FALSE, row.names = 1, stringsAsFactors = TRUE,
            col.names = c("idx", "activity")
        )
    # any processing needed?
    invisible(activityLabels$activity)
}

loadAndMergeData <- function(files = list(
    list(data = "UCI HAR Dataset/test/X_test.txt",
         activities = "UCI HAR Dataset/test/y_test.txt",
         subjects = "UCI HAR Dataset/test/subject_test.txt"),
    list(data = "UCI HAR Dataset/train/X_train.txt",
         activities = "UCI HAR Dataset/train/y_train.txt",
         subjects = "UCI HAR Dataset/train/subject_train.txt")
        )
    )
{
    do.call("rbind",
            lapply(files, function (file) {
                actData <- read.table(unz(dataZipFile, filename=file$data),
                                      header = FALSE,
                                      colClasses = rep("numeric",
                                                       length(getColNames())),
                                      col.names = getColNames()
                )
                activities <- read.table(unz(dataZipFile,
                                             filename=file$activities),
                                         header = FALSE,
                                         colClasses = c("integer"),
                                         col.names=c("val")
                )
                subjects <- read.table(unz(dataZipFile,
                                           filename=file$subjects),
                                       header = FALSE,
                                       colClasses = c("factor"),
                                       col.names=c("subject")
                )
                actData <- cbind(actData,
                                 subject=subjects$subject,
                                 activity=getActivityLabels()[activities$val]
                )
                actData
            })
    )
}

extractMeanAndStd <- function(actData) {
    cbind(subject=actData$subject,
          activity=actData$activity,
          actData[ , grep("mean\\(\\)|std\\(\\)", getRealColNames()) ]
    )
}

loadTestDataActivities <- function (file = "UCI HAR Dataset/test/y_test.txt") {
    testactivities <- read.table(file, header=FALSE, colClasses = c("integer"))
    invisible(testactivities)
}

reshapeAndSummarize <- function(actData) {
    actDataM <- melt(actData, id.vars = c("subject","activity"), variable.name = "measurement")
    actDataM <- ddply(actDataM, .(subject, activity, variable), function(x) { c(mean = mean(x$value)) })
    actDataM <- rename(actDataM, c(variable = "measurement"))
    actDataM <- cbind(colsplit(actDataM$measurement, "_", c("measurement", "axis")),
                      actDataM[, !(names(actDataM) %in% c("measurement"))])
    actDataM <- dcast(actDataM, ... ~ axis, value.var = "mean")
    actDataM <- actDataM[, c("subject", "activity", "measurement", "X","Y", "Z", "Magnitude")]
    actDataM
}

betterVariableNameMap <- list(
    "tBodyAcc-mean()-X"    	    = "MeanTimeseriesBodyAcceleration_X",
    "tBodyAcc-mean()-Y"		    = "MeanTimeseriesBodyAcceleration_Y",
    "tBodyAcc-mean()-Z"		    = "MeanTimeseriesBodyAcceleration_Z",
    "tBodyAcc-std()-X"		    = "StandardDeviationTimeseriesBodyAcceleration_X",
    "tBodyAcc-std()-Y"		    = "StandardDeviationTimeseriesBodyAcceleration_Y",
    "tBodyAcc-std()-Z"		    = "StandardDeviationTimeseriesBodyAcceleration_Z",
    "tGravityAcc-mean()-X"		= "MeanTimeseriesGravityAcceleration_X",
    "tGravityAcc-mean()-Y"		= "MeanTimeseriesGravityAcceleration_Y",
    "tGravityAcc-mean()-Z"		= "MeanTimeseriesGravityAcceleration_Z",
    "tGravityAcc-std()-X"		= "StandardDeviationTimeseriesGravityAcceleration_X",
    "tGravityAcc-std()-Y"		= "StandardDeviationTimeseriesGravityAcceleration_Y",
    "tGravityAcc-std()-Z"		= "StandardDeviationTimeseriesGravityAcceleration_Z",
    "tBodyAccJerk-mean()-X"		= "MeanTimeseriesBodyAccelerationJerk_X",
    "tBodyAccJerk-mean()-Y"		= "MeanTimeseriesBodyAccelerationJerk_Y",
    "tBodyAccJerk-mean()-Z"		= "MeanTimeseriesBodyAccelerationJerk_Z",
    "tBodyAccJerk-std()-X"		= "StandardDeviationTimeseriesBodyAccelerationJerk_X",
    "tBodyAccJerk-std()-Y"		= "StandardDeviationTimeseriesBodyAccelerationJerk_Y",
    "tBodyAccJerk-std()-Z"		= "StandardDeviationTimeseriesBodyAccelerationJerk_Z",
    "tBodyGyro-mean()-X"		= "MeanTimeseriesBodyGyroscope_X",
    "tBodyGyro-mean()-Y"		= "MeanTimeseriesBodyGyroscope_Y",
    "tBodyGyro-mean()-Z"		= "MeanTimeseriesBodyGyroscope_Z",
    "tBodyGyro-std()-X"		    = "StandardDeviationTimeseriesBodyGyroscope_X",
    "tBodyGyro-std()-Y"		    = "StandardDeviationTimeseriesBodyGyroscope_Y",
    "tBodyGyro-std()-Z"		    = "StandardDeviationTimeseriesBodyGyroscope_Z",
    "tBodyGyroJerk-mean()-X"	= "MeanTimeseriesBodyGyroscopeJerk_X",
    "tBodyGyroJerk-mean()-Y"	= "MeanTimeseriesBodyGyroscopeJerk_Y",
    "tBodyGyroJerk-mean()-Z"	= "MeanTimeseriesBodyGyroscopeJerk_Z",
    "tBodyGyroJerk-std()-X"		= "StandardDeviationTimeseriesBodyGyroscopeJerk_X",
    "tBodyGyroJerk-std()-Y"		= "StandardDeviationTimeseriesBodyGyroscopeJerk_Y",
    "tBodyGyroJerk-std()-Z"		= "StandardDeviationTimeseriesBodyGyroscopeJerk_Z",
    "tBodyAccMag-mean()"		= "MeanTimeseriesBodyAcceleration_Magnitude",
    "tBodyAccMag-std()"		    = "StandardDeviationTimeseriesBodyAcceleration_Magnitude",
    "tGravityAccMag-mean()"		= "MeanTimeseriesGravityAcceleration_Magnitude",
    "tGravityAccMag-std()"		= "StandardDeviationTimeseriesGravityAcceleration_Magnitude",
    "tBodyAccJerkMag-mean()"	= "MeanTimeseriesBodyAccelerationJerk_Magnitude",
    "tBodyAccJerkMag-std()"		= "StandardDeviationTimeseriesBodyAccelerationJerk_Magnitude",
    "tBodyGyroMag-mean()"		= "MeanTimeseriesBodyGyroscope_Magnitude",
    "tBodyGyroMag-std()"		= "StandardDeviationTimeseriesBodyGyroscope_Magnitude",
    "tBodyGyroJerkMag-mean()"	= "MeanTimeseriesBodyGyroscopeJerk_Magnitude",
    "tBodyGyroJerkMag-std()"	= "StandardDeviationTimeseriesBodyGyroscopeJerk_Magnitude",
    "fBodyAcc-mean()-X"		    = "MeanFrequencyDomainBodyAcceleration_X",
    "fBodyAcc-mean()-Y"		    = "MeanFrequencyDomainBodyAcceleration_Y",
    "fBodyAcc-mean()-Z"		    = "MeanFrequencyDomainBodyAcceleration_Z",
    "fBodyAcc-std()-X"		    = "StandardDeviationFrequencyDomainBodyAcceleration_X",
    "fBodyAcc-std()-Y"		    = "StandardDeviationFrequencyDomainBodyAcceleration_Y",
    "fBodyAcc-std()-Z"		    = "StandardDeviationFrequencyDomainBodyAcceleration_Z",
    "fBodyAccJerk-mean()-X"		= "MeanFrequencyDomainBodyAccelerationJerk_X",
    "fBodyAccJerk-mean()-Y"		= "MeanFrequencyDomainBodyAccelerationJerk_Y",
    "fBodyAccJerk-mean()-Z"		= "MeanFrequencyDomainBodyAccelerationJerk_Z",
    "fBodyAccJerk-std()-X"		= "StandardDeviationFrequencyDomainBodyAccelerationJerk_X",
    "fBodyAccJerk-std()-Y"		= "StandardDeviationFrequencyDomainBodyAccelerationJerk_Y",
    "fBodyAccJerk-std()-Z"		= "StandardDeviationFrequencyDomainBodyAccelerationJerk_Z",
    "fBodyGyro-mean()-X"		= "MeanFrequencyDomainBodyGyroscope_X",
    "fBodyGyro-mean()-Y"		= "MeanFrequencyDomainBodyGyroscope_Y",
    "fBodyGyro-mean()-Z"		= "MeanFrequencyDomainBodyGyroscope_Z",
    "fBodyGyro-std()-X"		    = "StandardDeviationFrequencyDomainBodyGyroscope_X",
    "fBodyGyro-std()-Y"		    = "StandardDeviationFrequencyDomainBodyGyroscope_Y",
    "fBodyGyro-std()-Z"		    = "StandardDeviationFrequencyDomainBodyGyroscope_Z",
    "fBodyAccMag-mean()"		= "MeanFrequencyDomainBodyAcceleration_Magnitude",
    "fBodyAccMag-std()"		    = "StandardDeviationFrequencyDomainBodyAcceleration_Magnitude",
    "fBodyBodyAccJerkMag-mean()"  	= "MeanFrequencyDomainBodyBodyAccelerationJerk_Magnitude",
    "fBodyBodyAccJerkMag-std()"	= "StandardDeviationFrequencyDomainBodyBodyAccelerationJerk_Magnitude",
    "fBodyBodyGyroMag-mean()"	= "MeanFrequencyDomainBodyBodyGyroscope_Magnitude",
    "fBodyBodyGyroMag-std()"	= "StandardDeviationFrequencyDomainBodyBodyGyroscope_Magnitude",
    "fBodyBodyGyroJerkMag-mean()" 	= "MeanFrequencyDomainBodyBodyGyroscopeJerk_Magnitude",
    "fBodyBodyGyroJerkMag-std()"  	= "StandardDeviationFrequencyDomainBodyBodyGyroscopeJerk_Magnitude"
)

# Load and cache data, loadAndMergeData also replaces some column names
fetchData()
rawActivityData <- extractMeanAndStd(loadAndMergeData())

# Reshape data into tidy format and average every measurement
tidyActivityData <- reshapeAndSummarize(rawActivityData)

# write out tidy data table
write.table(tidyActivityData, file = "tidyActivityData.tab")
