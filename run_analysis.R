# fetchData(): 
#
# Download dataset and cache local filename into dataZipFile

fetchData <- function (url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip") {
    # extract file name from URL
    file = basename(URLdecode(url))
    # download file if it doesn't already exists
    if (!file.exists(file)) {
        message(paste("Downloading",file,"..."))
        download.file(url, destfile=file, method = "curl")
    }
    # use globally scoped variable to cache zip file name
    dataZipFile <<- file
}

# getRealColNames(file, force)
#
# Read column names from the features.txt file inside the dataset zipfile and
# cache the column names in the globally scoped variable featuresColNames
#
# optionally pass force = TRUE to force a re-read if data is already cached.

getRealColNames <- function (file = "UCI HAR Dataset/features.txt",
                             force = FALSE) {
    # if the dataZipFile variable isn't set, call fetchData() to set it
    if (!exists("dataZipFile"))
        fetchData()
    # use globally scoped variable to cache names, if it doesn't exist then
    # read the data
    if (!exists("featuresColNames") | force)
        featuresColNames <<- read.table(
            unz(dataZipFile, filename = file),
            header = FALSE, row.names = 1, stringsAsFactors = FALSE,
            col.names = c("idx", "names")
        )
    invisible(featuresColNames$names)
}

# getColNames():
#
# Read in column names from getRealColNames(), and replace some column names
# with hand written names in the betterVariableNameMap.  Also remove illegal
# variable name characters "(", ")", and "-" from each colum name.

getColNames <- function() {
    # replace some column names with more descriptive column names
    realColNames <- getRealColNames()
    for (i in 1:length(realColNames))
        if (realColNames[i] %in% names(betterVariableNameMap))
            realColNames[i] <- betterVariableNameMap[[realColNames[i]]]
    colnames <- gsub("[()-]", "", realColNames)
    invisible(colnames)
}

# getActivityLabels(file, force):
#
# Read in the activity_labels.txt file contained in the dataset zipfile and
# cache in a global variable activityLabels.  If variable already exists
# then return cached value.  Optionally force a re-read of the data is the
# variable is cached.

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
    invisible(activityLabels$activity)
}

# loadAndMergeData(files):
#
# Read in the datasets from the test and training data, along with their
# accompanying subject ID list and activity ID list.  The files argument is a 
# list of lists, where the second order list contains the attributes "data",
# "activities", and "subjects".  The value of each is the location of the file
# containing the measurement data, activity IDs, and subject IDs respectively.
# 
# The training and test data are conbined into a single dataset.  The
# getColNames() and getActivityLabels() functions are used to set the column
# names and to replace the activity IDs with descriptive values, respectively.
#
# do.call is used to allow us to take the output of the lapply opertaion and
# pass the resulting list elements as individual arguments to the rbind function
# which will combine the two resultant dataframes into a single dataframe.
#
# lapply iterates over the files argument, passing each list value to it's
# anonymous function argument as the "file" parameter.
#
# the anonymous function first reads in the datatable from the data file for
# either the test or training data into a dataframe called actData, setting the
# column names from getColNames().
#
# activities are loaded into another dataframe called activities, which has
# a single variable, "val", which contains the activity ID.  Each row in the
# activities dataframe corresponds to a row in the actData dataframe, serving as
# an external column with the activity ID for each set of observations.
#
# subjects are loaded into a dataframe called subjects, which has a single
# variable, "subject", which is the subject's ID number.  Each row in the
# subjects dataframe corresponds to a row in the actData dataframe, serving as
# an external column with the subject ID for each set of observations.
#
# The single variables from the subjects and activities dataframes are added as
# new columns to the actData dataframe, with the activities converted to their
# respective activity names using the output of the getActivityLabels() function

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

# extractMeanAndStd(actData):
#
# Extracts and returns only the columns containing the strings "mean()" and
# "std()" in the original columns list as per the project instructions.  Also
# preseves the subject and activity columns, using cbind to re-assemble the
# columns into a new dataframe.

extractMeanAndStd <- function(actData) {
    cbind(subject=actData$subject,
          activity=actData$activity,
          actData[ , grep("mean\\(\\)|std\\(\\)", getRealColNames()) ]
    )
}

# reshapeAndSummarize(actData):
#
# Reshapes the data into a tidy dataset where each row represents a single,
# multi-dimensional, observation with the dimensions X, Y, Z, Magnitude.  Each
# measurement is summarized with the mean() function per the project
# instructions.
#
# The data are first melted, preserving the subject and activity variables, with
# all other columns being converted to the 'variable' row and their values
# converted to the 'value' row.  Before melting, each row in the dataset was
# actually many observations and transformations of those observations packed
# into a single row.  The melt operation allows us to break them up by
# observation, however the observations are multi-dimenstional and we need to
# cast some of the values back together later.
#
# The molten data is summarized using the ddply function, which makes it very
# easy to subset on the subject, activity, and measurement to calculate the mean
# of each dimenstion of each observation.  This produces a new column called
# "mean", which contains the result of the operation along with the original
# subject, activity, and variable columns.
#
# The variable column of the molten data contains both the measurement type and
# a single dimension of that measurement.  We want to dimension in it's own
# column, so that we can cast each dimension into a column later.  This is
# accomplished with colsplit.  Since the new column names from
# betterVariableNameMap already separate the measurement from the dimension name
# with an underscore (e.g. "TimeseriesBodyAccelerationMean_X"), we can split on
# the underscore and add two more new columns called "measurement" and "axis"
# (with values such as "TimeseriesBodyAccelerationMean" and "X").  Then,
# dropping the variable column, as it's contents now exist in the new columns,
# we use cbind to add the new columns back into the original dataframe while
# excluding the "variable" column.  Our dataframe now has the columns "subject",
# "activity", "measurement", "axis", and "mean".
#
# To convert all the values of the axis column into their own columns, with the
# values of those columns being the mean we calculated above, we use dcast.
# dcast is told to preserve all columns, except for "axis", who's values will
# become new columns, and "mean", who's values will become the values of the new
# columns created from the "axis" column.  The resulting dataframe now has the
# columns "subject", activity", "measurement", "X", "Y", "Z", "Magnitude",
# however they are in the wrong order.
#
# The columns are re-ordered into the preferred order by subsetting on the
# column names, specifying them in the order which we would like them to appear.
#
# The rows are not in any particular order, so we sort them on the subject and
# activity rows, treating the subject IDs as integers (so that 1 comes before
# 10, etc).
#
# Finally the subject, activity, and measurement variables are transformed into
# factors to that subset operations are optimized.

reshapeAndSummarize <- function(actData) {
    actDataM <- melt(actData, id.vars = c("subject","activity"))
    actDataM <- ddply(actDataM, .(subject, activity, variable), function(x) { c(mean = mean(x$value)) })
    actDataM <- cbind(colsplit(actDataM$variable, "_", c("measurement", "axis")),
                      actDataM[, !(names(actDataM) %in% c("variable"))])
    actDataM <- dcast(actDataM, ... ~ axis, value.var = "mean")
    actDataM <- actDataM[, c("subject", "activity", "measurement", "X","Y", "Z", "Magnitude")]
    actDataM <- actDataM[with(actDataM, order(as.integer(as.character(subject)), activity)), ]
    actDataM <- transform(actDataM, subject=factor(subject), activity=factor(activity), measurement=factor(measurement))
    actDataM
}

betterVariableNameMap <- list(
    "tBodyAcc-mean()-X"    	    = "TimeseriesBodyAccelerationMean_X",
    "tBodyAcc-mean()-Y"		    = "TimeseriesBodyAccelerationMean_Y",
    "tBodyAcc-mean()-Z"		    = "TimeseriesBodyAccelerationMean_Z",
    "tBodyAcc-std()-X"		    = "TimeseriesBodyAccelerationStandardDeviation_X",
    "tBodyAcc-std()-Y"		    = "TimeseriesBodyAccelerationStandardDeviation_Y",
    "tBodyAcc-std()-Z"		    = "TimeseriesBodyAccelerationStandardDeviation_Z",
    "tGravityAcc-mean()-X"		= "TimeseriesGravityAccelerationMean_X",
    "tGravityAcc-mean()-Y"		= "TimeseriesGravityAccelerationMean_Y",
    "tGravityAcc-mean()-Z"		= "TimeseriesGravityAccelerationMean_Z",
    "tGravityAcc-std()-X"		= "TimeseriesGravityAccelerationStandardDeviation_X",
    "tGravityAcc-std()-Y"		= "TimeseriesGravityAccelerationStandardDeviation_Y",
    "tGravityAcc-std()-Z"		= "TimeseriesGravityAccelerationStandardDeviation_Z",
    "tBodyAccJerk-mean()-X"		= "TimeseriesBodyAccelerationJerkMean_X",
    "tBodyAccJerk-mean()-Y"		= "TimeseriesBodyAccelerationJerkMean_Y",
    "tBodyAccJerk-mean()-Z"		= "TimeseriesBodyAccelerationJerkMean_Z",
    "tBodyAccJerk-std()-X"		= "TimeseriesBodyAccelerationJerkStandardDeviation_X",
    "tBodyAccJerk-std()-Y"		= "TimeseriesBodyAccelerationJerkStandardDeviation_Y",
    "tBodyAccJerk-std()-Z"		= "TimeseriesBodyAccelerationJerkStandardDeviation_Z",
    "tBodyGyro-mean()-X"		= "TimeseriesBodyGyroscopeMean_X",
    "tBodyGyro-mean()-Y"		= "TimeseriesBodyGyroscopeMean_Y",
    "tBodyGyro-mean()-Z"		= "TimeseriesBodyGyroscopeMean_Z",
    "tBodyGyro-std()-X"		    = "TimeseriesBodyGyroscopeStandardDeviation_X",
    "tBodyGyro-std()-Y"		    = "TimeseriesBodyGyroscopeStandardDeviation_Y",
    "tBodyGyro-std()-Z"		    = "TimeseriesBodyGyroscopeStandardDeviation_Z",
    "tBodyGyroJerk-mean()-X"	= "TimeseriesBodyGyroscopeJerkMean_X",
    "tBodyGyroJerk-mean()-Y"	= "TimeseriesBodyGyroscopeJerkMean_Y",
    "tBodyGyroJerk-mean()-Z"	= "TimeseriesBodyGyroscopeJerkMean_Z",
    "tBodyGyroJerk-std()-X"		= "TimeseriesBodyGyroscopeJerkStandardDeviation_X",
    "tBodyGyroJerk-std()-Y"		= "TimeseriesBodyGyroscopeJerkStandardDeviation_Y",
    "tBodyGyroJerk-std()-Z"		= "TimeseriesBodyGyroscopeJerkStandardDeviation_Z",
    "tBodyAccMag-mean()"		= "TimeseriesBodyAccelerationMean_Magnitude",
    "tBodyAccMag-std()"		    = "TimeseriesBodyAccelerationStandardDeviation_Magnitude",
    "tGravityAccMag-mean()"		= "TimeseriesGravityAccelerationMean_Magnitude",
    "tGravityAccMag-std()"		= "TimeseriesGravityAccelerationStandardDeviation_Magnitude",
    "tBodyAccJerkMag-mean()"	= "TimeseriesBodyAccelerationJerkMean_Magnitude",
    "tBodyAccJerkMag-std()"		= "TimeseriesBodyAccelerationJerkStandardDeviation_Magnitude",
    "tBodyGyroMag-mean()"		= "TimeseriesBodyGyroscopeMean_Magnitude",
    "tBodyGyroMag-std()"		= "TimeseriesBodyGyroscopeStandardDeviation_Magnitude",
    "tBodyGyroJerkMag-mean()"	= "TimeseriesBodyGyroscopeJerkMean_Magnitude",
    "tBodyGyroJerkMag-std()"	= "TimeseriesBodyGyroscopeJerkStandardDeviation_Magnitude",
    "fBodyAcc-mean()-X"		    = "FrequencyDomainBodyAccelerationMean_X",
    "fBodyAcc-mean()-Y"		    = "FrequencyDomainBodyAccelerationMean_Y",
    "fBodyAcc-mean()-Z"		    = "FrequencyDomainBodyAccelerationMean_Z",
    "fBodyAcc-std()-X"		    = "FrequencyDomainBodyAccelerationStandardDeviation_X",
    "fBodyAcc-std()-Y"		    = "FrequencyDomainBodyAccelerationStandardDeviation_Y",
    "fBodyAcc-std()-Z"		    = "FrequencyDomainBodyAccelerationStandardDeviation_Z",
    "fBodyAccJerk-mean()-X"		= "FrequencyDomainBodyAccelerationJerkMean_X",
    "fBodyAccJerk-mean()-Y"		= "FrequencyDomainBodyAccelerationJerkMean_Y",
    "fBodyAccJerk-mean()-Z"		= "FrequencyDomainBodyAccelerationJerkMean_Z",
    "fBodyAccJerk-std()-X"		= "FrequencyDomainBodyAccelerationJerkStandardDeviation_X",
    "fBodyAccJerk-std()-Y"		= "FrequencyDomainBodyAccelerationJerkStandardDeviation_Y",
    "fBodyAccJerk-std()-Z"		= "FrequencyDomainBodyAccelerationJerkStandardDeviation_Z",
    "fBodyGyro-mean()-X"		= "FrequencyDomainBodyGyroscopeMean_X",
    "fBodyGyro-mean()-Y"		= "FrequencyDomainBodyGyroscopeMean_Y",
    "fBodyGyro-mean()-Z"		= "FrequencyDomainBodyGyroscopeMean_Z",
    "fBodyGyro-std()-X"		    = "FrequencyDomainBodyGyroscopeStandardDeviation_X",
    "fBodyGyro-std()-Y"		    = "FrequencyDomainBodyGyroscopeStandardDeviation_Y",
    "fBodyGyro-std()-Z"		    = "FrequencyDomainBodyGyroscopeStandardDeviation_Z",
    "fBodyAccMag-mean()"		= "FrequencyDomainBodyAccelerationMean_Magnitude",
    "fBodyAccMag-std()"		    = "FrequencyDomainBodyAccelerationStandardDeviation_Magnitude",
    "fBodyBodyAccJerkMag-mean()"  	= "FrequencyDomainBodyBodyAccelerationJerkMean_Magnitude",
    "fBodyBodyAccJerkMag-std()"	= "FrequencyDomainBodyBodyAccelerationJerkStandardDeviation_Magnitude",
    "fBodyBodyGyroMag-mean()"	= "FrequencyDomainBodyBodyGyroscopeMean_Magnitude",
    "fBodyBodyGyroMag-std()"	= "FrequencyDomainBodyBodyGyroscopeStandardDeviation_Magnitude",
    "fBodyBodyGyroJerkMag-mean()" 	= "FrequencyDomainBodyBodyGyroscopeJerkMean_Magnitude",
    "fBodyBodyGyroJerkMag-std()"  	= "FrequencyDomainBodyBodyGyroscopeJerkStandardDeviation_Magnitude"
)

# Load and cache data, loadAndMergeData also replaces some column names
library(plyr)
library(reshape2)
fetchData()
rawActivityData <- extractMeanAndStd(loadAndMergeData())

# Reshape data into tidy format and average every measurement
tidyActivityData <- reshapeAndSummarize(rawActivityData)

# write out tidy data table
write.table(tidyActivityData, file = "tidyActivityData.tab")
