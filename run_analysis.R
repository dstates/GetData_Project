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
    colnames <- gsub("[()-]", "", getRealColNames())
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
         activities = "UCI HAR Dataset/test/y_test.txt"),
    list(data = "UCI HAR Dataset/train/X_train.txt",
         activities = "UCI HAR Dataset/test/y_test.txt")
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
                actData$activity <- getActivityLabels()[activities$val]
                actData
            })
    )
}

extractMeanAndStd <- function(actData) {
    actData[ , grep("mean()|std()", getRealColNames()) ]
}

loadTestDataActivities <- function (file = "UCI HAR Dataset/test/y_test.txt") {
    testactivities <- read.table(file, header=FALSE, colClasses = c("integer"))
    invisible(testactivities)
}