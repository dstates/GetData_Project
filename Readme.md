# Getting and Cleaning Data Course Project

## Overview

This repo contains the script `run_analysis.R` which reads in data from the UCI
Machine Learning Repository's Human Activity Recognition Using Smartphones Data
Set.  The measurements which were summarized by their Mean and Standard
Deviation are extracted.  The extracted measurement variable names have been
renamed to be more verbose and readable.  Each measurement has been summarized
with the `mean()` function, producting a single resulting value for each
measurement from the raw dataset.  The resulting data has been reshaped into a
[tidy](http://vita.had.co.nz/papers/tidy-data.pdf) dataset, which each row
representing a multi-dimensional observation from a single measurement.  The
dimenstions are X, Y, Z, Magnitude.

All the data are in a single table, although it makes sense to break these data
into multiple tables, there does not appear to be a way to submit multiple data
table file to the grading system on coursera.

## Script Operation

The script `run_analysis.R` is broken up into functional components to handle
each step of processing the data.  Each function is thoroughly documented in the
source,  please read the run_analysis.R script source for an indepth explanation
of each function and processing step.

At a high level, the functions are as follows:

* `fetchData()` - Checks to see if the dataset has already been downloaded, and
downloads it if it is missing.  Also caches the destination filename into a
global variable to remember if the dataset has been downloaded already for
subsiquent calls
* `getRealColNames()` - Loads the features.txt file with contains all the
column names for each measurement in the dataset.  Caches the value so that
subsiquent calls don't require reloading the file
* `getColNames()` - Reads column names from `getRealColNames()` and removes
characters that are not legal R variable names.  Also uses the
`betterVariableNameMap` list to replace some column names with longer, more
descriptive names.
* `getActivityLabels()` - Reads the activity names from the
`activity_labels.txt` file and caches the result in a global variable so that
subsiquent calls to the function don't require re-reading the file
* `loadAndMergeData()` - Reads the test and train data, along with the activity
and subject id files that accompany them, combines the test and training data
into a single dataframe along with new columns for the activity name and
subject ID.  Columns are named from the getColNames() function and activities
are named from the getActivityLabels() function.
* `extractMeanAndStd()` - Extracts out columns who's original name matches the
pattern "mean()" or "std()" from the dataset returned by `loadAndMergeData()`.
Also preserves the activity and subject columns.
* `reshapeAndSummarize()` - uses the `reshape2` and `plyr` libraries to reshape
the dataset into a much more tidy format.  Also summarizes the data per the
project instructions by calculating the mean of every measurement.

The above functions are run at the bottom of the script to produce the tidy
dataset table file `tidyActivityData.tab`.  An excerpt from the script that
loads the raw data and writes the tidy data:

```
# Load and cache data, loadAndMergeData also replaces some column names
library(plyr)
library(reshape2)
fetchData()
rawActivityData <- extractMeanAndStd(loadAndMergeData())

# Reshape data into tidy format and average every measurement
tidyActivityData <- reshapeAndSummarize(rawActivityData)

# write out tidy data table
write.table(tidyActivityData, file = "tidyActivityData.tab")
```