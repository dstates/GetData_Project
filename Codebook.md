# Getting and Cleaning Data Project

Codebook to accompany the `tidyActivityData.tab` table

## Format

The format of the data is tabulated data, with column headers in the first row.
Each row in the table represents the mean of all observations of the same
measurement; that is, a  measurement with four dimensions (X,Y,Z,Magnitude).
This format was chosen for it's tidy and compact nature, making it easy to run
analysis, modeling and graphic representation over each measurement in the table.
Note that for some measurements, not all three dimensions are available, these
missing values are coded NA.

## Cleanup

Per assignment instructions, only the variables which summarized the original
raw data with mean() and sd() were included in this dataset.  The original
column names were changed to be longer and more descriptive (for a list of
column names included, see the first row of the dataset, or the object
`betterVariableNameMap` which is a column rename map in the code
`run_analysis.R`).

To include the variable names, activity names and subject IDs, the files
containing these data are also loaded into tables and then added as variables to
the existing table.

The data is then `melt()`ed down to convert all measurements into a single
column, which is then summarized by measurement with the `mean()` function.

## Output Example


```r
source("run_analysis.R")
head(tidyActivityData)
```

```
> head(tidyActivityData)
    subject activity                                              measurement          X          Y          Z  Magnitude
55        1   LAYING                  FrequencyDomainBodyAccelerationJerkMean -0.9570739 -0.9224626 -0.9480609         NA
235       1   LAYING     FrequencyDomainBodyAccelerationJerkStandardDeviation -0.9641607 -0.9322179 -0.9605870         NA
415       1   LAYING                      FrequencyDomainBodyAccelerationMean -0.9390991 -0.8670652 -0.8826669 -0.8617676
595       1   LAYING         FrequencyDomainBodyAccelerationStandardDeviation -0.9244374 -0.8336256 -0.8128916 -0.7983009
775       1   LAYING              FrequencyDomainBodyBodyAccelerationJerkMean         NA         NA         NA -0.9333004
955       1   LAYING FrequencyDomainBodyBodyAccelerationJerkStandardDeviation         NA         NA         NA -0.9218040
```

## Variables

### subject

**Subject ID** - Integer, Identifies which subject was being measured

### activity

**Activity Name** - Facor, Identifies which activity was being performed when
the measurement was observed.

Levels:  LAYING, SITTING, STANDING, WALKING, WALKING_DOWNSTAIRS,
WALKING_UPSTAIRS 

### measurement

**Measurement Name** - Factor, Identifies whether the measurement is a
timeseries measurement or a derived frequency domain, if the measurement is from
the body (phone) or derived as gravity, the source of the measurement, the
measurement type, and the type of summarization applied to the raw measurement.

Levels: FrequencyDomainBodyAccelerationJerkMean,
FrequencyDomainBodyAccelerationJerkStandardDeviation,
FrequencyDomainBodyAccelerationMean,
FrequencyDomainBodyAccelerationStandardDeviation,
FrequencyDomainBodyBodyAccelerationJerkMean,
FrequencyDomainBodyBodyAccelerationJerkStandardDeviation,
FrequencyDomainBodyBodyGyroscopeJerkMean,
FrequencyDomainBodyBodyGyroscopeJerkStandardDeviation,
FrequencyDomainBodyBodyGyroscopeMean,
FrequencyDomainBodyBodyGyroscopeStandardDeviation,
FrequencyDomainBodyGyroscopeMean,
FrequencyDomainBodyGyroscopeStandardDeviation,
TimeseriesBodyAccelerationJerkMean,
TimeseriesBodyAccelerationJerkStandardDeviation,
TimeseriesBodyAccelerationMean,
TimeseriesBodyAccelerationStandardDeviation,
TimeseriesBodyGyroscopeJerkMean,
TimeseriesBodyGyroscopeJerkStandardDeviation,
TimeseriesBodyGyroscopeMean,
TimeseriesBodyGyroscopeStandardDeviation,
TimeseriesGravityAccelerationMean,
TimeseriesGravityAccelerationStandardDeviation

### X

**X Component of Observation** - Continuous, Observed value for the X dimension
of the measurement

### Y

**Y Component of Observation** - Continuous, Observed value for the Y dimension
of the measurement

### Z

**Z Component of Observation** - Continuous, Observed value for the Z dimension
of the measurement

### Magnitude

**Magnitude Component of Observation** - Continuous, Observed value for the
Magnitude dimension of the measurement
