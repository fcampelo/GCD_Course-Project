---
title: 'Course Project: Getting and Cleaning Data'
author: "Felipe Campelo"
date: "December 12, 2014"
output: html_document
---

## Introduction
This document describes the analysis performed for the Course Project of the 
Getting and Cleaning Data course on Coursera. The objective of the project is to 
generate a tidy data set from the "Human Activity Recognition Using Smartphones 
Data Set", originally posted at the [UCI Machine Learning Repository][1]. 

Before we get down to business, I must highlight a few things:

- All the analysis contained in this document is replicated in the plain
__run_analysis.R__ file, so you can either knit this document or simply run the 
other file.
- This analysis assumes that the __UCI HAR Dataset__ folder (obtained after 
unzipping the __UCI HAR Dataset.zip__ file) is a subfolder of the working 
directory. In other words, I assume that the folder structure of the dataset is
maintained, instead of simply pasting all files to the working directory.

So, without further ado, let's proceed to the analysis!

[1]: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones "UCI"

### 1) Merge the training and the test sets into a single one.

The first activity consists in loading and merging the two data sets into a 
single dataframe. First things first, let's load the relevant files from the 
training and test groups.


```r
# Load test data:
test.features<-read.table("UCI HAR Dataset/test/X_test.txt",
                          header=F, sep="")
test.subjects<-read.table("UCI HAR Dataset/test/subject_test.txt",
                          header=F, sep="")
test.activities<-read.table("UCI HAR Dataset/test/y_test.txt",
                            header=F, sep="")

# Load train data
train.features<-read.table("UCI HAR Dataset/train/X_train.txt",
                           header=F, sep="")
train.subjects<-read.table("UCI HAR Dataset/train/subject_train.txt",
                           header=F, sep="")
train.activities<-read.table("UCI HAR Dataset/train/y_train.txt",
                             header=F, sep="")

# Load feature labels
feature.labels<-read.table("UCI HAR Dataset/features.txt",
                           header=F, sep="",
                           colClasses = c("integer","character"))[,2]

# Load activity labels
activity.labels<-read.table("UCI HAR Dataset/activity_labels.txt",
                            header=F, sep="",
                            colClasses = c("integer","character"))[,2]
```

Now we can begin to manipulate these objects a little bit to make everything a 
little more workable:


```r
# Consolidate the test and train group data: 
test.data<-cbind(Subject=test.subjects[,1],
                 Activity = as.factor(test.activities[,1]),
                 test.features)

train.data<-cbind(Subject=train.subjects[,1],
                 Activity = as.factor(train.activities[,1]),
                 train.features)

# Get them both into a single dataframe. I'll use the dplyr package to make 
# things simpler from now on:
library(dplyr,warn.conflicts = F)
full.data<-tbl_df(rbind(train.data,test.data))

# Get column names right
names(full.data)<-c("Subject",
                    "Activity",
                    feature.labels)

# Also let's make the activity labels a little more descriptive
levels(full.data$Activity)<-activity.labels

# Finally (for now) let's also free up some memory by removing the now-useless 
# variables
rm(list=c("feature.labels","activity.labels",
          "test.activities","test.data","test.features","test.subjects",
          "train.activities","train.data","train.features","train.subjects"))
```

Before we proceed, a bit of a cleanup: there seems to be a few columns with 
duplicated names in our dataframe:

```r
# Find out how many columns have at least one other under the same name
dups<-which(duplicated(names(full.data)))
length(dups)
```

```
## [1] 84
```

```r
# Notice that the names are duplicated, but the contents are not:
col1<-names(full.data)[dups[1]]
indx<-which(names(full.data)==col1)
identical(full.data[,indx[1]],full.data[,indx[2]])
```

```
## [1] FALSE
```

This will mess up the column selection methods of the **dplyr** package if we 
don't do something, so let's do something!


```r
# get the names of the relevant columns
dupnames<-names(full.data)[dups]

# Modify these names so that each is slightly but surely different from all 
# others. It can be done by simply adding a unique number before each name, and 
# then re-attributing the (now modified) column names to the relevant columns
names(full.data)[dups]<-paste0(1:84,dupnames)

# And voila'! All is good again!
which(duplicated(names(full.data)))
```

```
## integer(0)
```

### 2) Extract only the columns containing the mean and standard deviation for each measurement

This one will be relatively easy, since we already attributed the feature names 
to their respective columns. Notice that I will only select the columns that 
actually represent mean and standard deviation for each measurement. This 
excludes five columns that have 'mean' in their names, but are actually 
representing quantities that are averaged over sampling windows (explained in 
the original *features_info.txt* file). These (excluded) columns are:

- gravityMean
- tBodyAccMean
- tBodyAccJerkMean
- tBodyGyroMean
- tBodyGyroJerkMean


With that out of the way, let's proceed with our work:


```r
# Select only the columns that contain 'mean()' or 'std()' in their names
mean.sd.data<-select(full.data,
                     c(Subject,
                       Activity,
                       contains("mean()"),contains("std()")))
```

### 3) Use descriptive activity names to name the activities in the data set

This was already done in **Section 1**. We can check it easily:


```r
# Check that the Activity factor has descriptive level names
summary(mean.sd.data$Activity)
```

```
##            WALKING   WALKING_UPSTAIRS WALKING_DOWNSTAIRS 
##               1722               1544               1406 
##            SITTING           STANDING             LAYING 
##               1777               1906               1944
```


### 4) Label the data set with descriptive variable names

Again, already done in **Section 1**!


```r
# Check that the Columns have descriptive names
names(mean.sd.data)
```

```
##  [1] "Subject"                     "Activity"                   
##  [3] "tBodyAcc-mean()-X"           "tBodyAcc-mean()-Y"          
##  [5] "tBodyAcc-mean()-Z"           "tGravityAcc-mean()-X"       
##  [7] "tGravityAcc-mean()-Y"        "tGravityAcc-mean()-Z"       
##  [9] "tBodyAccJerk-mean()-X"       "tBodyAccJerk-mean()-Y"      
## [11] "tBodyAccJerk-mean()-Z"       "tBodyGyro-mean()-X"         
## [13] "tBodyGyro-mean()-Y"          "tBodyGyro-mean()-Z"         
## [15] "tBodyGyroJerk-mean()-X"      "tBodyGyroJerk-mean()-Y"     
## [17] "tBodyGyroJerk-mean()-Z"      "tBodyAccMag-mean()"         
## [19] "tGravityAccMag-mean()"       "tBodyAccJerkMag-mean()"     
## [21] "tBodyGyroMag-mean()"         "tBodyGyroJerkMag-mean()"    
## [23] "fBodyAcc-mean()-X"           "fBodyAcc-mean()-Y"          
## [25] "fBodyAcc-mean()-Z"           "fBodyAccJerk-mean()-X"      
## [27] "fBodyAccJerk-mean()-Y"       "fBodyAccJerk-mean()-Z"      
## [29] "fBodyGyro-mean()-X"          "fBodyGyro-mean()-Y"         
## [31] "fBodyGyro-mean()-Z"          "fBodyAccMag-mean()"         
## [33] "fBodyBodyAccJerkMag-mean()"  "fBodyBodyGyroMag-mean()"    
## [35] "fBodyBodyGyroJerkMag-mean()" "tBodyAcc-std()-X"           
## [37] "tBodyAcc-std()-Y"            "tBodyAcc-std()-Z"           
## [39] "tGravityAcc-std()-X"         "tGravityAcc-std()-Y"        
## [41] "tGravityAcc-std()-Z"         "tBodyAccJerk-std()-X"       
## [43] "tBodyAccJerk-std()-Y"        "tBodyAccJerk-std()-Z"       
## [45] "tBodyGyro-std()-X"           "tBodyGyro-std()-Y"          
## [47] "tBodyGyro-std()-Z"           "tBodyGyroJerk-std()-X"      
## [49] "tBodyGyroJerk-std()-Y"       "tBodyGyroJerk-std()-Z"      
## [51] "tBodyAccMag-std()"           "tGravityAccMag-std()"       
## [53] "tBodyAccJerkMag-std()"       "tBodyGyroMag-std()"         
## [55] "tBodyGyroJerkMag-std()"      "fBodyAcc-std()-X"           
## [57] "fBodyAcc-std()-Y"            "fBodyAcc-std()-Z"           
## [59] "fBodyAccJerk-std()-X"        "fBodyAccJerk-std()-Y"       
## [61] "fBodyAccJerk-std()-Z"        "fBodyGyro-std()-X"          
## [63] "fBodyGyro-std()-Y"           "fBodyGyro-std()-Z"          
## [65] "fBodyAccMag-std()"           "fBodyBodyAccJerkMag-std()"  
## [67] "fBodyBodyGyroMag-std()"      "fBodyBodyGyroJerkMag-std()"
```

### 5) Create a second, independent tidy data set: average of each variable for each activity and each subject.

To fulfill this last part of the assingment, let's first group the data by the 
relevant variables, and then summarize the data for these groups. I'll use the 
_chaining_ operator from package **dplyr** to achieve this (and a few other cool 
things):


```r
# Assemble new data frame
new.df<-mean.sd.data %>%              # Get the mean.sd.data data frame tbl
  group_by(Activity,Subject) %>%      # group it by Activity and Subject
  summarise_each(funs(mean)) %>%      # Summarize all other columns by mean 
  arrange(Activity,Subject) %>%       # Sort rows by Activity and subject
  mutate(Subject=as.factor(Subject))  # And finally convert Subject into the factor variable it rightfully is.

print(new.df)
```

```
## Source: local data frame [180 x 68]
## Groups: Activity
## 
##    Activity Subject tBodyAcc-mean()-X tBodyAcc-mean()-Y tBodyAcc-mean()-Z
## 1   WALKING       1         0.2773308       -0.01738382        -0.1111481
## 2   WALKING       2         0.2764266       -0.01859492        -0.1055004
## 3   WALKING       3         0.2755675       -0.01717678        -0.1126749
## 4   WALKING       4         0.2785820       -0.01483995        -0.1114031
## 5   WALKING       5         0.2778423       -0.01728503        -0.1077418
## 6   WALKING       6         0.2836589       -0.01689542        -0.1103032
## 7   WALKING       7         0.2755930       -0.01865367        -0.1109122
## 8   WALKING       8         0.2746863       -0.01866289        -0.1072521
## 9   WALKING       9         0.2785028       -0.01808920        -0.1108205
## 10  WALKING      10         0.2785741       -0.01702235        -0.1090575
## ..      ...     ...               ...               ...               ...
## Variables not shown: tGravityAcc-mean()-X (dbl), tGravityAcc-mean()-Y
##   (dbl), tGravityAcc-mean()-Z (dbl), tBodyAccJerk-mean()-X (dbl),
##   tBodyAccJerk-mean()-Y (dbl), tBodyAccJerk-mean()-Z (dbl),
##   tBodyGyro-mean()-X (dbl), tBodyGyro-mean()-Y (dbl), tBodyGyro-mean()-Z
##   (dbl), tBodyGyroJerk-mean()-X (dbl), tBodyGyroJerk-mean()-Y (dbl),
##   tBodyGyroJerk-mean()-Z (dbl), tBodyAccMag-mean() (dbl),
##   tGravityAccMag-mean() (dbl), tBodyAccJerkMag-mean() (dbl),
##   tBodyGyroMag-mean() (dbl), tBodyGyroJerkMag-mean() (dbl),
##   fBodyAcc-mean()-X (dbl), fBodyAcc-mean()-Y (dbl), fBodyAcc-mean()-Z
##   (dbl), fBodyAccJerk-mean()-X (dbl), fBodyAccJerk-mean()-Y (dbl),
##   fBodyAccJerk-mean()-Z (dbl), fBodyGyro-mean()-X (dbl),
##   fBodyGyro-mean()-Y (dbl), fBodyGyro-mean()-Z (dbl), fBodyAccMag-mean()
##   (dbl), fBodyBodyAccJerkMag-mean() (dbl), fBodyBodyGyroMag-mean() (dbl),
##   fBodyBodyGyroJerkMag-mean() (dbl), tBodyAcc-std()-X (dbl),
##   tBodyAcc-std()-Y (dbl), tBodyAcc-std()-Z (dbl), tGravityAcc-std()-X
##   (dbl), tGravityAcc-std()-Y (dbl), tGravityAcc-std()-Z (dbl),
##   tBodyAccJerk-std()-X (dbl), tBodyAccJerk-std()-Y (dbl),
##   tBodyAccJerk-std()-Z (dbl), tBodyGyro-std()-X (dbl), tBodyGyro-std()-Y
##   (dbl), tBodyGyro-std()-Z (dbl), tBodyGyroJerk-std()-X (dbl),
##   tBodyGyroJerk-std()-Y (dbl), tBodyGyroJerk-std()-Z (dbl),
##   tBodyAccMag-std() (dbl), tGravityAccMag-std() (dbl),
##   tBodyAccJerkMag-std() (dbl), tBodyGyroMag-std() (dbl),
##   tBodyGyroJerkMag-std() (dbl), fBodyAcc-std()-X (dbl), fBodyAcc-std()-Y
##   (dbl), fBodyAcc-std()-Z (dbl), fBodyAccJerk-std()-X (dbl),
##   fBodyAccJerk-std()-Y (dbl), fBodyAccJerk-std()-Z (dbl),
##   fBodyGyro-std()-X (dbl), fBodyGyro-std()-Y (dbl), fBodyGyro-std()-Z
##   (dbl), fBodyAccMag-std() (dbl), fBodyBodyAccJerkMag-std() (dbl),
##   fBodyBodyGyroMag-std() (dbl), fBodyBodyGyroJerkMag-std() (dbl)
```

```r
# Save the resulting dataset to a file:
write.table(x = new.df,
            file = "new_df.txt",
            quote=F,
            row.names=F)
```

I guess that's all, folks! :)
