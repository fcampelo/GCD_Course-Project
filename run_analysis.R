#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Source code for Getting and Cleaning Data::Course Project
# By Felipe Campelo (fcampelo@ufmg.br / fcampelo@gmail.com)
#==========================================================
# This file (apart from this header) was generated from my R Markdown original
# using:
#
# > library(knitr)
# > purl("README.Rmd","run_analysis.R",documentation=0)
#
# For a detailed explanation of what is being done here, please refer to either 
# "run_analysis.Rmd" or "run_analysis.md".
#
# Cheers,
# Felipe
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

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



# Find out how many columns have at least one other under the same name
dups<-which(duplicated(names(full.data)))
length(dups)

# Notice that the names are duplicated, but the contents are not:
col1<-names(full.data)[dups[1]]
indx<-which(names(full.data)==col1)
identical(full.data[,indx[1]],full.data[,indx[2]])



# get the names of the relevant columns
dupnames<-names(full.data)[dups]

# Modify these names so that each is slightly but surely different from all 
# others. It can be done by simply adding a unique number before each name, and 
# then re-attributing the (now modified) column names to the relevant columns
names(full.data)[dups]<-paste0(1:84,dupnames)

# And voila'! All is good again!
which(duplicated(names(full.data)))



# Select only the columns that contain 'mean()' or 'std()' in their names
mean.sd.data<-select(full.data,
                     c(Subject,
                       Activity,
                       contains("mean()"),contains("std()")))



# Check that the Activity factor has descriptive level names
summary(mean.sd.data$Activity)



# Check that the Columns have descriptive names
names(mean.sd.data)



# Assemble new data frame
new.df<-mean.sd.data %>%              # Get the mean.sd.data data frame tbl
  group_by(Activity,Subject) %>%      # group it by Activity and Subject
  summarise_each(funs(mean)) %>%      # Summarize all other columns by mean 
  arrange(Activity,Subject) %>%       # Sort rows by Activity and subject
  mutate(Subject=as.factor(Subject))  # And finally convert Subject into the factor variable it rightfully is.

print(new.df)

# Save the resulting dataset to a file:
write.table(x = new.df,
            file = "new_df.txt",
            quote=F,
            row.names=F)


