## How to run run_analysis.R

Prior running run_analysis.R we need to setwd() to the folder that contains the dataset.

For example on my environment it would be: setwd(/Users/Lyuben/Desktop/R/coursera/UCI HAR Dataset-2)

## How to read run_analysis.R

The order of the functions in run_analysis.R is not necessarily the best order for

reading it, however it is the necessary order for the script to execute successfully.

Smaller utility functions tend to be on top of the file while the 
more complex ones are in the middle or the bottom of the file.

The last few lines of run_analysis.R invoke runAnalysis() method.

## Approach taken

I have created a large number of small utility functions to address different tasks along the process.

The workflow is as follows:
1. read vector stream to memory

2. merge test and train streams

3. add activities and subjects

4. out of all columns/features leave only those who are for std and mean variables

5. use group_by to achieve calculating the mean per activity per subject


However along the way there are many smaller tasks that I solve with many small functions.

## View in RStudio

I have included two calls to View() at the end of run_analysis.R

These open two things:

* A data set with feature metadata describing each feature.
* Independent tidy data set with the average of each variable for each activity and each subject.

## Circumstances

It is very unfortunate I had 38 degrees all 3 days when preparing this assignment.

I think I might have overcomplicated the code a bit, and maybe I am missing something obvious as an easy way to improve.

For example, the final naming of the columns could be dynamically userfriendly name as seen in View(featureMetadata)'s "ReadableName", however I kept hitting a bug where dplyr kept insisting
that there were duplicate column names.

## Codebook

I have automatically generated the codebook in a variable called "featureMetadata".

One can view it with View(featureMetadata) after loading my run_analysis.R 

Nevertheless, I am embedding a csv file with its content in a file https://github.com/lmilev/tidydata/blob/master/feature_codebook.csv in the repository.

I am using this codebook to programatically search for feature columns I am interested in.

