---
title: "README"
---

## How to run run_analysis.R

Prior running run_analysis.R we need to setwd() to the folder that contains the dataset.

For example on my environment it would be: setwd(/Users/Lyuben/Desktop/R/coursera/UCI HAR Dataset-2)

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
