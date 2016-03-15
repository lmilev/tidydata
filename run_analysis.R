library(stringr)
library(dplyr)

containsTextSingle <- function(line,text) {
  x <- str_extract(string = line, pattern = text);
  if (!is.na(x) && text == x) {
    TRUE
  } else {
    NA
  }
}

containsText <- function(lines, text) {
  size <- length(lines)
  result <- logical(size);
  for (i in 1:size) {
    line <- lines[i];
    result[i] <- containsTextSingle(line, text);
  }
  result
}

createEmptyVector <- function(type, size) {
  empty <- rep(NA, size)
  as.vector(empty, type)
}

# A helper function to create a list with names of variables 
# and values with their human readable description.
getKnownVariablesMetadata <- function() {
  #Taken from features_info.txt
  variablesRaw <- 
    "meanFreq(): Weighted average of the frequency components to obtain a mean frequency
  mean(): Mean value
  std(): Standard deviation
  mad(): Median absolute deviation 
  max(): Largest value in array
  min(): Smallest value in array
  sma(): Signal magnitude area
  energy(): Energy measure. Sum of the squares divided by the number of values. 
  iqr(): Interquartile range 
  entropy(): Signal entropy
  arCoeff(): Autorregresion coefficients with Burg order equal to 4
  correlation(): correlation coefficient between two signals
  maxInds(): index of the frequency component with largest magnitude
  skewness(): skewness of the frequency domain signal 
  kurtosis(): kurtosis of the frequency domain signal 
  bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
  angle(): Angle between to vectors.
  ";
  lineregex <- "[:alpha:]+[:punct:][:punct:][:punct:][[:alnum:][:space:]]+[:alnum:]+[:punct:]?[:space:]?\n";
  vars <- str_extract_all(variablesRaw, lineregex)[[1]]
  knownVariables <- vector(mode="list");
  for(i in seq_along(vars)) {
    var <- vars[i];
    pair <- str_split(string = var, ": ");
    name <- pair[[1]][1];
    name <- gsub("()", "", name, fixed = TRUE);
    desc <- pair[[1]][2];
    desc <- gsub("\n", "", desc, fixed = TRUE);
    knownVariables[name] <- desc;
  }
  
  knownVariables;
}

extractOriginalNames <- function(lines) {
  size <- length(lines)
  result <- character(size);
  for (i in 1:size) {
    line <- lines[i]
    r <- unlist(str_split(string = line, pattern = " ", n=2))
    result[i] <- r[2]
  }
  result
}

extractReadableNames <- function(lines) {
  orig <- extractOriginalNames(lines)
  size <- length(lines)
  result <- character(size);
  for (i in 1:size) {
    line <- lines[i]
    line <- str_replace(string = line, pattern = "^[:digit:]+ [t]{1}", "TimeDomain")
    line <- str_replace(string = line, pattern = "^[:digit:]+ [f]{1}", "FrequencyDomain")
    line <- str_replace(string = line, pattern = "Acc", "Acceleration")
    line <- str_replace(string = line, pattern = "Mag", "Magnitute")
    line <- str_replace(string = line, pattern = "Gyro", "Gyroscope")
    # using u,c,br ti avoid placing characters not valid for a column name.
    line <- str_replace_all(string = line, pattern = "-", ".")
    line <- str_replace_all(string = line, pattern = ",", "x")
    line <- str_replace_all(string = line, pattern = "[()]{2}", "")
    line <- str_replace_all(string = line, pattern = "BodyBody", "Body")
    result[i] <- line
  }
  result
}

extractFeatureId <- function(lines) {
  size <- length(lines)
  result <- integer(size);
  for (i in 1:size) {
    line <- lines[i];
    idregex <- "^[:digit:]+"
    id <- str_extract(line, idregex);
    id <- as.integer(id);
    result[i] <- id
  }
  result
}

extractIsDomain <- function(lines,domainId) {
  size <- length(lines)
  result <- logical(size);
  for (i in 1:size) {
    line <- lines[i];
    domainRegex <- paste("[:digit:]+.", domainId, sep="");
    x <- str_extract(string = line, pattern = domainRegex);
    if (!is.na(x) && length(x) > 0) {
      result[i] <- TRUE
    } else {
      result[i] <- NA
    }
  }
  result
}

extractCoordinates <- function(lines) {
  r <- "[XYZ,[:digit:]*]*$"
  size <- length(lines)
  result <- character(size);
  for (i in 1:554) {
    line <- lines[i];
    x <- str_extract(string = line, pattern = r);
    if (!is.na(x) && nchar(x) > 0) {
      result[i] <- x
    } else {
      result[i] <- NA
    }
  }
  angleregex <- ".[tXYZ]+[[:alnum:]+,[:punct:]]+"
  for (i in 555:size) {
    line <- lines[i];
    x <- str_extract(line, angleregex)
    if (!is.na(x) && nchar(x) > 0) {
      x <- gsub(")", "",x = x,fixed = TRUE);
      x <- gsub("(", "",x = x,fixed = TRUE);
      # todo not a bad idea to move angle to a different related table.
      result[i] <- x
    } else {
      result[i] <- NA
    }
  }
  result
}

extractVariableNames <- function(lines) {
  vars <- getKnownVariablesMetadata();
  varNames <- names(vars)
  size <- length(lines)
  result <- character(size);
  for (i in 1:size) {
    line <- lines[i];
    found <- FALSE;
    for (v in seq_along(varNames)) {
      vname <- varNames[v]
      check <- containsTextSingle(line, vname);
      if (!is.na(check) && check){
        result[i] <- vname;
        found <- TRUE
        break
      }
    }
    if (!found) {
      result[i] = NA
    }
  }
  as.factor(result)
}

extractVariableDescription <- function(lines) {
  vars <- getKnownVariablesMetadata();
  varNames <- names(vars)
  size <- length(lines)
  result <- character(size);
  for (i in 1:size) {
    line <- lines[i];
    found <- FALSE;
    for (v in seq_along(varNames)) {
      vname <- varNames[v]
      check <- containsTextSingle(line, vname);
      if (!is.na(check) && check){
        result[i] <- vars[v];
        found <- TRUE
        break
      }
    }
    if (!found) {
      result[i] = NA
    }
  }
  as.factor(unlist(result));
}

# For the features vector of 561 features.
# Each line contains information for:
# - position of the feature within the vector
# - signal measured
# - variable for the signal measured
# - time / frequency domain
# - additional coordinates X,Y,Z, etc.
#
# Returned dataframe would provide reasonable feature metadata that can be later
# mapped to values from the 561-readings vector:
# FeatureId IsTimeDomain IsFrequencyDomain IsAccelerometer IsGyroscope IsBody IsGravity IsJerk
# IsMagnitute VariableName VariableDescription
readFeaturesMetadata <- function(featuresFile) {
  lines <- readLines(featuresFile);
  
  size <- length(lines)
  
  df <- data.frame( 
    FeatureId = createEmptyVector("integer", size),
    IsTimeDomain = createEmptyVector("logical", size),
    IsFrequencyDomain = createEmptyVector("logical", size),
    IsAccelerometer = createEmptyVector("logical", size),
    IsGyroscope = createEmptyVector("logical", size),
    IsBody = createEmptyVector("logical", size),
    IsGravity = createEmptyVector("logical", size),
    IsJerk = createEmptyVector("logical", size),
    IsMagnitute = createEmptyVector("logical", size),
    Coordinates = createEmptyVector("character", size),
    VariableName = createEmptyVector("character", size),
    VariableDescription = createEmptyVector("character", size), 
    OriginalName = createEmptyVector("character", size),
    ReadableName = createEmptyVector("character", size),
    stringsAsFactors = FALSE);
  # NOTE: angle is special case and we need to address it later 
  # by a custom loop over last elements
  
  df["FeatureId"] <- extractFeatureId(lines);
  df["IsTimeDomain"] <- extractIsDomain(lines, "t");
  df["IsFrequencyDomain"] <- extractIsDomain(lines, "f")
  df["IsAccelerometer"] <- containsText(lines, "Acc")
  df["IsGyroscope"] <- containsText(lines, "Gyro")
  df["IsBody"] <- containsText(lines, "Body")
  df["IsGravity"] <- containsText(lines, "Gravity")
  df["IsJerk"] <- containsText(lines, "Jerk")
  df["IsMagnitute"] <- containsText(lines, "Mag")
  df["Coordinates"] <- extractCoordinates(lines);
  df["VariableName"] <- extractVariableNames(lines);
  df["VariableDescription"] <- extractVariableDescription(lines);
  df["OriginalName"] <- extractOriginalNames(lines);
  df["ReadableName"] <- extractReadableNames(lines);
  df
}

# this is detailed feature description of each featureid column in mainDataSet
featureMetadata <- readFeaturesMetadata("features.txt")

runAnalysis <- function() {
  # this is detailed feature description of each featureid column in mainDataSet
  featureMetadata <- readFeaturesMetadata("features.txt")
  
  mainDataSet <- mergeMainDataSets(r1 = "test/X_test.txt", s1 = "test/subject_test.txt", a1 = "test/y_test.txt",
                    r2 = "train/X_train.txt", s2 = "train/subject_train.txt", a2 = "train/y_train.txt"
  );
  
  # this is the basic stream form of the dataset with subject, and activity added
  mainDataSet <- tbl_df(mainDataSet);
  mainDataSet <- rename(mainDataSet, ActivityId=activity, SubjectId=subject)
  meanstdFiltered <- filter(featureMetadata, VariableName == "mean" | VariableName == "std")
  columnsToSelect <- c(
    convertReadableColumnName(meanstdFiltered["FeatureId"][[1]], featureMetadata),
    "SubjectId",
    "ActivityId")
  
  meanstdSelected <- select(mainDataSet, one_of(columnsToSelect))
  activities <- readActivityTable("activity_labels.txt")
  meanstdSelected <- meanstdSelected %>% left_join(activities, by="ActivityId")

  s <- summarise(group_by(meanstdSelected, ActivityId, SubjectId), 
            Mean.TimeDomainBodyAcceleration.mean.X=mean(FeatureId1.TimeDomainBodyAcceleration.mean.X),               
            Mean.TimeDomainBodyAcceleration.mean.Y=mean(FeatureId2.TimeDomainBodyAcceleration.mean.Y),                 
            Mean.TimeDomainBodyAcceleration.mean.Z=mean(FeatureId3.TimeDomainBodyAcceleration.mean.Z),                  
            Mean.TimeDomainBodyAcceleration.std.X = mean(FeatureId4.TimeDomainBodyAcceleration.std.X),                   
            Mean.TimeDomainBodyAcceleration.std.Y=mean(FeatureId5.TimeDomainBodyAcceleration.std.Y),                   
            Mean.TimeDomainBodyAcceleration.std.Z=mean(FeatureId6.TimeDomainBodyAcceleration.std.Z),                   
            Mean.TimeDomainGravityAcceleration.mean.X=mean(FeatureId41.TimeDomainGravityAcceleration.mean.X),              
            Mean.TimeDomainGravityAcceleration.mean.Y=mean(FeatureId42.TimeDomainGravityAcceleration.mean.Y),              
            Mean.TimeDomainGravityAcceleration.mean.Z=mean(FeatureId43.TimeDomainGravityAcceleration.mean.Z),              
            Mean.TimeDomainGravityAcceleration.std.X=mean(FeatureId44.TimeDomainGravityAcceleration.std.X),               
            Mean.TimeDomainGravityAcceleration.std.Y=mean(FeatureId45.TimeDomainGravityAcceleration.std.Y),               
            Mean.TimeDomainGravityAcceleration.std.Z=mean(FeatureId46.TimeDomainGravityAcceleration.std.Z),               
            Mean.TimeDomainBodyAccelerationJerk.mean.X=mean(FeatureId81.TimeDomainBodyAccelerationJerk.mean.X),             
            Mean.TimeDomainBodyAccelerationJerk.mean.Y=mean(FeatureId82.TimeDomainBodyAccelerationJerk.mean.Y),             
            Mean.TimeDomainBodyAccelerationJerk.mean.Z=mean(FeatureId83.TimeDomainBodyAccelerationJerk.mean.Z),             
            Mean.TimeDomainBodyAccelerationJerk.std.X = mean(FeatureId84.TimeDomainBodyAccelerationJerk.std.X),              
            Mean.TimeDomainBodyAccelerationJerk.std.Y = mean(FeatureId85.TimeDomainBodyAccelerationJerk.std.Y),              
            Mean.TimeDomainBodyAccelerationJerk.std.Z = mean(FeatureId86.TimeDomainBodyAccelerationJerk.std.Z),              
            Mean.TimeDomainBodyGyroscope.mean.X=mean(FeatureId121.TimeDomainBodyGyroscope.mean.X),                   
            Mean.TimeDomainBodyGyroscope.mean.Y=mean(FeatureId122.TimeDomainBodyGyroscope.mean.Y),                   
            Mean.TimeDomainBodyGyroscope.mean.Z=mean(FeatureId123.TimeDomainBodyGyroscope.mean.Z),                   
            Mean.TimeDomainBodyGyroscope.std.X=mean(FeatureId124.TimeDomainBodyGyroscope.std.X),                    
            Mean.TimeDomainBodyGyroscope.std.Y=mean(FeatureId125.TimeDomainBodyGyroscope.std.Y),                    
            Mean.TimeDomainBodyGyroscope.std.Z=mean(FeatureId126.TimeDomainBodyGyroscope.std.Z),                    
            Mean.TimeDomainBodyGyroscopeJerk.mean.X=mean(FeatureId161.TimeDomainBodyGyroscopeJerk.mean.X),               
            Mean.TimeDomainBodyGyroscopeJerk.mean.Y=mean(FeatureId162.TimeDomainBodyGyroscopeJerk.mean.Y),               
            Mean.TimeDomainBodyGyroscopeJerk.mean.Z=mean(FeatureId163.TimeDomainBodyGyroscopeJerk.mean.Z),               
            Mean.TimeDomainBodyGyroscopeJerk.std.X=mean(FeatureId164.TimeDomainBodyGyroscopeJerk.std.X),                
            Mean.TimeDomainBodyGyroscopeJerk.std.Y=mean(FeatureId165.TimeDomainBodyGyroscopeJerk.std.Y),                
            Mean.TimeDomainBodyGyroscopeJerk.std.Z=mean(FeatureId166.TimeDomainBodyGyroscopeJerk.std.Z),                
            Mean.TimeDomainBodyAccelerationMagnitute.mean=mean(FeatureId201.TimeDomainBodyAccelerationMagnitute.mean),         
            Mean.TimeDomainBodyAccelerationMagnitute.std=mean(FeatureId202.TimeDomainBodyAccelerationMagnitute.std),          
            Mean.TimeDomainGravityAccelerationMagnitute.mean=mean(FeatureId214.TimeDomainGravityAccelerationMagnitute.mean),      
            Mean.TimeDomainGravityAccelerationMagnitute.std=mean(FeatureId215.TimeDomainGravityAccelerationMagnitute.std),       
            Mean.TimeDomainBodyAccelerationJerkMagnitute.mean=mean(FeatureId227.TimeDomainBodyAccelerationJerkMagnitute.mean),     
            Mean.TimeDomainBodyAccelerationJerkMagnitute.std=mean(FeatureId228.TimeDomainBodyAccelerationJerkMagnitute.std),      
            Mean.TimeDomainBodyGyroscopeMagnitute.mean=mean(FeatureId240.TimeDomainBodyGyroscopeMagnitute.mean),            
            Mean.TimeDomainBodyGyroscopeMagnitute.std=mean(FeatureId241.TimeDomainBodyGyroscopeMagnitute.std),             
            Mean.TimeDomainBodyGyroscopeJerkMagnitute.mean=mean(FeatureId253.TimeDomainBodyGyroscopeJerkMagnitute.mean),        
            Mean.TimeDomainBodyGyroscopeJerkMagnitute.std=mean(FeatureId254.TimeDomainBodyGyroscopeJerkMagnitute.std),         
            Mean.FrequencyDomainBodyAcceleration.mean.X=mean(FeatureId266.FrequencyDomainBodyAcceleration.mean.X),           
            Mean.FrequencyDomainBodyAcceleration.mean.Y=mean(FeatureId267.FrequencyDomainBodyAcceleration.mean.Y),           
            Mean.FrequencyDomainBodyAcceleration.mean.Z=mean(FeatureId268.FrequencyDomainBodyAcceleration.mean.Z),           
            Mean.FrequencyDomainBodyAcceleration.std.X=mean(FeatureId269.FrequencyDomainBodyAcceleration.std.X),            
            Mean.FrequencyDomainBodyAcceleration.std.Y=mean(FeatureId270.FrequencyDomainBodyAcceleration.std.Y),            
            Mean.FrequencyDomainBodyAcceleration.std.Z=mean(FeatureId271.FrequencyDomainBodyAcceleration.std.Z),            
            Mean.FrequencyDomainBodyAccelerationJerk.mean.X=mean(FeatureId345.FrequencyDomainBodyAccelerationJerk.mean.X),       
            Mean.FrequencyDomainBodyAccelerationJerk.mean.Y=mean(FeatureId346.FrequencyDomainBodyAccelerationJerk.mean.Y),       
            Mean.FrequencyDomainBodyAccelerationJerk.mean.Z=mean(FeatureId347.FrequencyDomainBodyAccelerationJerk.mean.Z),       
            Mean.FrequencyDomainBodyAccelerationJerk.std.X=mean(FeatureId348.FrequencyDomainBodyAccelerationJerk.std.X),        
            Mean.FrequencyDomainBodyAccelerationJerk.std.Y=mean(FeatureId349.FrequencyDomainBodyAccelerationJerk.std.Y),        
            Mean.FrequencyDomainBodyAccelerationJerk.std.Z=mean(FeatureId350.FrequencyDomainBodyAccelerationJerk.std.Z),        
            Mean.FrequencyDomainBodyGyroscope.mean.X=mean(FeatureId424.FrequencyDomainBodyGyroscope.mean.X),              
            Mean.FrequencyDomainBodyGyroscope.mean.Y=mean(FeatureId425.FrequencyDomainBodyGyroscope.mean.Y),              
            Mean.FrequencyDomainBodyGyroscope.mean.Z=mean(FeatureId426.FrequencyDomainBodyGyroscope.mean.Z),              
            Mean.FrequencyDomainBodyGyroscope.std.X=mean(FeatureId427.FrequencyDomainBodyGyroscope.std.X),               
            Mean.FrequencyDomainBodyGyroscope.std.Y=mean(FeatureId428.FrequencyDomainBodyGyroscope.std.Y),               
            Mean.FrequencyDomainBodyGyroscope.std.Z=mean(FeatureId429.FrequencyDomainBodyGyroscope.std.Z),               
            Mean.FrequencyDomainBodyAccelerationMagnitute.mean=mean(FeatureId503.FrequencyDomainBodyAccelerationMagnitute.mean),    
            Mean.FrequencyDomainBodyAccelerationMagnitute.std=mean(FeatureId504.FrequencyDomainBodyAccelerationMagnitute.std),     
            Mean.FrequencyDomainBodyAccelerationJerkMagnitute.mean=mean(FeatureId516.FrequencyDomainBodyAccelerationJerkMagnitute.mean),
            Mean.FrequencyDomainBodyAccelerationJerkMagnitute.std=mean(FeatureId517.FrequencyDomainBodyAccelerationJerkMagnitute.std), 
            Mean.FrequencyDomainBodyGyroscopeMagnitute.mean=mean(FeatureId529.FrequencyDomainBodyGyroscopeMagnitute.mean),       
            Mean.FrequencyDomainBodyGyroscopeMagnitute.std=mean(FeatureId530.FrequencyDomainBodyGyroscopeMagnitute.std),        
            Mean.FrequencyDomainBodyGyroscopeJerkMagnitute.mean=mean(FeatureId542.FrequencyDomainBodyGyroscopeJerkMagnitute.mean),   
            Mean.FrequencyDomainBodyGyroscopeJerkMagnitute.std=mean(FeatureId543.FrequencyDomainBodyGyroscopeJerkMagnitute.std))
  s %>% left_join(activities, by="ActivityId")
}

readActivityTable <- function(activity_def_file) {
  lines <- readSimpleFile(activity_def_file)
  ids <- integer(length(lines))
  activities <- character(length(ids))
  for(i in seq_along(lines)) {
    line <- lines[i]
    r <- str_split_fixed(line," ", n=2)
    ids[i] <- as.integer(r[1])
    activities[i] <- r[2]
  }
  
  df <- tbl_df(data.frame(ActivityId=ids, ActivityName=activities))
  mutate(df, IsWalking = (ActivityId <= 3), IsAtRest = (ActivityId>3))
}

convertReadableColumnName <- function(existingColumnNames, featureMetadata) {
  result <- character(length(existingColumnNames))
  for(i in seq_along(result)) {
    if (i>561) {
      break
    }
    existing <- as.integer(existingColumnNames[i]);
    newname_part1 <- featureMetadata[which(featureMetadata$FeatureId == existing),"FeatureId"]
    newname_part2 <- featureMetadata[which(featureMetadata$FeatureId == existing),"ReadableName"]
    #the reason i cant use simpler column names is because i get column already exists errors
    result[i] <- paste("FeatureId",newname_part1,".",newname_part2,sep="")
    #result[i] <- paste(".", newname_part2,sep = "")
  }
  if (length(result) > 561) {
    for (i in 562:length(result)) {
      result[i] <- existingColumnNames[i]
    }
  }
  result
}

# Read a file containing a stream of 
# numbers in engineering notation.
#
# This approach intentionally does not use readings per line information.
# Later processing can identify which consequent elements belong to which so-called readings vector
# based on the known size i.e. 128 or 561 
readNumbersStream <- function(fileName) {
  regex <- "[:punct:]?[:digit:]+[:punct:]{1}[:digit:]*[:alnum:]{1}(.[:digit:]{3})";
  fileContent <- readLines(fileName);
  matches <- str_extract_all(fileContent, regex);
  unlist(matches)
}

# reads lines from a file, useful for "y_test.txt"
# where each line represents the activity identifier.
readSimpleFile <- function(fileName) {
  lines <- readLines(fileName);
  unlist(lines)
}

# I have chosen to keep column names simple, but
# we need to refer to the metadata taken in readFeaturesMetadata.
# Use readFeaturesMetadata's FeatureId to determine which is the column name
# for the feature. i.e. FeatureId5 would be for TimeDomain Body Acceleration Standard Deviation Y coordinate.
readMainDataSet <- function(readingsfile, subjectfile, activityfile) {
  readings <- as.numeric(readNumbersStream(readingsfile))
  mr <- matrix(readings, ncol = 561)
  subject <- as.integer(readSimpleFile(subjectfile))
  activity <- as.integer(readSimpleFile(activityfile))
  mr <- cbind(cbind(mr, subject), activity);
  df <- data.frame(mr);
  names <- names(df)
  for (i in seq_along(names)) {
    n <- names[i]
    n <- gsub("V", "", n, fixed=TRUE);
    names[i] <- n
  }
  names(df) <- convertReadableColumnName(names,featureMetadata)
  df
}

mergeMainDataSets <- function(r1,s1,a1,r2,s2,a2) {
  ds1 <- readMainDataSet(r1,s1,a1);
  ds2 <- readMainDataSet(r2,s2,a2);
  ds <- rbind(ds1,ds2)
  ds
}

# Loads contents of Inertial Signals files into a table.
# xfile - for example body_acc_x_test.txt
# yfile - for example body_acc_y_test.txt
# zfile - for example body_acc_z_test.txt
# The returned data frame contains a continuous stream of readings from X Y and Z.
# The data frame returned should be additional processed to mark the 128 elements in the so-called reading vector. 
#
# The returned data frame has already converted the values to numeric from the engineering notation.
readInertialSignalsFrame <- function(xfile, yfile, zfile) {
  x <- readNumbersStream(xfile);
  x <- unlist(sapply(x, as.numeric));
  y <- readNumbersStream(yfile);
  y <- unlist(sapply(y, as.numeric));
  z <- readNumbersStream(zfile);
  z <- unlist(sapply(z, as.numeric));
  data.frame(x,y,z);
}


result <- runAnalysis()

#View(featureMetadata)
#View(result)

result

