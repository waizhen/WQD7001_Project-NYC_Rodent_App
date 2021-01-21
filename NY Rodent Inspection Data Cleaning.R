##### DATA CLEANING #####

## Note that the data cleaning process mainly involves:
# 1. Managing duplicates
# 2. Missing values
# 3. Finding outliers
# 4. Converting data types
# 5. Filtering rows
# 6. Removing irrelevant columns

library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)

# Load the csv file and take a look
df <- read.csv("D:/Documents/UM Master of Data Science/WQD7001 Principles of Data Science/Group project/Rodent_Inspection.csv", 
               header = TRUE, sep = ',', stringsAsFactors = FALSE)

# View the first few rows
head(df, 3)

# Quick summary
summary(df)

dim(df); names(df)
str(df)


#
# We have done some data exploration on the raw dataset previously on every columns and we have roughly figure 
# out what to do with each of them. As usual, we will start from the left to the right and we may do some of them
# together simultaneously depending on the need.
#



### STEP 1 - CONVERTING DATA TYPES ###
# For our case, there are no duplicate rows, so it's safe for us to do data conversion in step 2
# Note that if there are duplicate entries, it is always better to do, so we'll still do this in step 1

## (1) Inspection Type:
# We determined previously that this column has a set of categorical values and it should be treated as 
# categorical variable so it can be converted into factor.
# This may be data transformation but I guess we'll still do it for now.

# View the class of the column
class(df$INSPECTION_TYPE)

# View and convert the column in factor type
as.factor(df$INSPECTION_TYPE)[1:5]
df$INSPECTION_TYPE <- as.factor(df$INSPECTION_TYPE)

# Check the column again
df$INSPECTION_TYPE[11:20]

# But I don't like the fact that some words are uppercase and some are lower case
# We'll change the levels here.
# Since this is a nominal and not ordinal column, no order is needed here.
levels(df$INSPECTION_TYPE) <- c("Bait", "Clean Ups", "Compliance", "Initial", "Stoppage")
levels(df$INSPECTION_TYPE)

# Check the rows that have Bait and Stoppage, see if the value cases have been changed or not
df %>%
  filter((INSPECTION_TYPE == "Bait") | (INSPECTION_TYPE == "Stoppage")) %>%
  select(INSPECTION_TYPE, JOB_TICKET_OR_WORK_ORDER_ID, JOB_ID)

# Double check the integer values of a few rows
unclass(df$INSPECTION_TYPE[11:20])


## NOTE: We'll leave job id and job progress for now since we have some decisions to make ##



## (16) Borough:
# Like last one, We know previously that this column has a set of categorical values and should be treated as 
# categorical variable so it can be converted into factor.
# This may be data transformation from my understanding.

# View the class & attribute of the column
class(df$BOROUGH)

# View and convert the column in factor type
as.factor(df$BOROUGH)[1:3]
df$BOROUGH <- as.factor(df$BOROUGH)

# Check the column again
df$BOROUGH[1:10]

# View the levels of the column
levels(df$BOROUGH)

# I've decided to change the levels to follow the boro code, 
# But somehow it doesn't work smoothly so we leave it first.
# levels(df$BOROUGH) <- c('Manhattan', 'Bronx', 'Brooklyn', 'Queens', 'Staten Island')
# levels(df$BOROUGH)

# Double check the integer values of a few rows
unclass(df$BOROUGH[1:10])



## (17) Inspection Date:
# There are a total of 4 classes of conversion for date time
# However we should only choose one: which is the strptime function

# Date Class: strptime
# Type Conversion: String to Date/Time (much preferred)
# strptime("09/23/2020 01:10:39 AM", format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York")
# Take note that after conversion, this column can be used for feature engineering eg. week day

# View the class & attribute of the column
class(df$INSPECTION_DATE)

# Convert the column to datetime (POSIX) type
df$INSPECTION_DATE <- strptime(df$INSPECTION_DATE, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York")
df$INSPECTION_DATE[1:5]

# View the class again
class(df$INSPECTION_DATE)

# Convert to POSIXct and check how it changes
df$INSPECTION_DATE <- as.POSIXct(df$INSPECTION_DATE)
df$INSPECTION_DATE[1:5]

# View the class again
class(df$INSPECTION_DATE)

# Access the weekday (wday) attribute
# head(df$INSPECTION_DATE$wday)
weekdays(df$INSPECTION_DATE[1:5])

# Access the month (mon) attribute
# head(df$INSPECTION_DATE$mon)
months(df$INSPECTION_DATE[1:5])

# Access the quarter attribute
quarters(df$INSPECTION_DATE[1:5])



## (18) Result:
# Like last one, We know previously that this column has a set of categorical values and better treated as 
# categorical variable so it can be converted into factor.

# View the class & attribute of the column
class(df$RESULT)

# View the column in factor type
as.factor(df$RESULT)[1:3]
df$RESULT <- as.factor(df$RESULT)

# Check the column again
df$RESULT[1:10]

# View the levels of the column
levels(df$RESULT)

# Double check the integer values of a few rows
unclass(df$RESULT[1:10])

# An empty entry has became part of the factor levels, we need to address that later.
# This is unavoidable for now because we only address missing entries later.



## (19) Approved Date:
# Like inspection date, there are a total of 4 classes of conversion for date time
# We discussed that we'll choose one: which is the strptime function

# View the class & attribute of the column
class(df$APPROVED_DATE)

# Convert the column to factor type
df$APPROVED_DATE <- strptime(df$APPROVED_DATE, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York")
df$APPROVED_DATE[1:5]

# View the class again
class(df$APPROVED_DATE)

# Convert to POSIXct and check how it changes
df$APPROVED_DATE <- as.POSIXct(df$APPROVED_DATE)
df$APPROVED_DATE[1:5]

# View the class again
class(df$APPROVED_DATE)

# Access the weekday (wday) attribute
weekdays(df$APPROVED_DATE[1:5])

# Access the month (mon) attribute
months(df$APPROVED_DATE[1:5])

# Access the quarter attribute
quarters(df$APPROVED_DATE[1:5])


#
# What has been done in Data Type Conversion:
# 1) Converted Inspection type, Borough, Result to Factor class.
# 2) Converted Inspection date and Approved date to POSIXct datetime class.
#



### STEP 2 - DEALING WITH DUPLICATES ###
# Let's check for duplicates first and see.

# Identifying duplicates
df[duplicated(df),]

df[duplicated(df[, 2]),]

# Dplyr method
distinct(df)

#
# All the methods above shows 0 duplicated rows and 1899187 distinct rows, means that
# there are no duplicates at all. Good news to us.
#



### STEP 3 - DEALING WITH MISSING VALUES ###
# Note: This is very time consuming and pretty challenging part of the process.
# Looks like there are no empty or NA entries for the first 8 columns so we skip them.
# We start from the 9th column.

## (9) House Number
# There are 33881 empty entries in this house_number column, we'll see what we can do about that.
# We think this column is not so important if we decide to use BBL as the identifier.
# We leave it for now with the assumption of redundancy and prioritise BBL column.


## (10) Street Name (11) Zip Code
# There are 2559 empty entries in street name, well hopefully it won't be a problem later.
# For zip codes, there are 9992 NA entries and a lot of 0 zip codes.
# These columns enable us to establish a regional parameter to look for correlations later.
# There are 1520 BBL codes that have empty street name entries
# 

# Check the unique BBL entries that has empty street name
unique(df[df$STREET_NAME == '', 'BBL'])

# Check one of the BBL to see the number of rows associated
df[(df$STREET_NAME == '') & (df$BBL == 3012840006), c('JOB_TICKET_OR_WORK_ORDER_ID', 'BBL', 'ZIP_CODE')]

# This BBL code has 7 job ticket id, means it has 7 inspections carried out

# Check another BBL along with zip code
df[(df$STREET_NAME == '') & (df$BBL == 1004770071), c('JOB_TICKET_OR_WORK_ORDER_ID', 'BBL', 'ZIP_CODE')]



###########################################################################################

##### IMPORTANT #####

# We use bbl of rows with missing street names to track their counterparts that has a valid street name
# Then, we use those street names to impute the missing street name entries.
# At the same time, we can track the zip codes as well and impute the missing zip code entries.
# Same goes to latitude and longitude.

for (row in row.names(df[df$STREET_NAME == '' & df$LOT != 8900 & df$LOT != 8901,])) {
  bbl <- df[row, 'BBL']
  df[row, 'STREET_NAME'] = df[df$BBL == bbl & df$STREET_NAME != '', 'STREET_NAME'][1]
}

for (row in row.names(df[(df$ZIP_CODE == 0 | is.na(df$ZIP_CODE) == TRUE) & df$LOT != 8900 & df$LOT != 8901,])) {
  bbl <- df[row, 'BBL']
  df[row, 'ZIP_CODE'] = df[df$BBL == bbl & df$STREET_NAME != '', 'ZIP_CODE'][1]
}

for (row in row.names(df[is.na(df$LATITUDE) & is.na(df$LONGITUDE) & df$LOT != 8900 & df$LOT != 8901,])) {
  bbl <- df[row, 'BBL']
  df[row, 'LATITUDE'] = df[df$BBL == bbl & df$STREET_NAME != '' & (is.na(df$LATITUDE) == FALSE), 'LATITUDE'][1]
  df[row, 'LONGITUDE'] = df[df$BBL == bbl & df$STREET_NAME != '' & (is.na(df$LONGITUDE) == FALSE), 'LONGITUDE'][1]
}

##########################################################################################

# Create a spare copy of data frame in case things go wrong, we can reset back to this point

df_copy <- data.frame(df)
head(df_copy, 3)

# Reset data frame
df <- data.frame(df_copy)

###########################################################################################
# We create another data frame containing the bbl, street name and zipcode for the lot with 8900/8901
# Then we get the correct street name and zipcode using provided latitude and longitude and impute them.
#########################################################################################

# df[(df$BOROUGH == 'Manhattan') & (df$BBL == ), c('STREET_NAME', 'BBL', 'BORO_CODE', 'BLOCK', 'LOT', 'BOROUGH', 'ZIP_CODE', 'LATITUDE', 'LONGITUDE', 'X_COORD')]

manhattan_bbl <- sort(unique(df[(df$LOT == 8901 | df$LOT == 8900) & df$BOROUGH == 'Manhattan', 'BBL']))
manhattan_st <- c("THOMPSON STREET", "EAST 121 STREET")
manhattan_zc <- c(10012, 10035)

manhattan8900 <- data.frame('bbl' = manhattan_bbl, 'street' = manhattan_st, 'zipcode' = manhattan_zc, 
                            stringsAsFactors = FALSE)

# Note: BBL 2024748900 is flagged for having 2 street names after imputation, we have to change manually later.
bronx_bbl <- sort(unique(df[(df$LOT == 8901 | df$LOT == 8900) & df$BOROUGH == 'Bronx', 'BBL']))
bronx_st <- c("FIELDSTON ROAD", "EAST 161 STREET", "EAST 162 STREET", "EAST 162 STREET", "GERARD AVENUE#", "EDWARD L GRANT HWY", 
              "EDWARD L GRANT HWY", "UNIVERSITY AVENUE", "EXTERIOR STREET", "LONGWOOD AVENUE", "JEROME AVENUE", "RICHMAN PLAZA", 
              "EAST 176 STREET", "WEST FARMS ROAD", "EAST 178 STREET", "WASHINGTON AVENUE", "EAST 183 STREET", "3 AVENUE", 
              "3 AVENUE", "3 AVENUE", "3 AVENUE", "3 AVENUE", "EAST FORDHAM ROAD", "EAST FORDHAM ROAD", "EAST FORDHAM ROAD", 
              "OLD KINGSBRIDGE ROAD", "EAST 181 STREET", "ARMAND PLACE", "HEATH AVENUE", "CROES AVENUE", "ROSEDALE AVENUE", 
              "WESTCHESTER AVENUE", "EAST TREMONT AVENUE", "EAST TREMONT AVENUE", "WESTCHESTER AVENUE", "NORTH CHESTNUT DRIVE", 
              "EASTCHESTER ROAD", "GUNTHER AVENUE", "WICKHAM AVENUE", "SEXTON PLACE", "GIVAN AVENUE", "EAST TREMONT AVENUE", 
              "LAYTON AVENUE", "LAFAYETTE AVENUE", "LAFAYETTE AVENUE", "LAFAYETTE AVENUE", "MEAGHER AVENUE", "MILES AVENUE", 
              "REYNOLDS AVENUE", "LONGSTREET AVENUE", "ALAN PLACE", "FERN PLACE")
bronx_zc <- c(10471, 10451, 10451, 10451, 10451, 10452, 10452, 10453, 10452, 10474, 10452, 10453, 10457, 10460, 10457, 
              10457, 10457, 10457, 10457, 10457, 10457, 10457, 10458, 10458, 10458, 10460, 10460, 10463, 10463, 10472, 
              10472, 10462, 10460, 10460, 10461, 10467, 10469, 10469, 10469, 10469, 10469, 10461, 10465, 10465, 10465, 
              10465, 10465, 10465, 10465, 10465, 10465, 10465)

bronx8900 <- data.frame('bbl' = bronx_bbl, 'street' = bronx_st, 'zipcode' = bronx_zc, 
                        stringsAsFactors = FALSE)


brooklyn_bbl <- sort(unique(df[(df$LOT == 8901 | df$LOT == 8900) & df$BOROUGH == 'Brooklyn', 'BBL']))
brooklyn_st <- c("HARRISON ALLEY", "TROY AVENUE", "CARROLL STREET", "LEWIS AVENUE", "CUMBERLAND STREET", "PRINCE STREET", 
                 "DEBEVOISE STREET**#", "WOODPOINT ROAD", "NOLL STREET", "RENAISSANCE COURT", "NORWOOD AVENUE", "CRESCENT STREET**", 
                 "EAST 93 STREET", "48 STREET**", "KINGSTON AVENUE", "BAY AVENUE", "CROPSEY AVENUE", "BANNER 3RD ROAD**", 
                 "AMERSFORT PLACE", "AVENUE D**", "AVENUE J**", "EAST 86 STREET**", "MATTHEWS PLACE**", "BRIGHTON 3 STREET", 
                 "BRIGHTON 3 PLACE", "BRIGHTON 4 PLACE", "BRIGHTON 4 COURT", "BRIGHTON 5 LANE", "BRIGHTON 5 COURT", "CONEY ISLAND AVENUE", 
                 "EMMONS AVENUE**")
brooklyn_zc <- c(11201, 11213, 11213, 11233, 11238, 11201, 11201, 11211, 11206, 11206, 11208, 11208, 11212, 11219, 11203, 
                 11230, 11214, 11235, 11210, 11203, 11236, 11236, 11236, 11235, 11235, 11235, 11235, 11235, 11235, 11235, 
                 11235)

brooklyn8900 <- data.frame('bbl' = brooklyn_bbl, 'street' = brooklyn_st, 'zipcode' = brooklyn_zc, 
                           stringsAsFactors = FALSE)


queens_bbl <- sort(unique(df[(df$LOT == 8901 | df$LOT == 8900) & df$BOROUGH == 'Queens', 'BBL']))
queens_st <- c("50 STREET", "NATIONAL STREET", "58 DRIVE", "60 ROAD", "60 COURT", "61 STREET", 
               "ONDERDONK AVENUE", "SUMMERFIELD STREET", "JAMAICA AVENUE", "DILLON STREET**#")
queens_zc <- c(11377, 11368, 11378, 11378, 11378, 11378, 11385, 11385, 11432, 11433)

queens8900 <- data.frame('bbl' = queens_bbl, 'street' = queens_st, 'zipcode' = queens_zc, 
                         stringsAsFactors = FALSE)


staten_bbl <- sort(unique(df[(df$LOT == 8901 | df$LOT == 8900) & df$BOROUGH == 'Staten Island', 'BBL']))
staten_st <- c("VAN DUZER STREET**", "MORNINGSTAR ROAD**", "RICHMOND AVENUE**", "CORYN COURT**")
staten_zc <- c(10304, 10303, 10314, 10312)

staten8900 <- data.frame('bbl' = staten_bbl, 'street' = staten_st, 'zipcode' = staten_zc, 
                         stringsAsFactors = FALSE)

###############################################################################################

# Use a for loop to loop over the newly created data frame and use the street name and zipcode to
# impute the df entries that have lot 8900/8901 

for (row in 1:nrow(manhattan8900)) {
  bbl <- manhattan8900[row, 'bbl']
  if (length(df[(df$STREET_NAME == '') & (df$BOROUGH == 'Manhattan') & (df$BBL == bbl), 'STREET_NAME']) > 0) {
    df[(df$STREET_NAME == '') & (df$BOROUGH == 'Manhattan') & (df$BBL == bbl), 'STREET_NAME'] <- manhattan8900[row, 'street']
  }
  if (length(df[(df$ZIP_CODE == 0) & (df$BOROUGH == 'Manhattan') & (df$BBL == bbl), 'ZIP_CODE']) > 0) {
    df[(df$ZIP_CODE == 0) & (df$BOROUGH == 'Manhattan') & (df$BBL == bbl), 'ZIP_CODE'] <- manhattan8900[row, 'zipcode']
  }
}

for (row in 1:nrow(bronx8900)) {
  bbl <- bronx8900[row, 'bbl']
  if (length(df[(df$STREET_NAME == '') & (df$BOROUGH == 'Bronx') & (df$BBL == bbl), 'STREET_NAME']) > 0) {
    df[(df$STREET_NAME == '') & (df$BOROUGH == 'Bronx') & (df$BBL == bbl), 'STREET_NAME'] <- bronx8900[row, 'street']
  }
  df[((df$ZIP_CODE == 0) | is.na(df$ZIP_CODE)) & (df$BOROUGH == 'Bronx') & (df$BBL == bbl), 'ZIP_CODE'] <- bronx8900[row, 'zipcode']
}

for (row in 1:nrow(brooklyn8900)) {
  bbl <- brooklyn8900[row, 'bbl']
  if (length(df[(df$STREET_NAME == '') & (df$BOROUGH == 'Brooklyn') & (df$BBL == bbl), 'STREET_NAME']) > 0) {
    df[(df$STREET_NAME == '') & (df$BOROUGH == 'Brooklyn') & (df$BBL == bbl), 'STREET_NAME'] <- brooklyn8900[row, 'street']
  }
  if (length(df[((df$ZIP_CODE == 0) | is.na(df$ZIP_CODE)) & (df$BOROUGH == 'Brooklyn') & (df$BBL == bbl), 'ZIP_CODE']) > 0) {
    df[((df$ZIP_CODE == 0) | is.na(df$ZIP_CODE)) & (df$BOROUGH == 'Brooklyn') & (df$BBL == bbl), 'ZIP_CODE'] <- brooklyn8900[row, 'zipcode']
  }
}

for (row in 1:nrow(queens8900)) {
  bbl <- queens8900[row, 'bbl']
  if (length(df[(df$STREET_NAME == '') & (df$BOROUGH == 'Queens') & (df$BBL == bbl), 'STREET_NAME']) > 0) {
    df[(df$STREET_NAME == '') & (df$BOROUGH == 'Queens') & (df$BBL == bbl), 'STREET_NAME'] <- queens8900[row, 'street']
  }
  if (length(df[((df$ZIP_CODE == 0) | is.na(df$ZIP_CODE)) & (df$BOROUGH == 'Queens') & (df$BBL == bbl), 'ZIP_CODE']) > 0) {
    df[((df$ZIP_CODE == 0) | is.na(df$ZIP_CODE)) & (df$BOROUGH == 'Queens') & (df$BBL == bbl), 'ZIP_CODE'] <- queens8900[row, 'zipcode']
  }
}

for (row in 1:nrow(staten8900)) {
  bbl <- staten8900[row, 'bbl']
  if (length(df[((df$ZIP_CODE == 0) | is.na(df$ZIP_CODE)) & (df$BOROUGH == 'Staten Island') & (df$BBL == bbl), 'ZIP_CODE']) > 0) {
    df[((df$ZIP_CODE == 0) | is.na(df$ZIP_CODE)) & (df$BOROUGH == 'Staten Island') & (df$BBL == bbl), 'ZIP_CODE'] <- staten8900[row, 'zipcode']
  }
}

##############################################################################################
##############################################################################################

# Export the cleaned part 0 dataframe to a csv file for conventional purpose in case we have to reset:
# NOTE: the df is from THIS POINT ONWARDS (after imputation of 8900/8901 Lot rows)
# NOTE: remember to run the 8900/8901 df codes if you think it's necessary

write.csv(df, "D:/Documents/UM Master of Data Science/WQD7001 Principles of Data Science/Group project/Rodent_Inspection_clean00.csv", 
          row.names = FALSE)

# Load clean00 data and convert some data types
df <- read.csv("D:/Documents/UM Master of Data Science/WQD7001 Principles of Data Science/Group project/Rodent_Inspection_clean00.csv", 
               header = TRUE, sep = ',', stringsAsFactors = FALSE)

df$INSPECTION_TYPE <- as.factor(df$INSPECTION_TYPE)
df$BOROUGH <- as.factor(df$BOROUGH)
df$INSPECTION_DATE <- strptime(df$INSPECTION_DATE, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
df$INSPECTION_DATE <- as.POSIXct(df$INSPECTION_DATE)
df$RESULT <- as.factor(df$RESULT)
df$APPROVED_DATE <- strptime(df$APPROVED_DATE, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
df$APPROVED_DATE <- as.POSIXct(df$APPROVED_DATE)

##############################################################################################
##############################################################################################

# After Imputation, check the remaining rows that still have empty street names or zip codes
# From my expectations, probably only zip code column will have some empty entries
# By using these specific rows, we cross check with lat long data again to double confirm if we should drop them.

# Display the remaing number of rows with empty street name/zip code
sum(is.na(df$STREET_NAME))
sum(df$STREET_NAME == '', na.rm = TRUE)
sum(is.na(df$ZIP_CODE))
sum(df$ZIP_CODE == 0, na.rm = TRUE)
sum((df$STREET_NAME == '') & (is.na(df$ZIP_CODE)), na.rm = TRUE)
sum((df$STREET_NAME == '') & (df$ZIP_CODE == 0), na.rm = TRUE)
nrow(df[(is.na(df$STREET_NAME)) & (!is.na(df$LATITUDE)),])

# There are still a lot of empty entries!!!
# 754 rows that have empty NA street names, 2277 rows that have 0 zipcode, 7362 rows that have NA zipcode
# No rows at all that have both empty street names and zipcodes
# It looks like there are 606 rows with NA street names have existing lat/long data
# Minus these, we have remaining 148 rows that need to be investigated as well

# Take a look at those rows, see if they can impute them.
df[(is.na(df$STREET_NAME)) & (!is.na(df$LATITUDE)), c('STREET_NAME', 'BBL', 'BORO_CODE', 'BLOCK', 'LOT', 'BOROUGH', 
                                                      'ZIP_CODE', 'LATITUDE', 'LONGITUDE', 'X_COORD')]
df[(is.na(df$STREET_NAME)), c('STREET_NAME', 'BBL', 'BORO_CODE', 'BLOCK', 'LOT', 'BOROUGH', 
                              'ZIP_CODE', 'LATITUDE', 'LONGITUDE', 'X_COORD')]

# We need to impute these rows!!! With the manual method by getting data using lat/long values 

#########################################################################################################

manhattan_bbl <- sort(unique(df[(df$BOROUGH == 'Manhattan') & (is.na(df$STREET_NAME)) & (!is.na(df$LATITUDE)), 'BBL']))
manhattan_st <- c("WHITEHALL STREET", "LIBERTY STREET", "BROADWAY", "WEST BROADWAY", "CANAL STREET", "MADISON STREET", 
                  "CROSBY STREET", "VARICK STREET", "GREENE STREET", "SHINBONE ALLEY", "GREAT JONES ALLEY", "GREAT JONES STREET", 
                  "LAFAYETTE STREET", "WASHINGTON MEWS", "LAFAYETTE COURT", "HUDSON STREET", "GROVE COURT", "RENWICK STREET", 
                  "WASHINGTON STREET", "MILLIGAN PLACE", "PATCHIN PLACE", "WEST 30 STREET", "WEST 30 STREET", "WEST 33 STREET", 
                  "WEST 35 STREET", "BROADWAY ALLEY", "SNIFFEN COURT", "8 AVENUE", "BROADWAY", "RIVERSIDE BOULEVARD", 
                  "RIVERSIDE BOULEVARD", "BROADWAY", "EAST 75 STREET", "EAST 94 STREET", "HENDERSON PLACE", "MADISON AVENUE", 
                  "LEXINGTON AVENUE", "LEXINGTON AVENUE", "5 AVENUE", "CATHEDRAL PARKWAY", "CATHEDRAL PARKWAY", "WEST 133 STREET", 
                  "WEST 133 STREET", "WEST 133 STREET", "WEST 163 STREET", "WEST 163 STREET", "WEST 163 STREET", "RIVERSIDE DRIVE WEST", 
                  "WASHINGTON TERRACE", "RIVERSIDE DRIVE")
manhattan_zc <- c(10004, 10005, 10013, 10013, 10013, 10002, 10013, 10013, 10012, 10012, 10012, 10012, 10003, 10003, 10003, 
                  10013, 10014, 10013, 10013, 10011, 10011, 10001, 10001, 10001, 10001, 10016, 10016, 10036, 10019, 10069, 
                  10069, 10025, 10021, 10128, 10028, 10029, 10029, 10029, 10037, 10026, 10026, 10030, 10030, 10030, 10032, 
                  10032, 10032, 10032, 10033, 10033)

manhattan_na <- data.frame('bbl' = manhattan_bbl, 'street' = manhattan_st, 'zipcode' = manhattan_zc, 
                           stringsAsFactors = FALSE)

# Note: BBL 2024748900 is flagged for having 2 street names after imputation, we have to change manually later.
bronx_bbl <- sort(unique(df[(df$BOROUGH == 'Bronx') & (is.na(df$STREET_NAME)) & (!is.na(df$LATITUDE)), 'BBL']))
bronx_st <- c("JEROME AVENUE", "WEST 170 STREET", "EAST 132 STREET", "EAST 149 STREET", "OAK POINT AVENUE", "LONGWOOD AVENUE", 
              "HUNTS POINT AVENUE", "HUNTS POINT AVENUE", "ANDREWS AVENUE SOUTH", "EAST FORDHAM ROAD", "EAST FORDHAM ROAD", "3 AVENUE", 
              "EAST FORDHAM ROAD", "EAST FORDHAM ROAD", "EAST FORDHAM ROAD", "EAST FORDHAM ROAD", "WEST FORDHAM ROAD", "HEATH AVENUE", 
              "DOROTHEA PLACE", "COLES LANE", "POE PLACE", "BAINBRIDGE AVENUE", "KINNEAR PLACE", "KINNEAR PLACE", 
              "ZEREGA AVENUE", "WERTCHESTER AVENUE", "SAINT RAYMONDS AVENUE", "EAST TREMONT AVENUE", "ROBERTS AVENUE", "EAST 214 STREET", 
              "BRUNER AVENUE", "WICKHAM AVENUE", "CLARENCE AVENUE", "PATRICIA LANE", "EGER PLACE", "EGER PLACE", 
              "EGER PLACE", "DARE PLACE", "BEVY PLACE", "ALAN PLACE", "SCHOFIELD STREET", "EAST BURNSIDE AVENUE", 
              "EAST BURNSIDE AVENUE")
bronx_zc <- c(10452, 10452, 10454, 10455, 10474, 10474, 10459, 10459, 10453, 10458, 10458, 10457, 10458, 10458, 10458, 
              10458, 10468, 10463, 10458, 10458, 10458, 10458, 10462, 10461, 10461, 10472, 10462, 10460, 10461, 10469, 
              10469, 10469, 10465, 10465, 10465, 10465, 10465, 10465, 10465, 10465, 10464, 10453, 10453)

bronx_na <- data.frame('bbl' = bronx_bbl, 'street' = bronx_st, 'zipcode' = bronx_zc, 
                       stringsAsFactors = FALSE)


# Note: Brighton 3 Walk and Brighton Beach are the same, use Brighton 3 Walk/Brighton 4 Walk/Brighton 5 Walk
brooklyn_bbl <-  sort(unique(df[(df$BOROUGH == 'Brooklyn') & (is.na(df$STREET_NAME)) & (!is.na(df$LATITUDE)), 'BBL']))
brooklyn_st <- c("PRESIDENT STREET", "NELSON STREET", "SACKETT STREET", "UNION STREET", "UNION STREET", "UNION STREET", 
                 "NEW YORK AVENUE", "PRESIDENT STREET", "CARROLL STREET", "PRESIDENT STREET", "CARROLL STREET", "NEW YORK AVENUE", 
                 "BROOKLYN AVENUE", "KINGSTON AVENUE", "PRESIDENT STREET", "CARROLL STREET", "BROADWAY", "LAFAYETTE AVENUE", 
                 "NORTH 6 STREET", "JEFFERSON STREET", "TAPSCOTT STREET", "CLARENDON ROAD", "OCEAN AVENUE", "WOODRUFF AVENUE", 
                 "MERMAID AVENUE", "MERMAID AVENUE", "BRIGHTON 3 WALK", "BRIGHTON 4 WALK", "BRIGHTON 5 WALK")
brooklyn_zc <- c(11231, 11231, 11217, 11213, 11213, 11213, 11213, 11213, 11213, 11213, 11213, 11213, 11213, 11213, 11213, 
                 11213, 11221, 11216, 11249, 11206, 11212, 11203, 11225, 11226, 11224, 11224, 11235, 11235, 11235)

brooklyn_na <- data.frame('bbl' = brooklyn_bbl, 'street' = brooklyn_st, 'zipcode' = brooklyn_zc, 
                          stringsAsFactors = FALSE)


queens_bbl <- sort(unique(df[(df$BOROUGH == 'Queens') & (is.na(df$STREET_NAME)) & (!is.na(df$LATITUDE)), 'BBL']))
queens_st <- c("CENTER BOULEVARD", "54 AVENUE", "108 STREET", "60 ROAD", "SUYDAM STREET")
queens_zc <- c(11109, 11373, 11368, 11378, 11385)

queens_na <- data.frame('bbl' = queens_bbl, 'street' = queens_st, 'zipcode' = queens_zc, 
                        stringsAsFactors = FALSE)


staten_bbl <- sort(unique(df[(df$BOROUGH == 'Staten Island') & (is.na(df$STREET_NAME)) & (!is.na(df$LATITUDE)), 'BBL']))
staten_st <- "VAN NAME AVENUE"
staten_zc <- 10303

staten_na <- data.frame('bbl' = staten_bbl, 'street' = staten_st, 'zipcode' = staten_zc, 
                        stringsAsFactors = FALSE)

######################################################################################################

for (row in 1:nrow(manhattan_na)) {
  bbl <- manhattan_na[row, 'bbl']
  if (length(df[(is.na(df$STREET_NAME)) & (df$BOROUGH == 'Manhattan') & (df$BBL == bbl), 'STREET_NAME']) > 0) {
    df[(is.na(df$STREET_NAME)) & (df$BOROUGH == 'Manhattan') & (df$BBL == bbl), 'STREET_NAME'] <- manhattan_na[row, 'street']
  }
  if (length(df[((is.na(df$ZIP_CODE)) | (df$ZIP_CODE == 0)) & (df$BOROUGH == 'Manhattan') & (df$BBL == bbl), 'ZIP_CODE']) > 0) {
    df[((is.na(df$ZIP_CODE)) | (df$ZIP_CODE == 0)) & (df$BOROUGH == 'Manhattan') & (df$BBL == bbl), 'ZIP_CODE'] <- manhattan_na[row, 'zipcode']
  }
}

for (row in 1:nrow(bronx_na)) {
  bbl <- bronx_na[row, 'bbl']
  if (length(df[(is.na(df$STREET_NAME)) & (df$BOROUGH == 'Bronx') & (df$BBL == bbl), 'STREET_NAME']) > 0) {
    df[(is.na(df$STREET_NAME)) & (df$BOROUGH == 'Bronx') & (df$BBL == bbl), 'STREET_NAME'] <- bronx_na[row, 'street']
  }
  if (length(df[((is.na(df$ZIP_CODE)) | (df$ZIP_CODE == 0)) & (df$BOROUGH == 'Bronx') & (df$BBL == bbl), 'ZIP_CODE']) > 0) {
    df[((is.na(df$ZIP_CODE)) | (df$ZIP_CODE == 0)) & (df$BOROUGH == 'Bronx') & (df$BBL == bbl), 'ZIP_CODE'] <- bronx_na[row, 'zipcode']
  }
}

for (row in 1:nrow(brooklyn_na)) {
  bbl <- brooklyn_na[row, 'bbl']
  if (length(df[(is.na(df$STREET_NAME)) & (df$BOROUGH == 'Brooklyn') & (df$BBL == bbl), 'STREET_NAME']) > 0) {
    df[(is.na(df$STREET_NAME)) & (df$BOROUGH == 'Brooklyn') & (df$BBL == bbl), 'STREET_NAME'] <- brooklyn_na[row, 'street']
  }
  if (length(df[((is.na(df$ZIP_CODE)) | (df$ZIP_CODE == 0)) & (df$BOROUGH == 'Brooklyn') & (df$BBL == bbl), 'ZIP_CODE']) > 0) {
    df[((is.na(df$ZIP_CODE)) | (df$ZIP_CODE == 0)) & (df$BOROUGH == 'Brooklyn') & (df$BBL == bbl), 'ZIP_CODE'] <- brooklyn_na[row, 'zipcode']
  }
}

for (row in 1:nrow(queens_na)) {
  bbl <- queens_na[row, 'bbl']
  if (length(df[(is.na(df$STREET_NAME)) & (df$BOROUGH == 'Queens') & (df$BBL == bbl), 'STREET_NAME']) > 0) {
    df[(is.na(df$STREET_NAME)) & (df$BOROUGH == 'Queens') & (df$BBL == bbl), 'STREET_NAME'] <- queens_na[row, 'street']
  }
  if (length(df[((is.na(df$ZIP_CODE)) | (df$ZIP_CODE == 0)) & (df$BOROUGH == 'Queens') & (df$BBL == bbl), 'ZIP_CODE']) > 0) {
    df[((is.na(df$ZIP_CODE)) | (df$ZIP_CODE == 0)) & (df$BOROUGH == 'Queens') & (df$BBL == bbl), 'ZIP_CODE'] <- queens_na[row, 'zipcode']
  }
}

for (row in 1:nrow(staten_na)) {
  bbl <- staten_na[row, 'bbl']
  if (length(df[(is.na(df$STREET_NAME)) & (df$BOROUGH == 'Staten Island') & (df$BBL == bbl), 'STREET_NAME']) > 0) {
    df[(is.na(df$STREET_NAME)) & (df$BOROUGH == 'Staten Island') & (df$BBL == bbl), 'STREET_NAME'] <- staten_na[row, 'street']
  }
  if (length(df[((is.na(df$ZIP_CODE)) | (df$ZIP_CODE == 0)) & (df$BOROUGH == 'Staten Island') & (df$BBL == bbl), 'ZIP_CODE']) > 0) {
    df[((is.na(df$ZIP_CODE)) | (df$ZIP_CODE == 0)) & (df$BOROUGH == 'Staten Island') & (df$BBL == bbl), 'ZIP_CODE'] <- staten_na[row, 'zipcode']
  }
}

######################################################################################################

# Display the remaining number of rows with empty street name/zip code (compare)
sum(is.na(df$STREET_NAME))
sum(df$STREET_NAME == '', na.rm = TRUE)
sum(is.na(df$ZIP_CODE))
sum(df$ZIP_CODE == 0, na.rm = TRUE)
sum((df$STREET_NAME == '') & (is.na(df$ZIP_CODE)), na.rm = TRUE)
sum((df$STREET_NAME == '') & (df$ZIP_CODE == 0), na.rm = TRUE)

# From 754 rows that have empty street names to now 148 (imputed 606)
# From 7362 rows that have NA zipcode to now 6760 (imputed 602)
# Question is, what about the rest of the 148 street names? Check it out now


df[(df$BOROUGH == 'Manhattan') & (is.na(df$STREET_NAME)), c('STREET_NAME', 'BBL', 'BORO_CODE', 'BLOCK', 'LOT', 
                                                            'BOROUGH', 'ZIP_CODE', 'LATITUDE', 'LONGITUDE', 'JOB_PROGRESS')]
# For Manhattan, we identified 3 unique BBL (each has 1 row only) that has empty lat/long data
# Total of 3 rows, all only in job progress 1

df[(df$BOROUGH == 'Bronx') & (is.na(df$STREET_NAME)), c('STREET_NAME', 'BBL', 'BORO_CODE', 'BLOCK', 'LOT', 
                                                        'BOROUGH', 'ZIP_CODE', 'LATITUDE', 'LONGITUDE', 'JOB_PROGRESS')]
# For Bronx, we identified 8 unique BBL (each has 1 row only) that has empty lat/long data
# Total of 8 rows, all only in job progress 1

df[(df$BOROUGH == 'Brooklyn') & (is.na(df$STREET_NAME)), c('STREET_NAME', 'BBL', 'BORO_CODE', 'BLOCK', 'LOT', 
                                                           'BOROUGH', 'ZIP_CODE', 'LATITUDE', 'LONGITUDE', 'JOB_PROGRESS')]
# For Brooklyn, we identified 46 unique BBL (each has 1 row except one BBL has 2 rows) that has empty lat/long
# Total of 47 rows, all in job progress 1 except one in job progress 1 + 2

df[(df$BOROUGH == 'Queens') & (is.na(df$STREET_NAME)), c('STREET_NAME', 'BBL', 'BORO_CODE', 'BLOCK', 'LOT', 
                                                         'BOROUGH', 'ZIP_CODE', 'LATITUDE', 'LONGITUDE', 'JOB_PROGRESS')]
# For Queens, we identified 76 unique BBL (each has 1 row only) that has empty lat/long data
# Total of 76 rows, all only in job progress 1

df[(df$BOROUGH == 'Staten Island') & (is.na(df$STREET_NAME)), c('STREET_NAME', 'BBL', 'BORO_CODE', 'BLOCK', 'LOT', 
                                                                'BOROUGH', 'ZIP_CODE', 'LATITUDE', 'LONGITUDE', 'JOB_PROGRESS')]
# For Queens, we identified 13 unique BBL (each has 1 row only) that has empty lat/long data
# Total of 13 rows, all only in job progress 1


# Conclusion: We can drop these 148 rows, they are not so meaningful
# They only accumulate up to 0.016% of half of the entire dataset, not even 0.1%, safe to drop them!


# But the data still have something wrong with it, especially with values of ***********

sort(unique(df[(df$STREET_NAME == "********************"), 'BBL']))
sort(unique(df[is.na(df$ZIP_CODE), 'BBL']))
sort(unique(df[(df$LATITUDE == 0 | is.na(df$LATITUDE)), 'BBL']))


#############################################################################################################
#############################################################################################################
#############################################################################################################

# Create another spare copy of data frame for resetting

df_copy2 <- data.frame(df)

# Reset data frame
df <- data.frame(df_copy2)

#############################################################################################################
#############################################################################################################
#############################################################################################################

# This is also the similar approach of using bbl as reference
# We use bbl to identify counterparts that has valid street names, zipcodes and lat/long values
# This needs to be done because previously we did not address NA zip codes properly
# I forgot to include the zipcode condition for imputation, so we missed out a portion of them
# Once we identify them, we can impute accordingly.

for (bbl in sort(unique(df[(df$STREET_NAME == "********************"), 'BBL']))) {
  condition_impute <- (df$BBL == bbl) & (df$STREET_NAME == "********************")
  condition_find <- (df$BBL == bbl) & (df$STREET_NAME != "********************") & (!is.na(df$STREET_NAME))
  df[condition_impute, 'STREET_NAME'] <- df[condition_find, 'STREET_NAME'][2]
}

for (bbl in sort(unique(df[is.na(df$ZIP_CODE), 'BBL']))) {
  if (length(df[(df$BBL == bbl) & (!is.na(df$ZIP_CODE)), 'ZIP_CODE']) > 0) {
    df[(df$BBL == bbl) & (is.na(df$ZIP_CODE)), 'ZIP_CODE'] <- df[(df$BBL == bbl) & (!is.na(df$ZIP_CODE)), 'ZIP_CODE'][1]
  }
}

for (bbl in sort(unique(df[(df$LATITUDE == 0 | is.na(df$LATITUDE)), 'BBL']))) {
  if (length(df[(df$BBL == bbl) & (!is.na(df$LATITUDE) & df$LATITUDE != 0), 'LATITUDE']) > 0) {
    df[(df$BBL == bbl) & (is.na(df$LATITUDE) | (df$LATITUDE == 0)), 'LATITUDE'] <- df[df$BBL == bbl & (!is.na(df$LATITUDE) & (df$LATITUDE != 0)), 'LATITUDE'][1]
  }
  if (length(df[(df$BBL == bbl) & (!is.na(df$LONGITUDE) & df$LONGITUDE != 0), 'LONGITUDE']) > 0) {
    df[(df$BBL == bbl) & (is.na(df$LONGITUDE) | (df$LONGITUDE == 0)), 'LONGITUDE'] <- df[df$BBL == bbl & (!is.na(df$LONGITUDE) & (df$LONGITUDE != 0)), 'LONGITUDE'][1]
  }
}

# Check the results
sum(df$STREET_NAME == "********************", na.rm = TRUE)  # It's 0 now

#############################################################################################################
#############################################################################################################
#############################################################################################################

# Create another spare copy of data frame for resetting

df_copy3 <- data.frame(df)

# Reset data frame
df <- data.frame(df_copy3)

#############################################################################################################
#############################################################################################################
#############################################################################################################

# Check for street names now
sum(is.na(df$STREET_NAME))  # Only left with 148 rows

##########################################################################################################

###########################################################
### What about zip codes? Let's do it from here onwards ###
###########################################################

# Specifically examine zip codes
sum(is.na(df$ZIP_CODE))   # 3482 rows 
sum(df$ZIP_CODE == 0, na.rm = TRUE)   # 2277 rows

# Imputation of rows that have street names but have ZERO VALUE for zip codes
# NOTE: A small amount of rows can't be imputed because we can't find same street name rows with a zip code

for (bbl in sort(unique(df[!is.na(df$STREET_NAME) & ((df$ZIP_CODE == 0) | (is.na(df$ZIP_CODE))), 'BBL']))) {
  borocode <- df[df$BBL == bbl, 'BORO_CODE'][1]
  block <- df[df$BBL == bbl, 'BLOCK'][1]
  street <- df[df$BBL == bbl, 'STREET_NAME'][1]
  condition <- (df$BORO_CODE == borocode) & (df$BLOCK == block) & (df$STREET_NAME == street) & (df$ZIP_CODE != 0) & (!is.na(df$ZIP_CODE))
  if (length(df[condition, 'ZIP_CODE']) > 0) {
    df[df$BBL == bbl, 'ZIP_CODE'] <- df[condition, 'ZIP_CODE'][1]
  }
}

# Take a look at the NA and zero rows again
sum(is.na(df$ZIP_CODE))   # 1476 rows
sum(df$ZIP_CODE == 0, na.rm = TRUE)   # 550 rows


sort(unique(df[!is.na(df$STREET_NAME) & ((df$ZIP_CODE == 0) | (is.na(df$ZIP_CODE))), 'BBL']))

# 136 unique BBL that has lat/long data
sort(unique(df[(df$INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")) & !is.na(df$STREET_NAME) & 
                 ((df$LATITUDE != 0) & (!is.na(df$LATITUDE))) & ((df$ZIP_CODE == 0) | (is.na(df$ZIP_CODE))), 'BBL']))

# 330 unique BBL that don't have lat/long data
sort(unique(df[(df$INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")) & !is.na(df$STREET_NAME) & 
                 ((df$LATITUDE == 0) | (is.na(df$LATITUDE))) & ((df$ZIP_CODE == 0) | (is.na(df$ZIP_CODE))), 'BBL']))


#############################################################################################################

# Create another spare copy of data frame for resetting

df_copy4 <- data.frame(df)

# Reset data frame
df <- data.frame(df_copy4)

#############################################################################################################

sum(is.na(df$ZIP_CODE))   # 1476 rows
sum(df$ZIP_CODE == 0, na.rm = TRUE)   # 550 rows

# Testing the Block minus 1 filter method see how many rows are imputed
# 1st attempt to impute zip codes along with some lat/long

time_filter <- (df$INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York"))
for (bbl in sort(unique(df[time_filter & !is.na(df$STREET_NAME) & ((df$LATITUDE == 0) | (is.na(df$LATITUDE))) & ((df$ZIP_CODE == 0) | (is.na(df$ZIP_CODE))), 'BBL']))) {
  borocode <- df[df$BBL == bbl, 'BORO_CODE'][1]
  block <- df[df$BBL == bbl, 'BLOCK'][1] - 1
  street <- df[df$BBL == bbl, 'STREET_NAME'][1]
  condition <- (df$BORO_CODE == borocode) & (df$BLOCK == block) & (df$STREET_NAME == street) & (df$ZIP_CODE != 0) & (!is.na(df$ZIP_CODE))
  if (length(df[condition, 'ZIP_CODE']) > 0) {
    df[df$BBL == bbl, 'ZIP_CODE'] <- df[condition, 'ZIP_CODE'][1]
  }
  if (length(df[condition, 'LATITUDE']) > 0) {
    df[df$BBL == bbl, 'LATITUDE'] <- df[condition, 'LATITUDE'][1]
  }
  if (length(df[condition, 'LONGITUDE']) > 0) {
    df[df$BBL == bbl, 'LONGITUDE'] <- df[condition, 'LONGITUDE'][1]
  }
}


# Take a look at the NA and zero rows again
sum(is.na(df$ZIP_CODE))   # 1357 rows
sum(df$ZIP_CODE == 0, na.rm = TRUE)   # 550 rows

# 14 BBL has been imputed successfully, still have 316 BBL
sort(unique(df[(df$INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")) & !is.na(df$STREET_NAME) & 
                 ((df$LATITUDE == 0) | (is.na(df$LATITUDE))) & ((df$ZIP_CODE == 0) | (is.na(df$ZIP_CODE))), 'BBL']))



(startsWith(tolower(df$STREET_NAME), substr(tolower(street), 1, 3)))

#############################################################################################################

# Create another spare copy of data frame for resetting

df_copy5 <- data.frame(df)

# Reset data frame
df <- data.frame(df_copy5)

#############################################################################################################


# 2nd attempt to impute zip codes with some lat/long as well
# Our condition is that if the 1st element's lat/long is zero, we move on to 2nd element
# And we move to 3rd if 2nd element is also zero, then we stop

time_filter <- (df$INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York"))
for (bbl in sort(unique(df[time_filter & !is.na(df$STREET_NAME) & ((df$LATITUDE == 0) | (is.na(df$LATITUDE))) & ((df$ZIP_CODE == 0) | (is.na(df$ZIP_CODE))), 'BBL']))) {
  borocode <- df[df$BBL == bbl, 'BORO_CODE'][1]
  block <- df[df$BBL == bbl, 'BLOCK'][1]
  street <- df[df$BBL == bbl, 'STREET_NAME'][1]
  condition1 <- (df$BORO_CODE == borocode) & (df$BLOCK == block) & (startsWith(tolower(df$STREET_NAME), substr(tolower(street), 1, 3))) & (df$ZIP_CODE != 0) & (!is.na(df$ZIP_CODE))
  condition2 <- (df$BORO_CODE == borocode) & (df$BLOCK == block) & (startsWith(tolower(df$STREET_NAME), substr(tolower(street), 1, 3))) & (df$LATITUDE != 0) & (!is.na(df$LATITUDE))
  condition3 <- (df$BORO_CODE == borocode) & (df$BLOCK == block) & (startsWith(tolower(df$STREET_NAME), substr(tolower(street), 1, 3))) & (df$LONGITUDE != 0) & (!is.na(df$LONGITUDE))
  
  if (length(df[condition1, 'ZIP_CODE']) > 0) {
    df[df$BBL == bbl, 'ZIP_CODE'] <- df[condition1, 'ZIP_CODE'][1]
  }
  
  if (length(df[condition2, 'LATITUDE']) > 0 & ((df[condition2, 'LATITUDE'][1] != 0) & (!is.na(df[condition2, 'LATITUDE'][1])))) {
    df[df$BBL == bbl, 'LATITUDE'] <- df[condition2, 'LATITUDE'][1]
  } else if (length(df[condition2, 'LATITUDE']) > 0 & ((df[condition2, 'LATITUDE'][2] != 0) & (!is.na(df[condition2, 'LATITUDE'][2])))) {
    df[df$BBL == bbl, 'LATITUDE'] <- df[condition2, 'LATITUDE'][2]
  } else if (length(df[condition2, 'LATITUDE']) > 0 & ((df[condition2, 'LATITUDE'][3] != 0) & (!is.na(df[condition2, 'LATITUDE'][3])))) {
    df[df$BBL == bbl, 'LATITUDE'] <- df[condition2, 'LATITUDE'][3]
  }
  
  if (length(df[condition3, 'LONGITUDE']) > 0 & ((df[condition3, 'LONGITUDE'][1] != 0) & (!is.na(df[condition3, 'LONGITUDE'][1])))) {
    df[df$BBL == bbl, 'LONGITUDE'] <- df[condition3, 'LONGITUDE'][1]
  } else if (length(df[condition3, 'LONGITUDE']) > 0 & ((df[condition3, 'LONGITUDE'][2] != 0) & (!is.na(df[condition3, 'LONGITUDE'][2])))) {
    df[df$BBL == bbl, 'LONGITUDE'] <- df[condition3, 'LONGITUDE'][2]
  } else if (length(df[condition3, 'LONGITUDE']) > 0 & ((df[condition3, 'LONGITUDE'][3] != 0) & (!is.na(df[condition3, 'LONGITUDE'][3])))) {
    df[df$BBL == bbl, 'LONGITUDE'] <- df[condition3, 'LONGITUDE'][3]
  }
}


# Take a look at the NA and zero rows again
sum(is.na(df$ZIP_CODE))   # 1086 rows
sum(df$ZIP_CODE == 0, na.rm = TRUE)   # 550 rows

# 145 BBL has been imputed successfully, still have 171 BBL (originally 316 BBL)
sort(unique(df[(df$INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")) & !is.na(df$STREET_NAME) & 
                 ((df$LATITUDE == 0) | (is.na(df$LATITUDE))) & ((df$ZIP_CODE == 0) | (is.na(df$ZIP_CODE))), 'BBL']))



##############################################################################################
##############################################################################################

# Export the cleaned part 1 dataframe to a csv file for conventional purpose in case we have to reset:
# NOTE: the df is from THIS POINT ONWARDS (after 2nd attempt imputation of zipcode and lat/long)
# NOTE: remember to run some codes above for dfs if you think it's necessary

write.csv(df, "D:/Documents/UM Master of Data Science/WQD7001 Principles of Data Science/Group project/Rodent_Inspection_clean01.csv", 
          row.names = FALSE)

# Load clean01 data and convert some data types
df <- read.csv("D:/Documents/UM Master of Data Science/WQD7001 Principles of Data Science/Group project/Rodent_Inspection_clean01.csv", 
               header = TRUE, sep = ',', stringsAsFactors = FALSE)

df$INSPECTION_TYPE <- as.factor(df$INSPECTION_TYPE)
df$BOROUGH <- as.factor(df$BOROUGH)
df$INSPECTION_DATE <- strptime(df$INSPECTION_DATE, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
df$INSPECTION_DATE <- as.POSIXct(df$INSPECTION_DATE)
df$RESULT <- as.factor(df$RESULT)
df$APPROVED_DATE <- strptime(df$APPROVED_DATE, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
df$APPROVED_DATE <- as.POSIXct(df$APPROVED_DATE)

##############################################################################################
##############################################################################################

# Take a look at the NA and zero rows again
sum(is.na(df$ZIP_CODE))   # 1086 rows
sum(df$ZIP_CODE == 0, na.rm = TRUE)   # 550 rows

####################################################################################################

# Now let's work on the manual work for the 136 BBL left

# 142 unique BBL that has lat/long data, oops it's more than just 136!
sort(unique(df[(df$INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")) & !is.na(df$STREET_NAME) & 
                 ((df$LATITUDE != 0) & (!is.na(df$LATITUDE))) & ((df$ZIP_CODE == 0) | (is.na(df$ZIP_CODE))), 'BBL']))


# Note: BBL 1011710150 has 2 street names, we need to change it later.
# Also note: Lat/long data from bronx onwards (32nd BBL, cross bronx) are kinda inaccurate, altho BBL shows correct street
# For those cases, the zip code follows the street instead of lat/long address
# Note: BBL 2032310001 seems to be invalid even though the BBL exists, we'll remove it, zipcode input is 99999
bbl_latlong <- sort(unique(df[(df$INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")) & !is.na(df$STREET_NAME) & 
                                ((df$LATITUDE != 0) & (!is.na(df$LATITUDE))) & ((df$ZIP_CODE == 0) | (is.na(df$ZIP_CODE))), 'BBL']))
zc_latlong <- c(10004, 10038, 10014, 10016, 10036, 10069, 10069, 10069, 10069, 10069, 10069, 10069, 10069, 10069, 10069, 
                10069, 10069, 10069, 10036, 10024, 10021, 10035, 10029, 10037, 10037, 10033, 10451, 10451, 10451, 10451, 
                10452, 10472, 10472, 10474, 10474, 10474, 10474, 10459, 10459, 10459, 10474, 10453, 10453, 10453, 10453, 
                10453, 10453, 10453, 99999, 10453, 10453, 10468, 10468, 10468, 10462, 10461, 10472, 10472, 10472, 10472, 
                10472, 10460, 10460, 10467, 10475, 10475, 10465, 10465, 10465, 10464, 10464, 10464, 10453, 11201, 11201, 
                11232, 11232, 11232, 11232, 11216, 11213, 11221, 11221, 11221, 11217, 11249, 11249, 11207, 11220, 11235, 
                11103, 11377, 11372, 11377, 11368, 11368, 11368, 11368, 11368, 11373, 11373, 11373, 11373, 11379, 11385, 
                11375, 11356, 11357, 11357, 11357, 11365, 11418, 11433, 10301, 10304, 10304, 10314, 10314, 10314, 10314, 
                10314, 10314, 10305, 10305, 10304, 10305, 10305, 10306, 10305, 10305, 10305, 10308, 10308, 10308, 10306, 
                10308, 10312, 10312, 10312, 10309, 10309, 10312)
impute_zc <- data.frame('bbl' = bbl_latlong, 'zipcode' = zc_latlong, stringsAsFactors = FALSE)



# I suspected that there are some wrong info about street names
# Thus, I want to create another dataframe to address this, just in case I really want to change

invalid_bbl <- c(1000110000, 1006440005, 1011710150, 1012540001, 1014740060, 2043420042, 
                 2099990001, 3009030077, 5035550001, 5057400035)
correct_st <- c("STONE STREET", "GANSEVOORT STREET", "JOE DIMAGGIO HIGHWAY", "RIVERSIDE DRIVE", "F D R DRIVE", "WHITE PLAINS ROAD", 
                "ECHO PLACE", "39 STREET", "JEFFERSON AVENUE", "ASPEN KNOLLS WAY")
replace_st <- data.frame('bbl' = invalid_bbl, 'street' = correct_st, stringsAsFactors = FALSE)

######################################################################################################

# Create another spare copy of data frame for resetting

df_copy6 <- data.frame(df)

# Reset data frame
df <- data.frame(df_copy6)

######################################################################################################

for (row in 1:nrow(impute_zc)) {
  bbl <- impute_zc[row, 'bbl']
  df[(df$BBL == bbl), 'ZIP_CODE'] <- impute_zc[row, 'zipcode']
}

for (row in 1:nrow(replace_st)) {
  bbl <- replace_st[row, 'bbl']
  df[(df$BBL == bbl), 'STREET_NAME'] <- replace_st[row, 'street']
}


# Now check the unique BBL count (before this it's 142)
sort(unique(df[(df$INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")) & !is.na(df$STREET_NAME) & 
                 ((df$LATITUDE != 0) & (!is.na(df$LATITUDE))) & ((df$ZIP_CODE == 0) | (is.na(df$ZIP_CODE))), 'BBL']))

# Take a look at the NA & zero rows now (after the 2 for loops)
# Before this, NA count is 1086, zero count is 550
sum(is.na(df$ZIP_CODE))   # 976 rows
sum(df$ZIP_CODE == 0, na.rm = TRUE)   # 33 rows

# View the remaining unimputed rows
head(df[is.na(df$ZIP_CODE), c('STREET_NAME', 'BBL', 'BORO_CODE', 'BLOCK', 'LOT', 'BOROUGH', 'ZIP_CODE', 'LATITUDE', 'LONGITUDE', 'INSPECTION_DATE')])

# The count of NA zipcode rows before 2015-1-1 (636 rows)
sum((is.na(df$ZIP_CODE)) & (df$INSPECTION_DATE < as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")))

# The count of NA zipcode rows after 2015-1-1 (340 rows)
sum((is.na(df$ZIP_CODE)) & (df$INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")))

# The count of both NA street and zipcode rows (18 rows)
sum(is.na(df$STREET_NAME) & ((df$ZIP_CODE == 0) | (is.na(df$ZIP_CODE))))

# The count of NA street and non NA/zero zipcode rows (130 rows)
sum(is.na(df$STREET_NAME) & !((df$ZIP_CODE == 0) | (is.na(df$ZIP_CODE))))

# The count of remaining rows with valid street names and NA/zero zipcode (322 rows)
nrow(df[(df$INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")) & !is.na(df$STREET_NAME) & 
          ((df$LATITUDE == 0) | (is.na(df$LATITUDE))) & ((df$ZIP_CODE == 0) | (is.na(df$ZIP_CODE))),])



# Double check on the 148 NA street name rows to examine the changes on lat/long or zipcode
df[(df$BOROUGH == 'Manhattan') & (is.na(df$STREET_NAME)), c('STREET_NAME', 'BBL', 'BORO_CODE', 'BLOCK', 'LOT', 
                                                            'BOROUGH', 'ZIP_CODE', 'LATITUDE', 'LONGITUDE', 'JOB_PROGRESS')]
df[(df$BOROUGH == 'Bronx') & (is.na(df$STREET_NAME)), c('STREET_NAME', 'BBL', 'BORO_CODE', 'BLOCK', 'LOT', 
                                                        'BOROUGH', 'ZIP_CODE', 'LATITUDE', 'LONGITUDE', 'JOB_PROGRESS')]
df[(df$BOROUGH == 'Brooklyn') & (is.na(df$STREET_NAME)), c('STREET_NAME', 'BBL', 'BORO_CODE', 'BLOCK', 'LOT', 
                                                           'BOROUGH', 'ZIP_CODE', 'LATITUDE', 'LONGITUDE', 'JOB_PROGRESS')]
df[(df$BOROUGH == 'Queens') & (is.na(df$STREET_NAME)), c('STREET_NAME', 'BBL', 'BORO_CODE', 'BLOCK', 'LOT', 
                                                         'BOROUGH', 'ZIP_CODE', 'LATITUDE', 'LONGITUDE', 'JOB_PROGRESS')]
df[(df$BOROUGH == 'Staten Island') & (is.na(df$STREET_NAME)), c('STREET_NAME', 'BBL', 'BORO_CODE', 'BLOCK', 'LOT', 
                                                                'BOROUGH', 'ZIP_CODE', 'LATITUDE', 'LONGITUDE', 'JOB_PROGRESS')]


######################################################################################################

# Create another spare copy of data frame for resetting

df_copy7 <- data.frame(df)

# Reset data frame
df <- data.frame(df_copy7)

######################################################################################################


###
#
# Conclusion: The rest of the 171 BBL will be dropped, we had done our best to impute for now
# A total of 322 rows (after 2015-1-1) and 18 rows (both NA for zipcode & street) will be dropped here
# 
# Besides, the 148 rows with NA street names will be dropped too, since their lat/long is NA
#
# Total up, we have 1139 rows to drop here 
# 976 (Na zipcode) + 33 (zero zipcode) + 130 (Na street) = 1139 rows
# Note: 18 rows that are both NA for zipcode + street are removed while removing zipcode rows
#
###


###
#####
#######
#########

# Remove the rows using dplyr filter
df <- df %>% 
  filter((ZIP_CODE != 0) & (!is.na(ZIP_CODE)) & (!is.na(STREET_NAME)))

# Check empty value counts
sum(is.na(df$ZIP_CODE))   
sum(df$ZIP_CODE == 0, na.rm = TRUE)   
sum(is.na(df$STREET_NAME))

# Check the new dimensions (1898048 rows)
dim(df)

#########
#######
#####
###


#############################################################################################################


#############################################################################################################

# Export the cleaned part 2 dataframe to a csv file for conventional purpose in case we have to reset:
# NOTE: the df is from THIS POINT ONWARDS (after removal of NA/0 zipcode and street)
# NOTE: remember to run some codes above for dfs if you think it's necessary

write.csv(df, "D:/Documents/UM Master of Data Science/WQD7001 Principles of Data Science/Group project/Rodent_Inspection_clean02.csv", 
          row.names = FALSE)

# Load clean01 data and convert some data types
df <- read.csv("D:/Documents/UM Master of Data Science/WQD7001 Principles of Data Science/Group project/Rodent_Inspection_clean02.csv", 
               header = TRUE, sep = ',', stringsAsFactors = FALSE)

df$INSPECTION_TYPE <- as.factor(df$INSPECTION_TYPE)
df$BOROUGH <- as.factor(df$BOROUGH)
df$INSPECTION_DATE <- strptime(df$INSPECTION_DATE, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
df$INSPECTION_DATE <- as.POSIXct(df$INSPECTION_DATE)
df$RESULT <- as.factor(df$RESULT)
df$APPROVED_DATE <- strptime(df$APPROVED_DATE, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
df$APPROVED_DATE <- as.POSIXct(df$APPROVED_DATE)


sum(is.na(df$ZIP_CODE))   
sum(df$ZIP_CODE == 0, na.rm = TRUE)   
sum(is.na(df$STREET_NAME))

dim(df)

#############################################################################################################




## (14) Latitude (15) Longitude
# There are 3084 Na values for both lat and long data initially while exploring data
# It seems like after we did some processing above that imputes values using counterparts, the count reduced!

# Display the remaing number of rows with NA and zero lat and long data
nrow(df[is.na(df$LATITUDE),])  # 90 rows
nrow(df[is.na(df$LONGITUDE),])  # 90 rows
sum(df$LATITUDE == 0, na.rm = TRUE)  # 2100 rows
sum(df$LONGITUDE == 0, na.rm = TRUE)  # 2100 rows

# How many NA lat/long rows are after 2015-1-1 (28 rows)
nrow(df[is.na(df$LATITUDE) & (df$INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")),])

# How many NA lat/long rows are before 2015-1-1 (62 rows)
nrow(df[is.na(df$LATITUDE) & (df$INSPECTION_DATE < as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")),])


# Check the rows that have these NA values
df[(is.na(df$LATITUDE)) & (is.na(df$LONGITUDE)), c('STREET_NAME', 'BBL', 'BORO_CODE', 'BLOCK', 'LOT', 
                                                   'BOROUGH', 'ZIP_CODE', 'LATITUDE', 'JOB_PROGRESS', 'INSPECTION_DATE')]
df[(df$LATITUDE == 0) & (df$LONGITUDE == 0), c('STREET_NAME', 'BBL', 'BORO_CODE', 'BLOCK', 'LOT', 
                                               'BOROUGH', 'ZIP_CODE', 'LATITUDE', 'JOB_PROGRESS', 'INSPECTION_DATE')]
df[(df$LATITUDE == 0) & (df$LONGITUDE == 0), 'BBL'][1:10]


# Check the row count associated with the identified BBL
nrow(df[df$BBL %in% sort(unique(df[is.na(df$LATITUDE) & (df$INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")), 'BBL'])),])
nrow(df[df$BBL %in% sort(unique(df[is.na(df$LATITUDE) & (df$INSPECTION_DATE < as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")), 'BBL'])),])


# It looks like all the BBL above haave no counterparts that are complete
# Means that they are the ones listed up there, 28 rows and 62 rows, no other than these rows
# This means that if we remove these rows, we won't lose any partial information for any specific BBL 
# since they're all removed here all in once.


# How many zero lat/long rows are after 2015-1-1 (849 rows)
nrow(df[(df$LATITUDE == 0 & !is.na(df$LATITUDE)) & (df$INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")),])

# How many zero lat/long rows are before 2015-1-1 (1251 rows)
nrow(df[(df$LATITUDE == 0 & !is.na(df$LATITUDE)) & (df$INSPECTION_DATE < as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")),])


# Check the row count associated with the identified BBL
nrow(df[df$BBL %in% sort(unique(df[(df$LATITUDE == 0 & !is.na(df$LATITUDE)) & (df$INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")), 'BBL'])),])
nrow(df[df$BBL %in% sort(unique(df[(df$LATITUDE == 0 & !is.na(df$LATITUDE)) & (df$INSPECTION_DATE < as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")), 'BBL'])),])

# Check unique BBL with NA/zero lat/long values
sort(unique(df[is.na(df$LATITUDE), 'BBL']))  # 82 BBL
sort(unique(df[(df$LATITUDE == 0 & !is.na(df$LATITUDE)), 'BBL']))  # 595 BBL


# However, for zero lat/long rows, several of them have complete counterpart rows!
# While it says 849 rows for after 2015-1-1, there are a total of 1288 rows associated with these BBL
# extra: 439 rows
# While it says 1251 rows for before 2015-1-1, there are a total of 1840 rows associated with these BBL
# extra: 589 rows
#
# After investigation, we found that the extra rows are actually in separate date ranges of 2015-1-1
# Some BBL have dates ranging from 2011 to 2018, so when we display all rows for a BBL, 
# it outputs rows more than what we identified for the specified date ranges.
# We also verified that all the associated rows are all having zero lat/long values
#
# This means that, we can remove all these rows (2100 rows) without losing partial data for existing BBL
#


# We need to determine if these rows are associated with any related complete rows 1st!!
# After verifying this, only we can use dplyr to filter NA entries

######################################################################################################

# Create another spare copy of data frame for resetting

df_copy8 <- data.frame(df)

# Reset data frame
df <- data.frame(df_copy8)

######################################################################################################

###
#
# Conclusion: 595 + 82 BBL = 677 BBL will be dropped, it is 0.1% of total data and we have did what we can above
#
# Besides, some of these rows have invalid street names, it's reasonable to drop them
#
# Total up, we have 2190 rows to drop here 
# 90 (Na lat/long) + 2100 (zero lat/long) = 2190 rows
# 
# We should be left with 1895858 rows after removing the rows
#
###


###
#####
#######
#########

# Remove the rows using dplyr filter
df <- df %>% 
  filter((df$LATITUDE != 0) & (!is.na(LATITUDE)))

# Check for NA/zero rows again and see after removal
nrow(df[is.na(df$LATITUDE),])  
nrow(df[is.na(df$LONGITUDE),])  
sum(df$LATITUDE == 0, na.rm = TRUE)  
sum(df$LONGITUDE == 0, na.rm = TRUE)

# Check the new dimensions (1898048 rows)
dim(df)

#########
#######
#####
###



## (18) RESULT
# There are 5 rows with empty result entries.
# These 5 rows all share similar traits, that is all are at job progress 2 and compliance type.
# This can be some kind of info for us to do comparison and imputation.
# Take note that result also can correspond to inspection type, we should check that out.

# Display the number of rows with empty entries
nrow(df[is.na(df$RESULT),])
sum(df$RESULT == '')

# Check the rows that have these NA values
df[df$RESULT == '', c('STREET_NAME', 'BBL', 'BORO_CODE', 'BLOCK', 'LOT', 
                      'BOROUGH', 'ZIP_CODE', 'LATITUDE', 'LONGITUDE', 'JOB_PROGRESS', 'INSPECTION_DATE')]
df[df$RESULT == '', 'BBL']

df[(df$RESULT == '') & (df$INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")), 
   c('STREET_NAME', 'BBL', 'BORO_CODE', 'BLOCK', 'LOT', 'BOROUGH', 'ZIP_CODE', 'LATITUDE', 'LONGITUDE', 'JOB_PROGRESS', 'INSPECTION_DATE')]

(df$INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York"))

# After investigation, we will only impute 3 BBL that has missing result values.
# 1019450011, 1002747501, 1022410024     2023310018 2026910047
# Row names: "19834"  "115108" "610513"
# We will use similarity comparison to determine what to impute.
# For example, if final inspection for a particular job id ends at job progress number 2 with a compliance visit, 
# Then that result will probably be a 'Passed' result since there's no more visits.
# For example, if the job id has 7 visits, with the final 4 visits as bait application visit, 
# then the visits prior to the bait application is very likely to be 'Rat Activity' result.


######################################################################################################

# Create another spare copy of data frame for resetting

df_copy9 <- data.frame(df)

# Reset data frame
df <- data.frame(df_copy9)

######################################################################################################


# Replace empty results with for loop over the 3 rows
for (row in row.names(df[((df$RESULT == '') & (df$INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York"))),])) {
  job_id <- df[row, 'JOB_ID']
  bbl <- df[row, 'BBL']
  if (length(df[(df$JOB_ID == job_id) & (df$BBL == bbl), 'JOB_PROGRESS']) == 2) {
    df[row, 'RESULT'] <- 'Passed'
  } else if (length(df[(df$JOB_ID == job_id) & (df$BBL == bbl), 'JOB_PROGRESS']) > 2) {
    df[row, 'RESULT'] <- 'Rat Activity'
  }
}


# Check for NA rows again and see
sum(df$RESULT == '')  # 2 rows left

# View the df dimension
dim(df)

# View the levels of Result factor column, see if we need to change it now
levels(df$RESULT)  # Still consist of '' level



# Next step is to remove the 2 rows as well as the rows associated with the job id of the 2 rows
# Since both rows are before 2015-1-1, so it is safe to remove that particular job id rows
df[((df$RESULT == '') & (df$INSPECTION_DATE < as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York"))), 'JOB_ID']

# We need to determine if these rows are associated with any related complete rows 1st!!
# After verifying, only we can use dplyr to filter '' entries, here we have 4 rows to filter and remove
job_to_remove <- df[((df$RESULT == '') & (df$INSPECTION_DATE < as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York"))), 'JOB_ID']
df <- df %>% 
  filter((JOB_ID != job_to_remove[1]) & (JOB_ID != job_to_remove[2]))


# Check for NA rows again and see
sum(df$RESULT == '')

# View the df dimension
dim(df)

# View the levels of Result factor column, see if we need to change it now
levels(df$RESULT)

# Let's now change the levels to exclude the unused empty string level:
df$RESULT <- droplevels(df$RESULT)
levels(df$RESULT)



## Double check the empty entries again for all columns
colSums(is.na(df))
colSums(df == '')




## (17) Inspection Date
# There are 17 NA entries for this date, let's take a look and deal with it

# Check the rows that have these NA values
df[is.na(df$INSPECTION_DATE), c('STREET_NAME', 'BBL', 'BORO_CODE', 'BLOCK', 'LOT', 
                                'BOROUGH', 'ZIP_CODE', 'LATITUDE', 'JOB_ID', 'JOB_PROGRESS', 'INSPECTION_DATE')]


# JOB_ID: 6301484, 6809582, 7170826, 7482483 (after 2015-1-1)
# The rest of the 13 Job Ids can be removed since they are single visit jobs and before 2015-1-1

df[df$JOB_ID == 6809582, c('STREET_NAME', 'BBL', 'BORO_CODE', 'BLOCK', 'LOT', 'BOROUGH', 'ZIP_CODE', 'LATITUDE', 'LONGITUDE', 'JOB_PROGRESS', 'INSPECTION_DATE')]


## For job id 6809582
# Use group_by to return job id with only 27 job progresses and from Bronx
df %>%
  filter(BORO_CODE == 2) %>%
  group_by(JOB_ID) %>%
  summarise(job_count = n()) %>%
  filter(job_count == 27)

df %>%
  filter(BORO_CODE == 2 & STREET_NAME == 'ANDERSON AVENUE') %>%
  group_by(JOB_ID) %>%
  summarise(job_count = n()) %>%
  filter(job_count >= 15 & job_count <= 27)

# This is just one way of checking for similar rows for imputation
# However, for our case, we found better methods to impute inspection date using approved date
# We check the intervals for each rows of that particular job to compare and estimate time intervals
# Typically, it takes 3 to 4 days from inspection to approval, so we'll assume 4 days for imputation
#
# Sad to say, there is something wrong with the approved date for the NA row for this id
# We can't use it to impute because it won't make sense for the imputed inspection date
# Thus, we just take the average interval from similar entries for inspection date between job visits
# The rough average interval is 11 days, use this for imputation


## For job id 6301484

# Use group_by to return job id with only 4 job progresses and from Manhattan
df %>%
  filter(BORO_CODE == 1 & STREET_NAME == 'WEST 107 STREET') %>%
  group_by(JOB_ID) %>%
  summarise(job_count = n()) %>%
  filter(job_count == 4)

df %>%
  filter(BORO_CODE == 1 & STREET_NAME == 'WEST 107 STREET' & (df$INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")) 
         & (df$INSPECTION_DATE < as.POSIXct("2019-01-01 00:00:00", tz = "America/New_York"))) %>%
  group_by(JOB_ID) %>%
  summarise(job_count = n()) %>%
  filter(job_count == 4)

# We managed to identify considerably similar job ids for imputation
# The job ids are 6341313 (4 jobs) and 6402057 (4 jobs)
# Both of the job ids are from similar date range of 2017 to 2018, from West 107 St & same zip codes
# The mean time difference are 2.5 days = 3 days from inspection to approval
# But since we have 2 more job ids to impute, we use 4 days as imputation standard


######################################################################################################

# Create another spare copy of data frame for resetting

df_copy10 <- data.frame(df)

# Reset data frame
df <- data.frame(df_copy10)

######################################################################################################

# Start the imputation for loop over the 4 job ids
for (job_id in c(6301484, 6809582, 7170826, 7482483)) {
  if (job_id == 6809582) {
    datetime_found <- df[(df$JOB_ID == job_id) & (df$JOB_PROGRESS == 2), 'INSPECTION_DATE'] - as.difftime(11, units = 'days')
    df[(df$JOB_ID == job_id) & (is.na(df$INSPECTION_DATE)), 'INSPECTION_DATE'] <- datetime_found
  } else {
    datetime_found <- df[(df$JOB_ID == job_id) & (is.na(df$INSPECTION_DATE)), 'APPROVED_DATE'] - as.difftime(4, units = 'days')
    df[(df$JOB_ID == job_id) & (is.na(df$INSPECTION_DATE)), 'INSPECTION_DATE'] <- datetime_found
  }
}

# Check for NA rows
sum(is.na(df$INSPECTION_DATE))


# Drop the remaining 13 rows using dplyr filter, total is 20 rows to drop
# This is because we also remove the associated rows of the same job ids
df[is.na(df$INSPECTION_DATE), 'JOB_ID']
df <- df %>%
  filter(!(JOB_ID %in% df[is.na(df$INSPECTION_DATE), 'JOB_ID']))

# Check for NA rows again and see
sum(is.na(df$INSPECTION_DATE))

# Check dimension (20 rows from 13 job ids are dropped)
dim(df)  # 1895834 rows

# Check NA for all columns
colSums(is.na(df))


#############################################################################################################

# Export the cleaned part 3 dataframe to a csv file for conventional purpose in case we have to reset:
# NOTE: the df is from THIS POINT ONWARDS
# NOTE: remember to run some codes above for dfs if you think it's necessary

write.csv(df, "D:/Documents/UM Master of Data Science/WQD7001 Principles of Data Science/Group project/Rodent_Inspection_clean03.csv", 
          row.names = FALSE)

# Load clean01 data and convert some data types
df <- read.csv("D:/Documents/UM Master of Data Science/WQD7001 Principles of Data Science/Group project/Rodent_Inspection_clean03.csv", 
               header = TRUE, sep = ',', stringsAsFactors = FALSE)

df$INSPECTION_TYPE <- as.factor(df$INSPECTION_TYPE)
df$BOROUGH <- as.factor(df$BOROUGH)
df$INSPECTION_DATE <- strptime(df$INSPECTION_DATE, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
df$INSPECTION_DATE <- as.POSIXct(df$INSPECTION_DATE)
df$RESULT <- as.factor(df$RESULT)
df$APPROVED_DATE <- strptime(df$APPROVED_DATE, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
df$APPROVED_DATE <- as.POSIXct(df$APPROVED_DATE)


colSums(is.na(df))

dim(df)

#############################################################################################################





### STEP 4 - CORRECTING INCONSISTENT VALUES
# We focus on the column of street names, because we notice a lot of naming inconsistencies.
# For example, Upper and lower cases, short forms, we want to correct to all title cases.
# Also, for some incomplete street names like 'STREET' and 'AVENUE', we want to correct and standardize
#

# Let's first display all unique street names and sort them
# From what I see, there are 10885 unique street names, though not all are really unique
sort(unique(df$STREET_NAME))

# Lef't do it Borough by Borough since some may have identical street names
# The dplyr pipe operation shows that there are a total of 12367 unique entries for each borough
# This proves that there are indeed identical street names for some boroughs
df %>%
  group_by(BOROUGH) %>%
  distinct(STREET_NAME) %>%
  summarise(count_distinct = n())


#  BOROUGH         count_distinct
#  <fct>                    <int>
#  1 Bronx                   2144
#  2 Brooklyn                3089
#  3 Manhattan               2051
#  4 Queens                  2710
#  5 Staten Island           2373


# Display the unique names for Manhattan (2051 names)
sort(unique(df[df$BOROUGH == 'Manhattan',]$STREET_NAME))


# There is a LOT of work to be done here, and it is troublesome
# 

######################################################################################################

# Create another spare copy of data frame for resetting

df_copy11 <- data.frame(df)

# Reset data frame
df <- data.frame(df_copy11)

######################################################################################################

df$STREET_NAME <- str_to_title(df$STREET_NAME, locale = 'en')

df %>%
  group_by(BOROUGH) %>%
  distinct(STREET_NAME) %>%
  summarise(count_distinct = n())

# Count for Manhattan dropped from 2051 to 1943 (108 standardized)
# Count for Bronx dropped from 2144 to 2016 (128 standardized)
# Count for Brooklyn dropped from 3089 to 2723 (366 standardized)
# Count for Queens dropped from 2710 to 2610 (100 standardized)
# Count for Staten Island dropped from 2373 to 2221 (152 standardized)

# Display the unique names for Manhattan (1943 names)
sort(unique(df[df$BOROUGH == 'Manhattan',]$STREET_NAME))

############################################################################

# START FROM HERE run the string operations!!!
# Directional strings at the end needs to run first
df$STREET_NAME <- gsub(" E$", " East", df$STREET_NAME)
df$STREET_NAME <- gsub(" N$", " North", df$STREET_NAME)
df$STREET_NAME <- gsub(" S$", " South", df$STREET_NAME)
df$STREET_NAME <- gsub(" W$", " West", df$STREET_NAME)


df$STREET_NAME <- gsub(' Ave$', ' Avenue', df$STREET_NAME)
df$STREET_NAME <- gsub(" Ave.$", " Avenue", df$STREET_NAME)
df$STREET_NAME <- gsub("Ave ", "Avenue ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Bl$", " Boulevard", df$STREET_NAME)
df$STREET_NAME <- gsub(" Blvd", " Boulevard", df$STREET_NAME)
df$STREET_NAME <- gsub(" Ct$", " Court", df$STREET_NAME)
df$STREET_NAME <- gsub(" Dr$", " Drive", df$STREET_NAME)
df$STREET_NAME <- gsub("^E ", "East ", df$STREET_NAME)
df$STREET_NAME <- gsub("^E. ", "East ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Expwy", " Expressway", df$STREET_NAME)
df$STREET_NAME <- gsub("^Ft ", "Fort ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Hts$", " Heights", df$STREET_NAME)
df$STREET_NAME <- gsub(" Hwy$", " Highway", df$STREET_NAME)
df$STREET_NAME <- gsub(" Pl$", " Place", df$STREET_NAME)
df$STREET_NAME <- gsub(" Py ", " Parkway ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Pkwy$", " Parkway", df$STREET_NAME)
df$STREET_NAME <- gsub(" Rd$", " Road", df$STREET_NAME)
df$STREET_NAME <- gsub(" Rvr ", " River ", df$STREET_NAME)
df$STREET_NAME <- gsub("^St ", "Saint ", df$STREET_NAME)
df$STREET_NAME <- gsub("^St. ", "Saint ", df$STREET_NAME)
df$STREET_NAME <- gsub(" St$", " Street", df$STREET_NAME)
df$STREET_NAME <- gsub(" St.$", " Street", df$STREET_NAME)
df$STREET_NAME <- gsub(" Ter$", " Terrace", df$STREET_NAME)
df$STREET_NAME <- gsub(" Tnpk$", " Turnpike", df$STREET_NAME)
df$STREET_NAME <- gsub(" Tpke$", " Turnpike", df$STREET_NAME)
df$STREET_NAME <- gsub("W ", "West ", df$STREET_NAME)

df$STREET_NAME <- gsub("^Mt ", "Mount ", df$STREET_NAME)
df$STREET_NAME <- gsub("^St. ", "Saint ", df$STREET_NAME)
df$STREET_NAME <- gsub("Sq ", "Square ", df$STREET_NAME)

df$STREET_NAME <- gsub("    ", " ", df$STREET_NAME)
df$STREET_NAME <- gsub("   ", " ", df$STREET_NAME)
df$STREET_NAME <- gsub("  ", " ", df$STREET_NAME)


# Highway or expressway related strings
df$STREET_NAME <- gsub(" Bx ", " Bronx ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Svc Rd ", " Service Road ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Sr ", " Service Road ", df$STREET_NAME)


# Create a for loop just for 11, 12 and 13 orders
for (ord_num in as.character(11:13)) {
  df$STREET_NAME <- gsub(paste(ord_num, " ", sep = ''), paste(ord_num, "th ", sep = ''), df$STREET_NAME)
}


# For loop for the rest of the orders
for (ord_num in as.character(0:9)) {
  if (ord_num == 1) {
    df$STREET_NAME <- gsub(paste(ord_num, " ", sep = ''), paste(ord_num, "st ", sep = ''), df$STREET_NAME)
  } else if (ord_num == 2) {
    df$STREET_NAME <- gsub(paste(ord_num, " ", sep = ''), paste(ord_num, "nd ", sep = ''), df$STREET_NAME)
  } else if (ord_num == 3) {
    df$STREET_NAME <- gsub(paste(ord_num, " ", sep = ''), paste(ord_num, "rd ", sep = ''), df$STREET_NAME)
  } else {
    df$STREET_NAME <- gsub(paste(ord_num, " ", sep = ''), paste(ord_num, "th ", sep = ''), df$STREET_NAME)
  }
}


# For ordered words standardize to number format like first, second etc
ordered_word <- c("First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth", "Ninth", "Tenth")
for (num in 1:10) {
  if (num == 1) {
    df$STREET_NAME <- gsub(paste(ordered_word[num], " ", sep = ''), paste(as.character(num), "st ", sep = ''), df$STREET_NAME)
  } else if (num == 2) {
    df$STREET_NAME <- gsub(paste(ordered_word[num], " ", sep = ''), paste(as.character(num), "nd ", sep = ''), df$STREET_NAME)
  } else if (num == 3) {
    df$STREET_NAME <- gsub(paste(ordered_word[num], " ", sep = ''), paste(as.character(num), "rd ", sep = ''), df$STREET_NAME)
  } else {
    df$STREET_NAME <- gsub(paste(ordered_word[num], " ", sep = ''), paste(as.character(num), "th ", sep = ''), df$STREET_NAME)
  }
}


for (street in df[grepl("^Mc", df$STREET_NAME) & !grepl("^Mc ", df$STREET_NAME), 'STREET_NAME']) {
  street_split <- strsplit(street, split = '')[[1]]
  street_split[3] <- toupper(street_split[3])
  new_street <- paste(street_split, sep = '', collapse = '')
  df[df$STREET_NAME == street, 'STREET_NAME'] <- new_street
}

# Combine the Mc and it's next string to make them complete
df$STREET_NAME <- gsub("^Mc ", "Mc", df$STREET_NAME)

# Check the 'Mc' street names and see the change
df[grepl("^Mc", df$STREET_NAME) & !grepl("^Mc ", df$STREET_NAME), 'STREET_NAME']


# Change the "1/2nd" back to "1/2" since the previous gsub accidentally modified the strings
df$STREET_NAME <- gsub("^1/2nd ", "1/2 ", df$STREET_NAME)
df$STREET_NAME <- gsub("^1/2 St ", "1/2 Saint ", df$STREET_NAME)


# IF, for some reason, the double space replacement did not work so we run again
# df$STREET_NAME <- gsub("  ", " ", df$STREET_NAME)

# Finish up the highway related standardization
df$STREET_NAME <- gsub(" N Sr$", " Service Road North", df$STREET_NAME)
df$STREET_NAME <- gsub(" N Svc Road$", " Service Road North", df$STREET_NAME)
df$STREET_NAME <- gsub(" E Service Road$", " Service Road East", df$STREET_NAME)
df$STREET_NAME <- gsub(" Nb$", " North", df$STREET_NAME)
df$STREET_NAME <- gsub(" Sb$", " South", df$STREET_NAME)
df$STREET_NAME <- gsub(" Sr$", " Service Road", df$STREET_NAME)
df$STREET_NAME <- gsub("Vanwyck ", "Van Wyck ", df$STREET_NAME)
df$STREET_NAME <- gsub("Throggs ", "Throgs ", df$STREET_NAME)
df$STREET_NAME <- gsub("Throg ", "Throgs ", df$STREET_NAME)
df$STREET_NAME <- gsub("Bklyn ", "Brooklyn ", df$STREET_NAME)
df$STREET_NAME <- gsub("Gowanu ", "Gowanus ", df$STREET_NAME)

# Churck related standardization
df$STREET_NAME <- gsub("Nichlas ", "Nicholas ", df$STREET_NAME)
df$STREET_NAME <- gsub("Nicolas ", "Nicholas ", df$STREET_NAME)
df$STREET_NAME <- gsub("Lukes ", "Luke's ", df$STREET_NAME)
df$STREET_NAME <- gsub("Marks ", "Mark's ", df$STREET_NAME)


######################################################################################################

# Create another spare copy of data frame for resetting

df_copy12 <- data.frame(df)

# Reset data frame
df <- data.frame(df_copy12)

######################################################################################################


# Some other misc replacements
df$STREET_NAME <- gsub("^Fred ", "Frederick ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Douglas ", " Douglass ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Fredric ", "Frederick ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Fredrick ", "Frederick ", df$STREET_NAME)
df$STREET_NAME <- gsub(" B$", " Boulevard", df$STREET_NAME)
df$STREET_NAME <- gsub("^West106th ", "West 106th ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Hrlm ", "Harlem River ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Fdr", "FDR", df$STREET_NAME)
df$STREET_NAME <- gsub("^F D R ", "FDR ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Edward Morgan ", "^Edward M Morgan ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Vllge$", " Village", df$STREET_NAME)
df$STREET_NAME <- gsub("^Williams ", "Williams ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Plce$", " Place", df$STREET_NAME)
df$STREET_NAME <- gsub(" St Extension", " Street", df$STREET_NAME)
df$STREET_NAME <- gsub("^Y West ", "West ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Street.$", " Street", df$STREET_NAME)
df$STREET_NAME <- gsub("^Avenue Of The Amer", "6th Avenue", df$STREET_NAME)
df$STREET_NAME <- gsub("^Avenue Of Americas", "6th Avenue", df$STREET_NAME)
df$STREET_NAME <- gsub("^Avenue Of The Americ", "6th Avenue", df$STREET_NAME)
df$STREET_NAME <- gsub("^Avenue Of The Americas", "6th Avenue", df$STREET_NAME)
df$STREET_NAME <- gsub("^Adam C Powell ", "Adam Clayton Powell Jr ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Adam C Powell Boulev", "Adam Clayton Powell Jr Boulevard", df$STREET_NAME)
df$STREET_NAME <- gsub("Adam Clayton Powell$", "Adam Clayton Powell Jr Boulevard", df$STREET_NAME)
df$STREET_NAME <- gsub(" Powell Boulevard$", " Powell Jr Boulevard", df$STREET_NAME)
df$STREET_NAME <- gsub("^Acp ", "Adam Clayton Powell Jr ", df$STREET_NAME)
df$STREET_NAME <- gsub("Broadway And Amp;Duane Street", "Broadway", df$STREET_NAME)
df$STREET_NAME <- gsub("^Un Plaza", "United Nations Plaza", df$STREET_NAME)
df$STREET_NAME <- gsub("Elk St. And Amp; Chambers Street", "Chambers Street", df$STREET_NAME)
df$STREET_NAME <- gsub("Chambers St.and Amp; Elk Street", "Chambers Street", df$STREET_NAME)
df$STREET_NAME <- gsub("Gold$", "Gold Street", df$STREET_NAME)
df$STREET_NAME <- gsub("Pearl$", "Pearl Street", df$STREET_NAME)
df$STREET_NAME <- gsub("^Pak ", "Park ", df$STREET_NAME)
df$STREET_NAME <- gsub("Stanton$", "Stanton Street", df$STREET_NAME)
df$STREET_NAME <- gsub("Essex$", "Essex Street", df$STREET_NAME)
df$STREET_NAME <- gsub("Lafayette$", "Lafayette Street", df$STREET_NAME)
df$STREET_NAME <- gsub("^Edgecomb ", "Edgecombe ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Thelonius ", "Thelonious ", df$STREET_NAME)
df$STREET_NAME <- gsub("East 116th St-Marin Boulevard", "East 116th Street", df$STREET_NAME)
df$STREET_NAME <- gsub("FDR$", "FDR Drive", df$STREET_NAME)
df$STREET_NAME <- gsub("60th Gol$", "Gold Street", df$STREET_NAME)
df$STREET_NAME <- gsub("^N D ", "Nathan D ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Park.$", " Park", df$STREET_NAME)
df$STREET_NAME <- gsub(" Gdns ", " Gardens ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Avenueic$", " Avenue", df$STREET_NAME)
df$STREET_NAME <- gsub(" Avenueicas$", " Avenue", df$STREET_NAME)
df$STREET_NAME <- gsub(" Crcl$", " Circle", df$STREET_NAME)


# Numbers related standardization/correction that aren't being addressed just now
df$STREET_NAME <- gsub("^159th W. Street", "Amsterdam Avenue", df$STREET_NAME)
df$STREET_NAME <- gsub("^142nd West ", "West ", df$STREET_NAME)
df$STREET_NAME <- gsub(" 3$", " 3rd Street", df$STREET_NAME)
df$STREET_NAME <- gsub(" 73$", " 73th Street", df$STREET_NAME)
df$STREET_NAME <- gsub("East 5th$", "East 5th Street", df$STREET_NAME)
df$STREET_NAME <- gsub("East 5th Th ", "East 5th ", df$STREET_NAME)
df$STREET_NAME <- gsub(" 109st.$", " 109th Street", df$STREET_NAME)
df$STREET_NAME <- gsub("East 61$", "East 61st Street", df$STREET_NAME)
df$STREET_NAME <- gsub(" 82st$", " 82th Street", df$STREET_NAME)
df$STREET_NAME <- gsub(" 145$", " 145th Street", df$STREET_NAME)
df$STREET_NAME <- gsub(" 169$", " 169th Street", df$STREET_NAME)
df$STREET_NAME <- gsub(" 116$", " 116th Street", df$STREET_NAME)
df$STREET_NAME <- gsub("8th$", "8th Avenue", df$STREET_NAME)
df$STREET_NAME <- gsub("Piers 92nd An$", "Pier 92", df$STREET_NAME)
df$STREET_NAME <- gsub("Pier 06$", "Pier 6", df$STREET_NAME)


# Manual correction of 2 rows
df[df$STREET_NAME == "******Frederick Douglass Boulevard**************", 'STREET_NAME'] <- "Frederick Douglass Boulevard"
df[df$STREET_NAME == "C", "STREET_NAME"] <- "Avenue C"



# The codes below are not run yet! I have some doubts running it...
for (row in row.names(df[df$STREET_NAME == "Avenue", ])) {
  bbl <- df[row, 'BBL']
  if (nchar(df[df$BBL == bbl, 'STREET_NAME'][2]) > 7 & !is.na(df[df$BBL == bbl, 'STREET_NAME'][2])) {
    df[row, 'STREET_NAME'] <- df[df$BBL == bbl, 'STREET_NAME'][2]
  } else if (nchar(df[df$BBL == bbl, 'STREET_NAME'][3]) > 7 & !is.na(df[df$BBL == bbl, 'STREET_NAME'][3])) {
    df[row, 'STREET_NAME'] <- df[df$BBL == bbl, 'STREET_NAME'][3]
  } else if (nchar(df[df$BBL == bbl, 'STREET_NAME'][4]) > 7 & !is.na(df[df$BBL == bbl, 'STREET_NAME'][4])) {
    df[row, 'STREET_NAME'] <- df[df$BBL == bbl, 'STREET_NAME'][4]
  } else if (nchar(df[df$BBL == bbl, 'STREET_NAME'][5]) > 7 & !is.na(df[df$BBL == bbl, 'STREET_NAME'][5])) {
    df[row, 'STREET_NAME'] <- df[df$BBL == bbl, 'STREET_NAME'][5]
  }
}

# 7 more rows (2 unique BBL) that did not get imputed, we do it manually
df[df$BBL == 3017490075, 'STREET_NAME'] <- "Throop Avenue"
df[df$BBL == 3017050058, 'STREET_NAME'] <- "Atlantic Avenue"


for (row in row.names(df[df$STREET_NAME == "Square", ])) {
  if (df[row, 'BBL'] == 1001170100) {
    df[row, 'STREET_NAME'] <- 'Chatham Square'
  } else if (df[row, 'BBL'] == 1006160054) {
    df[row, 'STREET_NAME'] <- 'Jackson Square'
  } else if (df[row, 'BBL'] == 3012120004) {
    df[row, 'STREET_NAME'] <- 'Grant Square'
  }
}

# Some rows linked to the BBL above with different street that we'll do the standardization manually
df[(df$BBL == 1001170100) & (df$STREET_NAME == "Pearl Street" | df$STREET_NAME == "Chatam Square"), 'STREET_NAME'] <- "Chatham Square" 
df[(df$BBL == 1006160054) & (df$STREET_NAME == "Greenwich Avenue"), 'STREET_NAME'] <- "Jackson Square"
df[(df$BBL == 3012120004) & (df$STREET_NAME == "Rogers Avenue"), 'STREET_NAME'] <- "Grant Square"

# Some manually imputations
df[df$STREET_NAME == 'Frederick Douglass', 'STREET_NAME'] <- "Frederick Douglass Boulevard"


# Just now it's for 'Avenue' now it's for 'Street'
for (row in row.names(df[df$STREET_NAME == "Street", ])) {
  bbl <- df[row, 'BBL']
  if (nchar(df[df$BBL == bbl, 'STREET_NAME'][2]) > 7 & !is.na(df[df$BBL == bbl, 'STREET_NAME'][2])) {
    df[row, 'STREET_NAME'] <- df[df$BBL == bbl, 'STREET_NAME'][2]
  } else if (nchar(df[df$BBL == bbl, 'STREET_NAME'][3]) > 7 & !is.na(df[df$BBL == bbl, 'STREET_NAME'][3])) {
    df[row, 'STREET_NAME'] <- df[df$BBL == bbl, 'STREET_NAME'][3]
  } else if (nchar(df[df$BBL == bbl, 'STREET_NAME'][4]) > 7 & !is.na(df[df$BBL == bbl, 'STREET_NAME'][4])) {
    df[row, 'STREET_NAME'] <- df[df$BBL == bbl, 'STREET_NAME'][4]
  } else if (nchar(df[df$BBL == bbl, 'STREET_NAME'][5]) > 7 & !is.na(df[df$BBL == bbl, 'STREET_NAME'][5])) {
    df[row, 'STREET_NAME'] <- df[df$BBL == bbl, 'STREET_NAME'][5]
  }
}

# The remaining rows and bbl with 'Street' as street name is at index 1
for (row in row.names(df[df$STREET_NAME == "Street", ])) {
  bbl <- df[row, 'BBL']
  df[row, 'STREET_NAME'] <- df[df$BBL == bbl, 'STREET_NAME'][1]
}


# Check the unique count now
df %>%
  group_by(BOROUGH) %>%
  distinct(STREET_NAME) %>%
  summarise(count_distinct = n())

# Count for Manhattan dropped from 2051 to 1943 to 882 (1060 standardized)
# Count for Bronx dropped from 2144 to 2016 to 1391 (625 standardized)
# Count for Brooklyn dropped from 3089 to 2723 to 1743 (980 standardized)
# Count for Queens dropped from 2710 to 2610 to 1899 (711 standardized)
# Count for Staten Island dropped from 2373 to 2221 to 2117 (104 standardized)


# Display the unique names for Manhattan (975 names)
sort(unique(df[df$BOROUGH == 'Manhattan',]$STREET_NAME))


######################################################################################################

# Create another spare copy of data frame for resetting

df_copy13 <- data.frame(df)

# Reset data frame
df <- data.frame(df_copy13)

######################################################################################################



# Now proceed to Bronx street names
# Display the unique names for Manhattan (1391 names)
sort(unique(df[df$BOROUGH == 'Bronx',]$STREET_NAME))

# String manipulation for remaining inconsistencies in Bronx
df$STREET_NAME <- gsub("^01st Country ", "Country ", df$STREET_NAME)
df$STREET_NAME <- gsub("^02nd Country ", "Country ", df$STREET_NAME)
df$STREET_NAME <- gsub("^03rd Country ", "Country ", df$STREET_NAME)
df$STREET_NAME <- gsub("^04th Country ", "Country ", df$STREET_NAME)
df$STREET_NAME <- gsub("^09th Country ", "Country ", df$STREET_NAME)
df$STREET_NAME <- gsub("^10th Country ", "Country ", df$STREET_NAME)
df$STREET_NAME <- gsub("^1291st Grant ", "Grant ", df$STREET_NAME)
df$STREET_NAME <- gsub("^2096th Morris ", "Morris ", df$STREET_NAME)
df$STREET_NAME <- gsub("^3403rd 3rd ", "3rd ", df$STREET_NAME)
df$STREET_NAME <- gsub("^3ave.", "3rd Avenue", df$STREET_NAME)
df$STREET_NAME <- gsub(" St John$", " Saint John", df$STREET_NAME)
df$STREET_NAME <- gsub(" St Johns$", " Saint John", df$STREET_NAME)
df$STREET_NAME <- gsub(" St. John$", " Saint John", df$STREET_NAME)
df$STREET_NAME <- gsub("^Bailsey ", "Baisley ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Bayshore ", "Bay Shore ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Bl ", " Boulevard ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Bouleva$", " Boulevard", df$STREET_NAME)
df$STREET_NAME <- gsub("^Billingsly ", "Billingsley ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Caroll ", "Carroll ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Castlehill ", "Castle Hill ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Cincinatus ", "Cincinnatus ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Co Op ", "Co-Op ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Courtland ", "Courtlandt ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Ep ", " Expressway ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Park No$", " Park North", df$STREET_NAME)
df$STREET_NAME <- gsub("^Dr M L ", "Dr Martin Luther ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Dr Martin L ", "Dr Martin Luther ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Dr Kazimiroff ", "Dr Theodore Kazimiroff ", df$STREET_NAME)
df$STREET_NAME <- gsub("East 220$", "East 220th Street", df$STREET_NAME)
df$STREET_NAME <- gsub(" Pkwy ", " Parkway ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Mt ", " Mount ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Edward Grant ", "Edward L Grant ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Erksine ", "Erskine ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Fairmont ", "Fairmount ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Zeis ", " Zeiser ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Feather Bed ", "Featherbed ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Fieldstone ", "Fieldston ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Avenue,$", " Avenue", df$STREET_NAME)
df$STREET_NAME <- gsub("^Gd ", "Grand ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Concours$", " Concourse", df$STREET_NAME)
df$STREET_NAME <- gsub(" Av$", " Avenue", df$STREET_NAME)
df$STREET_NAME <- gsub("^Hugh Grant ", "Hugh J Grant ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Hunt's ", "Hunts ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Hunt Point ", "Hunts Point ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Riv ", " River ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Hutch ", "Hutchinson ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Hutchenson ", "Hutchinson ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Pw$", " Parkway", df$STREET_NAME)
df$STREET_NAME <- gsub("^Kennelworth ", "Kennellworth ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Kings Bridge ", "Kingsbridge ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Pl ", " Place ", df$STREET_NAME)
df$STREET_NAME <- gsub("^M L King ", "Dr Martin Luther King ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Mac Cracken ", "McCracken ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Mac Donough ", "McDonough ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Macdonough ", "McDonough ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Mlk ", "Dr Martin Luther King ", df$STREET_NAME)
df$STREET_NAME <- gsub("^O'brien ", "O'Brien ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Obrien ", "O'Brien ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Osbourne ", "Osborne ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Aevnue$", " Avenue", df$STREET_NAME)
df$STREET_NAME <- gsub("^Rev J A ", "Reverend James A ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Rev James Polite ", "Reverend James A Polite ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Rev Polite ", "Reverend James A Polite ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Rev ", "Reverend ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Reservior ", "Reservoir ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Saint Georges ", "Saint George's ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Saint Marys ", "Saint Mary's ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Saint Pauls ", "Saint Paul's ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Saint Raymonds ", "Saint Raymond's ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Saint Raymond ", "Saint Raymond's ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Saint Paul ", "Saint Paul's ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Saint Peters ", "Saint Peter's ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Saint Johns ", "Saint John ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Sedgewick ", "Sedgwick ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Shorr ", "Schorr ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Sound View ", "Soundview ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Theriot ", "Thieriot ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Throgmorton ", "Throggmorton ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Throgmortin ", "Throggmorton ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Throgs Morton ", "Throggmorton ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Tibbet ", "Tibbett ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Tieman ", "Tiemann ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Turner ", "Turneur ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Vancortlandt ", "Van Cortlandt ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Whiteplains ", "White Plains ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Williambridge ", "Williamsbridge ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Williamsbdg ", "Williamsbridge ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Woodcrest ", "Woodycrest ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Ctr$", " Center", df$STREET_NAME)
df$STREET_NAME <- gsub("^Sacket ", "Sackett ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Coop ", "Co-Op ", df$STREET_NAME)


# Manual replacements
df[df$STREET_NAME == '229th Drive South', 'STREET_NAME'] <- "East 229th Street"
df[df$STREET_NAME == 'College', 'STREET_NAME'] <- "Morris Avenue"
df[df$STREET_NAME == 'Concourse', 'STREET_NAME'] <- "Grand Concourse"
df[df$STREET_NAME == 'Cross Bronx Service Road North', 'STREET_NAME'] <- "Cross Bronx Expressway Service Road North"
df[df$STREET_NAME == 'Cross Bronx Service Road South', 'STREET_NAME'] <- "Cross Bronx Expressway Service Road South"
df[df$STREET_NAME == 'Crotona', 'STREET_NAME'] <- "Crotona Avenue"
df[df$STREET_NAME == 'Cruger', 'STREET_NAME'] <- "Pelham Parkway North"
df[df$STREET_NAME == 'Dr M L King Jr Boule', 'STREET_NAME'] <- "Dr Martin Luther King Jr Boulevard"
df[df$STREET_NAME == 'Fairmount', 'STREET_NAME'] <- "Fairmount Place"
df[df$STREET_NAME == 'Grandconcourse', 'STREET_NAME'] <- "Grand Concourse"
df[df$STREET_NAME == 'Grant Highway', 'STREET_NAME'] <- "Edward L Grant Highway"
df[df$STREET_NAME == 'Hawkins', 'STREET_NAME'] <- "Hawkins Street"
df[df$STREET_NAME == 'L Grant Highway', 'STREET_NAME'] <- "Edward L Grant Highway"
df[df$STREET_NAME == 'Lydig', 'STREET_NAME'] <- "Lydig Avenue"
df[df$STREET_NAME == 'Magentastreet', 'STREET_NAME'] <- "Magenta Street"
df[df$STREET_NAME == 'Martin Luther King Avenue', 'STREET_NAME'] <- "Bartow Avenue"
df[df$STREET_NAME == 'Martin King Jr Boulevard', 'STREET_NAME'] <- "Dr Martin Luther King Jr Boulevard"
df[df$STREET_NAME == 'Martin Luther King Jr Boulevard', 'STREET_NAME'] <- "Dr Martin Luther King Jr Boulevard"
df[df$STREET_NAME == 'Rev James A Polite', 'STREET_NAME'] <- "Reverend James A Polite Avenue"
df[df$STREET_NAME == 'Road', 'STREET_NAME'] <- "Macombs Road"
df[df$STREET_NAME == 'Saint Ann&Apos;S Avenue', 'STREET_NAME'] <- "Saint Anns Avenue"
df[df$STREET_NAME == 'Taylor', 'STREET_NAME'] <- "Taylor Avenue"
df[df$STREET_NAME == 'Walton', 'STREET_NAME'] <- "Walton Avenue"
df[df$STREET_NAME == 'Zeiser Place', 'STREET_NAME'] <- "Father Zeiser Place"


df[df$STREET_NAME == 'Road', 'STREET_NAME'] <- ""
df[df$STREET_NAME == 'Road', 'STREET_NAME'] <- ""


# Just now it's for 'Avenue' now it's for 'Place'
for (row in row.names(df[df$STREET_NAME == "Place", ])) {
  bbl <- df[row, 'BBL']
  if (nchar(df[df$BBL == bbl, 'STREET_NAME'][1]) > 6 & !is.na(df[df$BBL == bbl, 'STREET_NAME'][1])) {
    df[row, 'STREET_NAME'] <- df[df$BBL == bbl, 'STREET_NAME'][1]
  } else if (nchar(df[df$BBL == bbl, 'STREET_NAME'][2]) > 6 & !is.na(df[df$BBL == bbl, 'STREET_NAME'][2])) {
    df[row, 'STREET_NAME'] <- df[df$BBL == bbl, 'STREET_NAME'][2]
  } else if (nchar(df[df$BBL == bbl, 'STREET_NAME'][3]) > 6 & !is.na(df[df$BBL == bbl, 'STREET_NAME'][3])) {
    df[row, 'STREET_NAME'] <- df[df$BBL == bbl, 'STREET_NAME'][3]
  } else if (nchar(df[df$BBL == bbl, 'STREET_NAME'][4]) > 6 & !is.na(df[df$BBL == bbl, 'STREET_NAME'][4])) {
    df[row, 'STREET_NAME'] <- df[df$BBL == bbl, 'STREET_NAME'][4]
  }
}


# Replacements based on BBL
df[df$BBL == 2024748900, 'STREET_NAME'] <- "Gerard Avenue"
df[df$BBL == 2028790123, 'STREET_NAME'] <- "University Avenue"
df[df$BBL == 2032730800, 'STREET_NAME'] <- "East Fordham Road"


# Check the unique count now
df %>%
  group_by(BOROUGH) %>%
  distinct(STREET_NAME) %>%
  summarise(count_distinct = n())

# Count for Manhattan dropped from 2051 to 1943 to 882 (1060 standardized)
# Count for Bronx dropped from 2144 to 2016 to 1273 (743 standardized)
# Count for Brooklyn dropped from 3089 to 2723 to 1743 (980 standardized)
# Count for Queens dropped from 2710 to 2610 to 1899 (711 standardized)
# Count for Staten Island dropped from 2373 to 2221 to 2117 (104 standardized)


######################################################################################################

# Create another spare copy of data frame for resetting

df_copy14 <- data.frame(df)

# Reset data frame
df <- data.frame(df_copy14)

######################################################################################################



# Now proceed to Brooklyn street names
# Display the unique names for Manhattan (1743 names)
sort(unique(df[df$BOROUGH == 'Brooklyn',]$STREET_NAME))

# Regex gsub replacement for Brooklyn
df$STREET_NAME <- gsub("^A Bay ", "Bay ", df$STREET_NAME)
df$STREET_NAME <- gsub("^A Flatbush ", "Flatbush ", df$STREET_NAME)
df$STREET_NAME <- gsub("^A Ide ", "Ide ", df$STREET_NAME)
df$STREET_NAME <- gsub("^A Quincy ", "Quincy ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Albermarle ", "Albemarle ", df$STREET_NAME)
df$STREET_NAME <- gsub("^B Ide ", "Ide ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Bayridge ", "Bay Ridge ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Bayview ", "Bay View ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Berkley ", "Berkeley ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Brightn ", "Brighton ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Be ", " Beach ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Celest ", "Celeste ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Gdns$", " Gardens", df$STREET_NAME)
df$STREET_NAME <- gsub("^Dosher ", "Doscher ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Ext$", " Extension", df$STREET_NAME)
df$STREET_NAME <- gsub("^Ecford ", "Eckford ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Eight ", "8th ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Parkwa$", " Parkway", df$STREET_NAME)
df$STREET_NAME <- gsub(" Green ", " Greene ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Hamiltn ", " Hamilton ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Hendricksn ", "Hendrickson ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Herzl ", "Herzel ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Street Adj.$", " Street", df$STREET_NAME)
df$STREET_NAME <- gsub(" Street Adj$", " Street", df$STREET_NAME)
df$STREET_NAME <- gsub("^Jewel Mc Coy ", "Jewel McCoy ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Mckoy ", " McKoy ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Kingland ", "Kingsland ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Kingsboro ", "Kingsborough ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Knickerbock ", "Knickerbocker ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Kosciusko ", "Kosciuszko ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Lexinton ", "Lexington ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Lennox ", "Lenox ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Mac Donald ", "McDonald ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Macdonald ", "McDonald ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Mac Kenzie ", "Mackenzie ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Martence ", "Martense ", df$STREET_NAME)
df$STREET_NAME <- gsub("^McKibben ", "McKibbin ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Mehanhan ", "Menahan ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Meserau ", "Mesereau ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Metro Tech ", "MetroTech ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Metrotech ", "MetroTech ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Boulev$", " Boulevard", df$STREET_NAME)
df$STREET_NAME <- gsub("^N ", "North ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Utrecth ", " Utrecht ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Newjersey ", "New Jersey ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Nichols ", "Nicholas ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Osborne ", "Osborn ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Paedergat ", "Paerdegat ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Paerdegate ", "Paerdegat ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Parrot ", "Parrott ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Pennsylvan ", "Pennsylvania ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Avunue$", " Avenue", df$STREET_NAME)
df$STREET_NAME <- gsub(" St ", " Street ", df$STREET_NAME)
df$STREET_NAME <- gsub(" South West$", " Southwest", df$STREET_NAME)
df$STREET_NAME <- gsub(" Sw$", " Southwest", df$STREET_NAME)
df$STREET_NAME <- gsub("^Richard ", "Richards ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Rockawy ", "Rockaway ", df$STREET_NAME)
df$STREET_NAME <- gsub("^S ", "South ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Saint John ", "Saint John's ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Saint Judes ", "Saint Jude ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Sand ", "Sands ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Schaeffer ", "Schaefer ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Schenk ", "Schenck ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Schermerhn ", "Schermerhorn ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Seabreeze ", "Sea Breeze ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Seigal ", "Seigel ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Sheep ", "Sheepshead ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Shephard ", "Shepherd ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Aveniue$", " Avenue", df$STREET_NAME)
df$STREET_NAME <- gsub("^Sout ", "South ", df$STREET_NAME)
df$STREET_NAME <- gsub("^South Gate ", "Southgate ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Stillwell ", "Stillwells ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Vanburen ", "Van Buren ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Vanbrunt ", "Van Brunt ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Vandeveer ", "Vanderveer ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Vansicklin ", "Van Sicklen ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Vandyke ", "Van Dyke ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Vansicklen ", "Van Sicklen ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Vansiclen ", "Van Siclen ", df$STREET_NAME)
df$STREET_NAME <- gsub(" No$", " North", df$STREET_NAME)
df$STREET_NAME <- gsub("^Voorhees ", "Voorhies ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Wasington ", "Washington ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Whyte ", "Wythe ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Winsor ", "Windsor ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Wykoff ", "Wyckoff ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Ny ", " New York ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Elderts  ", "Eldert  ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Dewitt ", "De Witt ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Belvedere  ", "Belvidere ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Mac Dougal ", "MacDougal ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Macdougal ", "MacDougal ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Malcom X ", "Malcolm X ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Mererole ", "Meserole ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Pitkins ", "Pitkin ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Schaffer ", "Schaefer ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Schnectady ", "Schenectady ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Teneyck ", "Ten Eyck ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Marlboro ", "Marlborough ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Ny ", "New York ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Vandam ", "Van Dam ", df$STREET_NAME)


# Manual replacement for Brooklyn
df[df$STREET_NAME == '20th Ln', 'STREET_NAME'] <- "20th Avenue"
df[df$STREET_NAME == '65', 'STREET_NAME'] <- "65th Street"
df[df$STREET_NAME == 'Brevoort', 'STREET_NAME'] <- "Brevoort Place"
df[df$STREET_NAME == 'Broadwa', 'STREET_NAME'] <- "Broadway"
df[df$STREET_NAME == 'Ditmas', 'STREET_NAME'] <- "Ditmas Avenue"
df[df$STREET_NAME == 'Dr Martin Luther King Place', 'STREET_NAME'] <- "Martin Luther King Jr Place"
df[df$STREET_NAME == 'Kingshigwy', 'STREET_NAME'] <- "Kings Highway"
df[df$STREET_NAME == 'L King Place', 'STREET_NAME'] <- "Martin Luther King Jr Place"
df[df$STREET_NAME == 'Malbone', 'STREET_NAME'] <- "Malbone Street"
df[df$STREET_NAME == 'Martin L King Place', 'STREET_NAME'] <- "Martin Luther King Jr Place"
df[df$STREET_NAME == 'Martin Luther King Place', 'STREET_NAME'] <- "Martin Luther King Jr Place"
df[df$STREET_NAME == 'P Avenue', 'STREET_NAME'] <- "Avenue P"
df[df$STREET_NAME == 'Ppw', 'STREET_NAME'] <- "Prospect Park West"
df[df$STREET_NAME == 'Prospect', 'STREET_NAME'] <- "Prospect Avenue"
df[df$STREET_NAME == 'Sheephead Bay', 'STREET_NAME'] <- "Sheepshead Bay Road"
df[df$STREET_NAME == 'Stuyvesant', 'STREET_NAME'] <- "Stuyvesant Avenue"
df[df$STREET_NAME == 'Thomas Boyland Street', 'STREET_NAME'] <- "Thomas S Boyland Street"
df[df$STREET_NAME == 'Tompkins', 'STREET_NAME'] <- "Tompkins Avenue"
df[df$STREET_NAME == 'V Avenue', 'STREET_NAME'] <- "Avenue V"
df[df$STREET_NAME == 'X Avenue', 'STREET_NAME'] <- "Avenue X"
df[df$STREET_NAME == 'Z Avenue', 'STREET_NAME'] <- "Avenue Z"
df[df$STREET_NAME == 'X Boulevard', 'STREET_NAME'] <- "Malcolm X Boulevard"
df[df$STREET_NAME == 'D Avenue', 'STREET_NAME'] <- "Avenue D"
df[df$STREET_NAME == 'Braodway', 'STREET_NAME'] <- "Broadway"

df[(df$STREET_NAME == 'Newell Street') & (df$BOROUGH == 'Brooklyn'), 'STREET_NAME'] <- "Newel Street"


# Replacements based on BBL
df[df$BBL == 3049300005, 'STREET_NAME'] <- "Beverly Road"
df[df$BBL == 3049300006, 'STREET_NAME'] <- "Beverly Road"
df[df$BBL == 3001480007, 'STREET_NAME'] <- "Myrtle Avenue"
df[df$BBL == 3026880042, 'STREET_NAME'] <- "Monitor Street"
df[df$BBL == 3026880001, 'STREET_NAME'] <- "Monitor Street"
df[df$BBL == 3026880001, 'STREET_NAME'] <- "Monitor Street"
df[df$BBL == 3026900001, 'STREET_NAME'] <- "Nassau Avenue"
df[df$BBL == 3026870001, 'STREET_NAME'] <- "Russell Street"
df[df$BBL == 3050600032, 'STREET_NAME'] <- "Saint Paul's Place"
df[df$BBL == 1018210031, 'STREET_NAME'] <- "Lenox Avenue"
df[df$BBL == 3033030027, 'STREET_NAME'] <- "Greene Avenue"
df[df$BBL == 3033260021, 'STREET_NAME'] <- "Grove Street"
df[df$BBL == 3045970001, 'STREET_NAME'] <- "East New York Avenue"
df[df$BBL == 3087250001, 'STREET_NAME'] <- "Coney Island Avenue"
df[df$BBL == 3031940014, 'STREET_NAME'] <- "Ditmars Street"

df[df$BBL == 3087250001, 'ZIP_CODE'] <- 11235


# Check the unique count now
df %>%
  group_by(BOROUGH) %>%
  distinct(STREET_NAME) %>%
  summarise(count_distinct = n())

# Count for Manhattan dropped from 2051 to 1943 to 882 (1060 standardized)
# Count for Bronx dropped from 2144 to 2016 to 1273 (743 standardized)
# Count for Brooklyn dropped from 3089 to 2723 to 1579 (1144 standardized)
# Count for Queens dropped from 2710 to 2610 to 1899 (711 standardized)
# Count for Staten Island dropped from 2373 to 2221 to 2117 (104 standardized)


######################################################################################################

# Create another spare copy of data frame for resetting

df_copy15 <- data.frame(df)

# Reset data frame
df <- data.frame(df_copy15)

######################################################################################################


# Now proceed to Queens street names
# Display the unique names for Queens (1889 names)
sort(unique(df[df$BOROUGH == 'Queens',]$STREET_NAME))

# Regex gsub replacement for Queens
df$STREET_NAME <- gsub("^B ", "Beach ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Bascon ", "Bascom ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Booth Meml ", "Booth Memorial ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Cedar Croft ", "Cedarcroft ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Beach59th ", "Beach 59th ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Centrevile ", "Centreville ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Charlecotte ", "Charlecote ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Chevychase ", "Chevy Chase ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Cohancey ", "Cohancy ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Commwth ", "Commonwealth ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Corprl Kennedy ", "Corporal Kennedy ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Crossbay ", "Cross Bay ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Boulearvd$", " Boulevard", df$STREET_NAME)
df$STREET_NAME <- gsub("^Cypres Hil ", "Cypress Hills ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Daniel ", "Daniels ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Decosta ", "De Costa ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Dumphries ", "Dumfries ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Mdow ", " Meadow ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Ln", " Lane", df$STREET_NAME)
df$STREET_NAME <- gsub(" Pd ", " Pond ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Garret ", "Garrett ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Grand Cent ", "Grand Central ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Guy Brewer ", "Guy R Brewer ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Healey ", "Healy ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Hemstead ", "Hempstead ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Hookcreek ", "Hook Creek ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Hor Harding ", "Horace Harding ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Humphrey ", "Humphreys ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Jamiaca ", "Jamaica ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Laburnam ", "Laburnum ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Lanette ", "Lanett ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Lineaus ", "Linneaus ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Mobil ", "Mobile ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Mulbach ", "Muhlebach ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Newberg ", "Newburg ", df$STREET_NAME)
df$STREET_NAME <- gsub("^O'donnell ", "O'Donnell ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Oceancrest ", "Ocean Crest ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Pine Grove ", "Pinegrove ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Plunket ", "Plunkett ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Polhemas ", "Polhemus ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Pompei ", "Pompeii ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Purvis ", "Purves ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Plz ", " Plaza ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Boule$", " Boulevard", df$STREET_NAME)
df$STREET_NAME <- gsub(" Frnt ", " Front ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Shorefront ", "Shore Front ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Avene$", " Avenue", df$STREET_NAME)
df$STREET_NAME <- gsub("^Springfild ", "Springfield ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Summerfild ", "Summerfield ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Toten ", "Totten ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Tuskegee Airman ", "Tuskegee Airmen ", df$STREET_NAME)
df$STREET_NAME <- gsub("^White Hall ", "Whitehall ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Roscoe ", "Ruscoe ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Expre$", " Expressway", df$STREET_NAME)


# Manual replacement for Queens
df[df$STREET_NAME == 'Baywater Avenue', 'STREET_NAME'] <- "Bayswater Avenue"
df[df$STREET_NAME == 'Bayswater Court', 'STREET_NAME'] <- "Baywater Court"
df[df$STREET_NAME == 'Centerville Avenue', 'STREET_NAME'] <- "Centreville Street"
df[df$STREET_NAME == 'Cip', 'STREET_NAME'] <- "Cross Island Parkway"
df[(df$STREET_NAME == 'De Kalb Avenue') & (df$BOROUGH == 'Queens'), 'STREET_NAME'] <- "Dekalb Avenue"
df[(df$STREET_NAME == 'Elderts Lane') & (df$BOROUGH == 'Queens'), 'STREET_NAME'] <- "Eldert Lane"
df[df$STREET_NAME == 'Gcp', 'STREET_NAME'] <- "Grand Central Parkway"
df[df$STREET_NAME == 'Manton', 'STREET_NAME'] <- "Manton Street"
df[(df$STREET_NAME == 'South Road') & (df$BOROUGH == 'Queens'), 'STREET_NAME'] <- "Tuskegee Airmen Way"
df[(df$STREET_NAME == 'Thompson Avenue') & (df$BOROUGH == 'Queens'), 'STREET_NAME'] <- "Thomson Avenue"
df[df$STREET_NAME == 'Wyckoff', 'STREET_NAME'] <- "Wyckoff Avenue"



# Replacements based on BBL
df[df$BBL == 4013130022, 'STREET_NAME'] <- "73rd Street"
df[df$BBL == 4099760005, 'STREET_NAME'] <- "139th Street"
df[df$BBL == 4026110447, 'STREET_NAME'] <- "Flushing Avenue"
df[df$BBL == 4162270013, 'STREET_NAME'] <- "Beach 117th Street"
df[df$BBL == 4010160098, 'STREET_NAME'] <- "Brooklyn Queens Expressway"
df[df$BBL == 4010190002, 'STREET_NAME'] <- "Brooklyn Queens Expressway"
df[df$BBL == 4097150228, 'STREET_NAME'] <- "84th Drive"
df[df$BBL == 4073040039, 'STREET_NAME'] <- "206th Street"
df[df$BBL == 4105920150, 'STREET_NAME'] <- "Grand Central Parkway"
df[df$BBL == 4022650106, 'STREET_NAME'] <- "Grand Central Parkway"
df[df$BBL == 4021620145, 'STREET_NAME'] <- "Grand Central Parkway"
df[df$BBL == 4019180110, 'STREET_NAME'] <- "Horace Harding Expressway"
df[df$BBL == 4051360008, 'STREET_NAME'] <- "Colden Street"
df[df$BBL == 4102540030, 'STREET_NAME'] <- "109th Avenue"
df[df$BBL == 4121788900, 'STREET_NAME'] <- "Dillon Street"
df[df$BBL == 4011860001, 'STREET_NAME'] <- "Northern Boulevard"
df[df$BBL == 4065487501, 'STREET_NAME'] <- "Park Drive East"
df[df$BBL == 4045440042, 'STREET_NAME'] <- "Powells Cove Boulevard"
df[df$BBL == 4045440040, 'STREET_NAME'] <- "Powells Cove Boulevard"
df[df$BBL == 4051930088, 'STREET_NAME'] <- "Robinson Street"
df[df$BBL == 4119540048, 'STREET_NAME'] <- "Van Wyck Expressway"
df[df$BBL == 4116150032, 'STREET_NAME'] <- "Van Wyck Expressway"
df[df$BBL == 4099720001, 'STREET_NAME'] <- "Van Wyck Expressway Service Road East"
df[df$BBL == 4094500047, 'STREET_NAME'] <- "Van Wyck Expressway Service Road West"
df[df$BBL == 4116390040, 'STREET_NAME'] <- "Van Wyck Expressway Service Road West"
df[df$BBL == 4095890019, 'STREET_NAME'] <- "Van Wyck Expressway Service Road West"
df[df$BBL == 4117600236, 'STREET_NAME'] <- "Van Wyck Expressway"
df[df$BBL == 4041540250, 'STREET_NAME'] <- "Whitestone Bridge Boulevard"
df[df$BBL == 4014930003, 'STREET_NAME'] <- "82nd Street"
df[df$BBL == 4120120010, 'STREET_NAME'] <- "140th Street"


# Check the unique count now
df %>%
  group_by(BOROUGH) %>%
  distinct(STREET_NAME) %>%
  summarise(count_distinct = n())

# Count for Manhattan dropped from 2051 to 1943 to 882 (1060 standardized)
# Count for Bronx dropped from 2144 to 2016 to 1273 [to now 1270?] (743 + 3 standardized)
# Count for Brooklyn dropped from 3089 to 2723 to 1579 (1144 standardized)
# Count for Queens dropped from 2710 to 2610 to 1785 (825 standardized)
# Count for Staten Island dropped from 2373 to 2221 to 2107 (104 + 10 standardized)


######################################################################################################

# Create another spare copy of data frame for resetting

df_copy16 <- data.frame(df)

# Reset data frame
df <- data.frame(df_copy16)

######################################################################################################


# Now proceed to Staten Island street names
# Display the unique names for Staten Island (2107 names)
sort(unique(df[df$BOROUGH == 'Staten Island',]$STREET_NAME))

# Regex gsub replacement for Staten Island
df$STREET_NAME <- gsub("^Arthur Kil ", "Arthur Kill ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Aspen Knol ", "Aspen Knolls ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Beverley ", "Beverly ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Burk ", "Burke ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Cedar Cliff ", "Cedarcliff ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Cedargrove ", "Cedar Grove ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Claypit ", "Clay Pit ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Cortland ", "Cortlandt ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Dehart ", "De Hart ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Degroot ", "De Groot ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Dianas ", "Diana ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Dongan Hil ", "Dongan Hills ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Drummgoole ", "Drumgoole ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Ebditts ", "Ebbitts ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Elm Tree ", "Elmtree ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Eltingeville ", "Eltingville ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Eltingvill ", "Eltingville ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Fairbank ", "Fairbanks ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Fielder ", "Fiedler ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Fingerboar ", "Fingerboard ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Fr ", "Father ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Gardinia ", "Gardenia ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Grand View ", "Grandview ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Harborview ", "Harbor View ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Houseman ", "Housman ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Hugenot ", "Huguenot ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Jaques ", "Jacques ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Jeanette ", "Jeannette ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Jers3y ", "Jersey ", df$STREET_NAME)
df$STREET_NAME <- gsub("^La Forge ", "Laforge ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Little Field ", "Littlefield ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Macarthur ", "McArthur ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Macfarland ", "McFarland ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Mac Clean ", "McClean ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Massachuse ", "Massachusetts ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Matthews ", "Mathews ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Merry Mount ", "Merrymount ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Moringstar ", "Morningstar ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Morning Star ", "Morningstar ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Netherlands ", "Netherland ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Nutley ", "Nutly ", df$STREET_NAME)
df$STREET_NAME <- gsub("^O'gorman ", "O'Gorman ", df$STREET_NAME)
df$STREET_NAME <- gsub("^O Gorman ", "O'Gorman ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Oconner ", "O'Connor ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Oconnor ", "O'Connor ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Orchard La ", "Orchard Lane ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Parkhill ", "Park Hill ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Terr$", " Terrace", df$STREET_NAME)
df$STREET_NAME <- gsub("^Pendelton ", "Pendleton ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Richmd ", "Richmond ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Saint Albans ", "Saint Alban's ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Saint Mary ", "Saint Mary's ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Sharrots ", "Sharrotts ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Shelden ", "Sheldon ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Silverlake ", "Silver Lake ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Stone Crest ", "Stonecrest ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Sweet Brok ", "Sweetbrook ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Sweet Water ", "Sweetwater ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Thorny Croft ", "Thornycroft ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Twombley ", "Twombly ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Vanname ", "Van Name ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Vanallen ", "Van Allen ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Vasser ", "Vassar ", df$STREET_NAME)
df$STREET_NAME <- gsub(" Rd ", " Road ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Vonbraun ", "Von Braun ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Watchague ", "Watchogue ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Vanpelt ", "Van Pelt ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Wellbrooke ", "Wellbrook ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Willow Brook ", "Willowbrook ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Wood Cutters ", "Woodcutters ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Wooley ", "Woolley ", df$STREET_NAME)

df$STREET_NAME <- gsub("^Garabaldi ", "Garibaldi ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Vanduzer ", "Van Duzer ", df$STREET_NAME)
df$STREET_NAME <- gsub("^Greenvalley ", "Green Valley ", df$STREET_NAME)




# Replacements based on BBL
df[df$BBL == 5015428900, 'STREET_NAME'] <- "Richmond Avenue"
df[df$BBL == 5010390010, 'STREET_NAME'] <- "Cortlandt Street"
df[df$BBL == 5054740024, 'STREET_NAME'] <- "East Perkiomen Avenue"
df[df$BBL == 5024507503, 'STREET_NAME'] <- "Richmond Hill Road"
df[df$BBL == 5028350033, 'STREET_NAME'] <- "Harbor View Place South"
df[df$BBL == 5010420075, 'STREET_NAME'] <- "Haughwout Avenue"
df[df$BBL == 5062970039, 'STREET_NAME'] <- "Ionia Avenue"
df[df$BBL == 5057010020, 'STREET_NAME'] <- "Ionia Avenue"
df[df$BBL == 5001940048, 'STREET_NAME'] <- "Jewett Avenue"
df[df$BBL == 5034030026, 'STREET_NAME'] <- "Lansing Street"
df[df$BBL == 5000620006, 'STREET_NAME'] <- "Van Buren Street"
df[df$BBL == 5052820057, 'STREET_NAME'] <- "William Avenue"
df[df$BBL == 5052820055, 'STREET_NAME'] <- "William Avenue"
df[df$BBL == 5046560001, 'STREET_NAME'] <- "Windemere Avenue"

df[df$BBL == 5012780003, 'STREET_NAME'] <- "Pond Way"
df[df$BBL == 5007680001, 'STREET_NAME'] <- "Graves Street"
df[df$BBL == 1011110001, 'STREET_NAME'] <- "5th Avenue"



# Check the unique count now
df %>%
  group_by(BOROUGH) %>%
  distinct(STREET_NAME) %>%
  summarise(count_distinct = n())

# Count for Manhattan dropped from 2051 to 1943 to 881 [to now 878?] (1060 + 3 standardized)
# Count for Bronx dropped from 2144 to 2016 to 1273 [to now 1270?] (743 + 3 standardized)
# Count for Brooklyn dropped from 3089 to 2723 to 1579 (1144 standardized)
# Count for Queens dropped from 2710 to 2610 to 1785 (825 standardized)
# Count for Staten Island dropped from 2373 to 2221 to 2019 (192 + 10 standardized)




####
########
############ @@@

### This code is very important as last part of BBL street standardisation!
# 2133 BBL with more than 1 street name for Manhattan after 2015-1-1
df %>%
  filter(BOROUGH == 'Manhattan' & INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")) %>%
  group_by(BBL) %>%
  distinct(STREET_NAME) %>%
  summarise(count_distinct = n()) %>%
  filter(count_distinct > 1)

# 127 BBL with more than 1 zip codes for Manhattan after 2015-1-1
df %>%
  filter(BOROUGH == 'Manhattan' & INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")) %>%
  group_by(BBL) %>%
  distinct(ZIP_CODE) %>%
  summarise(count_distinct = n()) %>%
  filter(count_distinct > 1)



## LATEST INFO HERE!
# 1127571 rows
nrow(df[df$INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York"),])

# 290783 unique BBL in total
length(unique(df$BBL))

# 8544 unique BBL with more than 1 unique street name
df %>%
  group_by(BBL) %>%
  distinct(STREET_NAME) %>%
  summarise(count_distinct = n()) %>%
  filter(count_distinct > 1)

# 5226 unique BBL with more than 1 unique street name (DEAL WITH THIS)
df %>%
  filter(INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")) %>%
  group_by(BBL) %>%
  distinct(STREET_NAME) %>%
  summarise(count_distinct = n()) %>%
  filter(count_distinct > 1)


######################################################################################################

# Create another spare copy of data frame for resetting

df_copy17 <- data.frame(df)

# Reset data frame
df <- data.frame(df_copy17)

######################################################################################################


## Our plan is to use the mode street name to standardize the street name for the BBL identified
## We ignore BBL that are totally before 2015-1-1, around 3318 BBL be ignored


# Extract the 5226 bbl to a vector variable to use in for loop
bbl_vector <- df %>%
  filter(INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")) %>%
  group_by(BBL) %>%
  distinct(STREET_NAME) %>%
  summarise(count_distinct = n()) %>%
  filter(count_distinct > 1) %>%
  select(BBL)

# Obtain the real vector inside the tibble
bbl_vector <- bbl_vector[[1]]

# Check the size of the bbl vector
length(bbl_vector)


# Loop over 5226 BBL and reassign the street names with just the mode street name
for (bbl in bbl_vector) {
  uniq_st <- unique(df[df$BBL == bbl, 'STREET_NAME'])
  mode_st <- uniq_st[which.max(tabulate(match(df[df$BBL == bbl, 'STREET_NAME'], uniq_st)))]
  df[df$BBL == bbl, 'STREET_NAME'] <- mode_st
}




# (THIS SHOULD GET 3318 UNIQUE BBL COUNT) BUT INSTEAD I GET 2450!!!
df %>%
  filter(INSPECTION_DATE < as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")) %>%
  group_by(BBL) %>%
  distinct(STREET_NAME) %>%
  summarise(count_distinct = n()) %>%
  filter(count_distinct > 1)


##
## There are a total of 290783 unique BBL 
## But, there are 198345 BBL that are before 2015-1-1, potentially will be removed
## While there are 92438 BBL that are after 2015-1-1, which is what we want here
## However, we should check for zip codes as well, and remove them instead of imputing them if less rows
##


######################################################################################################

# Create another spare copy of data frame for resetting

df_copy18 <- data.frame(df)

# Reset data frame
df <- data.frame(df_copy18)

######################################################################################################

## Start with filtering rows with more than 1 zip codes, because most of them are messy
## For this problem, we'll do it based on job id, to preserve rows that are preserveable

# Count the current unique BBL
length(unique(df$BBL))  # 290783 

# Count the current unique job id
length(unique(df$JOB_ID))  # 1374004

# 288 BBL with more than 1 zip codes for Manhattan after 2015-1-1
# Only 0.1% of the total BBL in the dataset, very negligible
# It seems like most of them are accurate information but with uncertain zip code
# For the sake of progressing, we'll use the mode to standardize them here
df %>%
  filter(INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")  & BBL != 1000000000 & BBL != 3000000000) %>%
  group_by(BBL) %>%
  distinct(ZIP_CODE) %>%
  summarise(count_distinct = n()) %>%
  filter(count_distinct > 1)


# We saw that there are 2 BBL that are pretty messy, that is 1000000000 and 3000000000
# We will straight remove these rows, for now we will ignore them first. (286 BBL to standardize)
# Then, the rest of the BBL, we'll standardize them using the mode zip code


# Extract the 286 bbl to a vector variable to use in for loop
bbl_zc_vector <- df %>%
  filter(INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")  & BBL != 1000000000 & BBL != 3000000000) %>%
  group_by(BBL) %>%
  distinct(ZIP_CODE) %>%
  summarise(count_distinct = n()) %>%
  filter(count_distinct > 1) %>%
  select(BBL)

# Obtain the real vector inside the tibble
bbl_zc_vector <- bbl_zc_vector[[1]]

# Check the size
length(bbl_zc_vector)


# Loop over 286 BBL and reassign the zip codes with just the mode zip code
for (bbl in bbl_zc_vector) {
  uniq_zc <- unique(df[df$BBL == bbl, 'ZIP_CODE'])
  mode_zc <- uniq_zc[which.max(tabulate(match(df[df$BBL == bbl, 'ZIP_CODE'], uniq_zc)))]
  df[df$BBL == bbl, 'ZIP_CODE'] <- mode_zc
}


######################################################################################################

# Create another spare copy of data frame for resetting
### NOTE!!! df_copy19 is the data frame that hasn't have irrelevant rows (before 2015-1-1) dropped yet ###

df_copy19 <- data.frame(df)

# Reset data frame
df <- data.frame(df_copy19)

######################################################################################################


## Now for inspection date!
# There are some incorrect values to be corrected here, though only a few entries need to be corrected

# View these rows and check the count of rows
sum(df$INSPECTION_DATE >= as.POSIXct("2021-01-01 00:00:00", tz = "America/New_York"))

df[df$INSPECTION_DATE >= as.POSIXct("2021-01-01 00:00:00", tz = "America/New_York"), c("BBL", "BOROUGH", "BLOCK", "LOT", "STREET_NAME", "ZIP_CODE", "LATITUDE", 
                                                                                       "LONGITUDE", "JOB_ID", 'INSPECTION_DATE', 'APPROVED_DATE', 'JOB_PROGRESS')]


# Change the inspection date according to the approved date
for (row in row.names(df[df$INSPECTION_DATE >= as.POSIXct("2021-01-01 00:00:00", tz = "America/New_York"), ])) {
  insdate <- as.POSIXlt(df[row, 'INSPECTION_DATE'])
  appdate <- as.POSIXlt(df[row, 'APPROVED_DATE'])
  insdate$year <- appdate$year
  df[row, 'INSPECTION_DATE'] <- as.POSIXct(insdate)
}

# View the count of rows now and see, should be zero
sum(df$INSPECTION_DATE >= as.POSIXct("2021-01-01 00:00:00", tz = "America/New_York"))








### STEP 5 - FILTERING ROWS ###
# This step is done specifically based on Inspection Date column.
# Our data consists of some data up to 2010 which is 10 years ago, and we don't want outdated data.
# We want to filter data that is before 1st January, 2015, meaning that 2014 and before will be removed.
# 


######################################################################################################

# Create another spare copy of data frame for resetting
### NOTE! df_copy20 is the df that has just corrected dates after 2020, the 4 rows ###

df_copy20 <- data.frame(df)

# Reset data frame
df <- data.frame(df_copy20)

######################################################################################################


# Remove rows based on JOB_ID 
# Before this, let us check the JOB_ID & BBL, save them on variables for filtering
jobs_remove <- sort(unique(df$JOB_ID[df$INSPECTION_DATE < as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York")]))
bbl_remove <- c(1000000000, 3000000000, 2032310001)

# Check the size
length(jobs_remove)  # 560810
length(bbl_remove)  # 3


# Before removing rows, what is the dimension?
dim(df)  # 1895834 rows

# Before removing rows, how many unique BBL are there?
length(unique(df$BBL))  # 290783 BBL

# Before removing rows, how many unique JOB_Id are there?
length(unique(df$JOB_ID))  # 1374004 JOB_ID

### I roughly design a filter based on JOB_ID
# Add the removal of the 2 identified BBL above, as well as the zipcode value of 99999 (BBL 2032310001)
df <- df %>%
  filter((!JOB_ID %in% jobs_remove) & (!BBL %in% bbl_remove))



## Verify the properties of the new df
# Check the dimension 
dim(df)  # 1121212 rows

# Check the unique BBL
length(unique(df$BBL))  # 219096 BBL

# Check the unique JOB ID
length(unique(df$JOB_ID))  # 813173 JOB_ID

# Check the dates, see if any of them are before 2015-1-1
sum(df$INSPECTION_DATE >= as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York"))
sum(df$INSPECTION_DATE < as.POSIXct("2015-01-01 00:00:00", tz = "America/New_York"))  # zero rows


# Verify about the distinct street name/zip code for each BBL only has 1 entry
# The below code should return zero rows/BBL
df %>%
  group_by(BBL) %>%
  distinct(STREET_NAME) %>%
  summarise(count_distinct = n()) %>%
  filter(count_distinct > 1)

df %>%
  group_by(BBL) %>%
  distinct(ZIP_CODE) %>%
  summarise(count_distinct = n()) %>%
  filter(count_distinct > 1)


# Check the current empty count for each columns
colSums(is.na(df))



## One last thing to do before we move on is to convert the POSIXct date column to Date class
## This essentially strips away the time attribute, we just retain the date

# Convert using date function
df$INSPECTION_DATE <- date(df$INSPECTION_DATE)

# Check the class now
class(df$INSPECTION_DATE)









### STEP 6 - REMOVING IRRELEVANT COLUMNS ###
# We have a total of 8 possible columns to remove, so we'll examine and remove them.
# So, we target to have 12 remaining columns after this step.
# 

## (6) Boro code (7) Block (8) Lot
# We previously mentioned that these 3 columns derive the BBL column, so they are redundant
# Boro is represented by Borough column, while block and lot is not as meaningful as street name
# It is safe to say that we can remove these columns.
# WARNING: dropping them means eliminating them completely, please proceed with caution!


######################################################################################################

# Create another spare copy of data frame for resetting
### NOTE!!! df_copy21 is the data frame that hasn't have irrelevant columns dropped yet ###

df_copy21 <- data.frame(df)

# Reset data frame
df <- data.frame(df_copy21)

######################################################################################################

# Export the cleaned version 04 (df_copy21) dataframe to a csv file for conventional purpose in case we have to reset:

write.csv(df_copy21, "D:/Documents/UM Master of Data Science/WQD7001 Principles of Data Science/Group project/Rodent_Inspection_clean04.csv", 
          row.names = FALSE)

# Load clean01 data and convert some data types
df <- read.csv("D:/Documents/UM Master of Data Science/WQD7001 Principles of Data Science/Group project/Rodent_Inspection_clean04.csv", 
               header = TRUE, sep = ',', stringsAsFactors = FALSE)

######################################################################################################


# Assigning NULL to 3 columns and drop them
df$BORO_CODE <- NULL
df$BLOCK <- NULL
df$LOT <- NULL

# View the dimension and structure
dim(df); str(df)


## (9) House Number
# House number is not a meaningful column and impossible to aggregate for insights
# It is better to use street names and zip codes rather than house number
# Basically, we can remove this column directly.

# Assigning NULL to house number column and drop it
df$HOUSE_NUMBER <- NULL

# View the dimension and structure
dim(df); str(df)


## (12) X_COORD (13) Y_COORD
# I am not sure if we can use this to impute latitude and longitude
# But if there are not much imputation to be done and these coordinates are not much help, 
# we can just remove them.
# Before we remove them, we should cross check with corresponding lat/long column values.


# Since I have imputed latitude and longitude, i can just remove these xy coordinates
# Assigning NULL to coordinate columns and drop them
df$X_COORD <- NULL
df$Y_COORD <- NULL

# View the dimension and structure
dim(df); str(df)


## (19) Approved Date
# After discussing with groupmates, we think approved date is irrelevant to our problem and solution.
# We think that people are more interested in inspection date instead of approved date
# Partly because approved date does not tell much message about the rodent activities
# We can just drop this column safely.

# Assigning NULL to approved date column and drop it
df$APPROVED_DATE <- NULL

# View the dimension and structure
dim(df); str(df)


## (20) Location
# Location is derived from latitude and longitude, so it is also redundant.
# As long as we have complete latitude and longitude, we can plot map points without location values.
# Let's just drop this column then.

# Assigning NULL to approved date column and drop it
df$LOCATION <- NULL

# View the dimension and structure
dim(df); str(df)








### STEP 7 - FEATURE ENGINEERING ###

# We have 12 columns currently, but we think we can feature engineer a few more
# Date can be a good option for this, we can make new columns of day, month and year.
#


# Access the weekday (wday) attribute
weekdays(df$INSPECTION_DATE[1:5])

# Access the month (mon) attribute
months(df$INSPECTION_DATE[1:5])

# Access the quarter attribute
quarters(df$INSPECTION_DATE[1:5])

# Access the year, month, day inside the date format
as.integer(format(df$INSPECTION_DATE[1:5], format="%Y"))
as.integer(format(df$INSPECTION_DATE[1:5], format="%m"))
as.integer(format(df$INSPECTION_DATE[1:5], format="%d"))


######################################################################################################

# Create another spare copy of data frame for resetting
### NOTE!!! df_copy22 is the data frame that already filtered rows and dropped unwanted columns ###

df_copy22 <- data.frame(df)

# Reset data frame
df <- data.frame(df_copy22)

######################################################################################################


## After discussing with the group, we determine that we'll use year, month and day
# FYI, year and month are being treated as categorical variable in factor numbers format
df$YEAR <- as.factor(as.integer(format(df$INSPECTION_DATE, format="%Y")))

df$MONTH <- as.factor(as.integer(format(df$INSPECTION_DATE, format="%m")))

df$DAY <- as.integer(format(df$INSPECTION_DATE, format="%d"))


# Check the final dimension and structure
dim(df); str(df)

# Any empty columns? check again
colSums(is.na(df))

# Touch up one small mistake identified for street name!
df$STREET_NAME <- gsub("^A Greene ", "Greene ", df$STREET_NAME)



##############################################################################################
##############################################################################################

# Export the final cleaned dataframe to a csv file for conventional purpose in case we have to reset:

write.csv(df, "D:/Documents/UM Master of Data Science/WQD7001 Principles of Data Science/Group project/Rodent_Inspection_clean_R1.csv", 
          row.names = FALSE)

# Load clean01 data and convert some data types
df <- read.csv("D:/Documents/UM Master of Data Science/WQD7001 Principles of Data Science/Group project/Rodent_Inspection_clean_R1.csv", 
               header = TRUE, sep = ',', stringsAsFactors = FALSE)

df$INSPECTION_TYPE <- as.factor(df$INSPECTION_TYPE)
df$BOROUGH <- as.factor(df$BOROUGH)
df$INSPECTION_DATE <- as.Date(df$INSPECTION_DATE)
df$RESULT <- as.factor(df$RESULT)
df$YEAR <- as.factor(df$YEAR)
df$MONTH <- as.factor(df$MONTH)
df$DAY <- as.integer(df$DAY)

##############################################################################################
##############################################################################################



