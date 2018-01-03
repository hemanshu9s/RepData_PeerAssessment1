#Hemanshu Singh#

#1. Code for reading in the dataset and/or processing the data
setwd("C:/Personal/coursera-data-science/5-Reproducible-Research/Project1")
getwd()
# set the file url 
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

# create the placeholder file
tf = tempfile(tmpdir=td, fileext=".zip")

# download into the placeholder file 
download.file(fileurl, tf)

# get the name of the first file in the zip archive
fname = unzip(tf, list=TRUE)$Name[1]

# unzip the file to the temporary directory
unzip(tf, files=fname, exdir="C:/Personal/coursera-data-science/5-Reproducible-Research/Project1", overwrite=TRUE)

# fpath is the full path to the extracted file
fpath = file.path("C:/Personal/coursera-data-science/5-Reproducible-Research/Project1", fname)

# load the csv in data frame
df <- read.csv(fpath, as.is=TRUE)


#2. Histogram of the total number of steps taken each day
# generate df2 with complete cases only
df2 <- na.omit(df)

# aggregate steps as per date to get total number of steps in a day
table_date_steps <- aggregate(steps ~ date, df2, sum)

## create histogram of total number of steps in a day
hist(table_date_steps$steps, col=1, main="Histogram of total number of steps per day", 
     xlab="Total number of steps in a day")

#3. mean and median total number of steps per day
mean(table_date_steps$steps)
median(table_date_steps$steps)


#4. Time series plot of the average number of steps taken
table_interval_steps <- aggregate(steps ~ interval, df2, mean)

#5. The 5-minute interval that, on average, contains the maximum number of steps
# generate the line plot of the 5-minute interval (x-axis) and the average number of 
# steps taken, averaged across all days (y-axis)
plot(table_interval_steps$interval, table_interval_steps$steps, type='l', col=1, 
     main="Average number of steps averaged over all days", xlab="Interval", 
     ylab="Average number of steps")

#6. Code to describe and show a strategy for imputing missing data
# find row id of maximum average number of steps in an interval
max_ave_steps_row_id <- which.max(table_interval_steps$steps)

# get the interval with maximum average number of steps in an interval
table_interval_steps [max_ave_steps_row_id, ]

# get rows with NA's
df_NA <- df[!complete.cases(df),]

# number of rows
nrow(df_NA)

# perform the imputation
for (i in 1:nrow(df)){
        if (is.na(df$steps[i])){
                interval_val <- df$interval[i]
                row_id <- which(table_interval_steps$interval == interval_val)
                steps_val <- table_interval_steps$steps[row_id]
                df$steps[i] <- steps_val
        }
}

# aggregate steps as per date to get total number of steps in a day
table_date_steps_imputed <- aggregate(steps ~ date, df, sum)

#Histogram of the total number of steps taken each day after missing values are imputed
hist(table_date_steps_imputed$steps, col=1, main="(Imputed) Histogram of total number of steps per day", xlab="Total number of steps in a day")


# get mean and median of total number of steps per day
mean(table_date_steps_imputed$steps)
median(table_date_steps_imputed$steps)
# get mean and median of total number of steps per day for data with NA's removed
mean(table_date_steps$steps)
median(table_date_steps$steps)


# convert date from string to Date class
df$date <- as.Date(df$date, "%Y-%m-%d")

# add a new column indicating day of the week 
df$day <- weekdays(df$date)

# add a new column called day type and initialize to weekday
df$day_type <- c("weekday")

# If day is Saturday or Sunday, make day_type as weekend
for (i in 1:nrow(df)){
        if (df$day[i] == "Saturday" || df$day[i] == "Sunday"){
                df$day_type[i] <- "weekend"
        }
}

# convert day_time from character to factor
df$day_type <- as.factor(df$day_type)

# aggregate steps as interval to get average number of steps in an interval across all days
table_interval_steps_imputed <- aggregate(steps ~ interval+day_type, df, mean)

#8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
# make the panel plot for weekdays and weekends
library(ggplot2)

qplot(interval, steps, data=table_interval_steps_imputed, geom=c("line"), xlab="Interval", 
      ylab="Number of steps", main="") + facet_wrap(~ day_type, ncol=1)

# remove the data frames to free memory
rm(df, df2, table_date_steps, table_interval_steps, df_NA, table_date_steps_imputed, 
   table_interval_steps_imputed)