# -----------------------------------------------------------------------------------------------
#
#   File: data_analysis.R
#   Author: Fredrick Stakem
#   Created: 8.19.14
#
#   Purpose: The purpose of this code is explore the data set given for project 2 
#            in the Coursera class 'Reproducable Research' from John Hopkins.
#
# -----------------------------------------------------------------------------------------------


# Question 1
#   Across the United States, which types of events (as indicated in the EVTYPE variable) 
#   are most harmful with respect to population health?


# Question 2
#   Across the United States, which types of events have the greatest economic consequences?

# Libraries
# If these are not installed run the following commands on each of them
#   - install.packages("abc")
#   - library(abc)
#
#   1) "R.utils"


# Parameters
debug <- TRUE
working_dir <- '~/projects/CourseraDataScience/projects/reproducible_research/project_2/src/'
data_url <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
download_path <- '../raw_data/storm.csv.bz2'
data_file <- '../raw_data/storm.csv'
debug_data_file <- '../raw_data/debug_storm.csv'

# Setup
setwd(working_dir)

# Get data
if(!file.exists(data_file))
{
    if(!file.exists(download_path))
    {
        download.file(data_url, download_path, method='curl')
    }
    
    bunzip2(download_path, remove=FALSE)
}

if(debug)
{
    if(!file.exists(debug_data_file))
    {
        stop()
    }
    
    raw_data <- read.table(debug_data_file, fill=TRUE, sep=',', header=TRUE)
} else
{
    raw_data <- read.table(data_file, fill=TRUE, sep=',', header=TRUE)
}

# Answer question 1
total_injuries <- aggregate(raw_data$INJURIES ~ raw_data$EVTYPE, raw_data, sum)
colnames(total_injuries)[1] <- 'event'
colnames(total_injuries)[2] <- 'injury'

total_fatalities <- aggregate(raw_data$FATALITIES ~ raw_data$EVTYPE, raw_data, sum)
colnames(total_fatalities)[1] <- 'event'
colnames(total_fatalities)[2] <- 'fatality'

total_human_loss <- data.frame(event=total_fatalities$event)
total_human_loss$human.loss <- total_fatalities$fatality + total_injuries$injury

# Answer question 2
sum_property_damage <- function(value, exponent)
{
    multiplier <- 1
    if(exponent == 'K')
    {
        multiplier <- 1000
    } else if(exponent == 'M')
    {
        multiplier <- 1000000
    } else if(exponent == 'B')
    {
        multiplier <- 1000000000
    }
    
    value * multiplier
}


#property_damage <- data.frame(event=raw_data$EVTYPE)
#property_damage$dmg.value <-

#total_prop_dmg <- aggregate(raw_data$PROPDMG ~ raw_data$EVTYPE, raw_data, sum)
#colnames(total_prop_dmg)[1] <- 'event'
#colnames(total_prop_dmg)[2] <- 'propdmg'

#summary(total_prop_dmg)









