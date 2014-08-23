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
debug <- FALSE
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

event_str <- new.env(hash=T, parent=emptyenv())
event_str$tornado <- 'tornado'
event_str$heat <- 'heat'
event_str$drought <- '^drought$'
event_str$wind <- 'wind'
event_str$flood <- 'flood'
event_str$lightning <- 'lightning'
event_str$ice <- 'ice'
event_str$current <- 'current|surf'
event_str$hail <- 'hail'
event_str$hurricane <- 'hurricane|tropical'
event_str$snow <- 'blizzard|snow|winter'
event_str$fog <- 'fog'
event_str$fire <- 'fire'
event_str$dust_storm <- 'dust storm'
event_str$avalanche <- 'avalanche'
event_str$cold <- 'cold'
event_str$rain <- 'rain'
event_str$surge <- 'surge'
event_str$frost <- 'frost'

# Answer question 1
total_injuries <- aggregate(raw_data$INJURIES ~ raw_data$EVTYPE, raw_data, sum)
colnames(total_injuries)[1] <- 'event'
colnames(total_injuries)[2] <- 'injury'

total_fatalities <- aggregate(raw_data$FATALITIES ~ raw_data$EVTYPE, raw_data, sum)
colnames(total_fatalities)[1] <- 'event'
colnames(total_fatalities)[2] <- 'fatality'

total_human_loss <- data.frame(event=total_fatalities$event)
total_human_loss$human.loss <- total_fatalities$fatality + total_injuries$injury

sorted_human_loss <- total_human_loss[order(total_human_loss$human.loss, decreasing=TRUE),]


event <- character(0)
loss <- numeric(0)
i <- 0
for(e_str in ls(event_str))
{ 
    event[i] <- e_str
    loss[i] <- sum( sorted_human_loss[grep(event_str[[e_str]], sorted_human_loss$event, ignore.case=TRUE), ]$human.loss )
    i <- i + 1
}

filtered_human_loss <- data.frame(event=event, loss=loss)
filtered_human_loss <- filtered_human_loss[order(filtered_human_loss$loss, decreasing=TRUE),]
final_human_loss <- data.frame(event=filtered_human_loss$event, loss=filtered_human_loss$loss)

plotting_data <- final_human_loss$loss[1:10]
plotting_labels <- final_human_loss$event[1:10]
barplot(plotting_data, names.arg=plotting_labels, xlab="Weather Events", ylab="Total Human Casualties", main="Total Human Casualties per Type of Weather Event", yaxp=c(0,100000,5))

# Answer question 2
calculate_damage <- function(damage)
{
    multiplier <- 1
    value <- as.numeric(damage[1])
    exponent <- damage[2]
     
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

raw_property_damage <- raw_data[, c('PROPDMG', 'PROPDMGEXP')]
na_rows <- which(is.na(raw_property_damage$PROPDMGEXP) == TRUE)
raw_property_damage$PROPDMGEXP[na_rows] <- '-'
calculated_property_damage <- apply(raw_property_damage, 1, calculate_damage)

property_damage <- data.frame(event=raw_data$EVTYPE)
property_damage$dmg.value <- calculated_property_damage
colnames(property_damage)[2] <- 'property.damage'


raw_crop_damage <- raw_data[, c('CROPDMG', 'CROPDMGEXP')]
na_rows <- which(is.na(raw_crop_damage$CROPDMGEXP) == TRUE)
raw_crop_damage$CROPDMGEXP[na_rows] <- '-'
calculated_crop_damage <- apply(raw_crop_damage, 1, calculate_damage)

crop_damage <- data.frame(event=raw_data$EVTYPE)
crop_damage$dmg.value <- calculated_crop_damage
colnames(crop_damage)[2] <- 'crop.damage'

economic_loss <- data.frame(event=property_damage$event)
economic_loss$dollars <- property_damage$property.damage + crop_damage$crop.damage

total_economic_loss <- aggregate(economic_loss$dollars ~ economic_loss$event, economic_loss, sum)
colnames(total_economic_loss)[1] <- 'event'
colnames(total_economic_loss)[2] <- 'dollars'

sorted_economic_loss <- total_economic_loss[order(total_economic_loss$dollars, decreasing=TRUE),]

event <- character(0)
loss <- numeric(0)
i <- 0
for(e_str in ls(event_str))
{ 
    event[i] <- e_str
    loss[i] <- sum( sorted_economic_loss[grep(event_str[[e_str]], sorted_economic_loss$event, ignore.case=TRUE), ]$dollars )
    i <- i + 1
}

filtered_economic_loss <- data.frame(event=event, loss=loss)
filtered_economic_loss <- filtered_economic_loss[order(filtered_economic_loss$loss, decreasing=TRUE),]
final_economic_loss <- data.frame(event=filtered_economic_loss$event, loss=filtered_economic_loss$loss)

plotting_data <- final_economic_loss$loss[1:10]
plotting_labels <- final_economic_loss$event[1:10]
barplot(plotting_data, names.arg=plotting_labels, xlab="Weather Events", ylab="Total Cost (dollars)", main="Total Ecomonic Damage per Type of Weather Event", yaxp=c(0,180000000000,5))











