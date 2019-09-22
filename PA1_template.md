Read In Libraries
=================

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(ggplot2)

Read In Data
============

    ActivityData<-read.csv("activity.csv")

What is the mean number of steps taken per day?
===============================================

      #Sum the amount of steps each day
      ActivityData_by_Date<- ActivityData %>%
        group_by(factor(date)) %>%
        summarise(total.steps = sum(steps, na.rm=T))

      #Create Histogram of the Total Steps taken each Day
      ggplot(ActivityData_by_Date, aes(total.steps) ) + 
        geom_histogram(bins = 20) +
        labs(x = "Total Steps Each Day", y = "Count of Days", title="Histogram of Total Steps Each Day")

![](PA1_template_files/figure-markdown_strict/TotalSteps-1.png)

      meanSteps<-round(mean(ActivityData_by_Date$total.steps), digits = 0)
      medianSteps<-round(median(ActivityData_by_Date$total.steps), digits=0)

The mean amount of steps taken per day is 9354. The median amount of
steps taken per day is 1.039510^{4}.

What is the average daily activity pattern?
===========================================

      #Average amount of steps in each interval
      ActivityData_by_Interval<- ActivityData %>%
        group_by(interval) %>%
        summarise(average.steps = mean(steps, na.rm=T))

      #Create Line graph for the average amount of steps taken in each 5-min interval
      ggplot(ActivityData_by_Interval, aes(y=average.steps, x=interval) ) + 
        geom_line() +
        labs(x = "5-min Interval", y = "Average Steps Taken", title="Average Steps Taken for each Interval")

![](PA1_template_files/figure-markdown_strict/AverageStepsPerInterval-1.png)

      #Interval with maximum number of steps
      max_Steps<-ActivityData_by_Interval[which.max(ActivityData_by_Interval$average.steps),]

The 835 interval contains the maximum amount of steps on average.

Imputing Missing Values
=======================

      #Count of missing values in data set
      Count_NA <- sum(is.na(ActivityData$steps))

There are 2304 intervals with missing step counts.

      #Identify Rows missing step values
      ActivityData_OnlyNA <- ActivityData[is.na(ActivityData$steps),]

      #Merge Intervals with missing step values to the interval average steps
      ActivityData_ReplacedNA <- merge(ActivityData_OnlyNA, ActivityData_by_Interval, by=c("interval"))
      ActivityData_ReplacedNA <- ActivityData_ReplacedNA[c("average.steps", "date", "interval")]
      names(ActivityData_ReplacedNA) <- c("steps", "date", "interval")
      
      #Data set without missing step values
      ActivityData_NoNA <- ActivityData[is.na(ActivityData$steps)==FALSE,]
      
      #Bind data with replaced NAs and without NAs
      ActivityData_NoNA <- rbind(ActivityData_NoNA, ActivityData_ReplacedNA)
      
      
      ActivityData_NoNA_by_Date<- ActivityData_NoNA %>%
      group_by(factor(date)) %>%
      summarise(total.steps = sum(steps, na.rm=T))

      #Create Histogram of the Total Steps taken each Day
      ggplot(ActivityData_NoNA_by_Date, aes(total.steps) ) + 
        geom_histogram(bins = 20) +
        labs(x = "Total Steps Each Day", y = "Count of Days", title="Histogram of Total Steps Each Day (NA values replaced with interval mean)")

![](PA1_template_files/figure-markdown_strict/Histogram%20with%20NA%20replaced%20with%20interval%20mean-1.png)

      NoNAMean <- round(mean(ActivityData_NoNA_by_Date$total.steps), digit=0)
      NoNAMedian <- round(median(ActivityData_NoNA_by_Date$total.steps), digit = 0)
      
      MeanDiff <- NoNAMean - meanSteps
      MedianDiff <- NoNAMedian - medianSteps

The mean and median is shifted to the right when imputing the missing
values with the mean of the interval. The mean shifts from 9354 to
1.076610^{4} a difference of 1412. The median shifts from 1.039510^{4}
to 1.076610^{4} a difference of 371.

Average Steps by Weekday versus Weekend
=======================================

      ActivityData_NoNA$AsDate <- as.Date(ActivityData_NoNA$date, format = "%Y-%m-%d")
      
      ActivityData_NoNA$Weekday <- ifelse(weekdays(ActivityData_NoNA$AsDate) == 'Saturday' | weekdays(ActivityData_NoNA$AsDate) == 'Sunday', 'weekend', 'weekday')
      
      ActivityData_NoNA_by_Weekday<- ActivityData_NoNA %>%
        group_by(interval, Weekday) %>%
        summarise(average.steps = mean(steps, na.rm=T))
      
      ggplot(ActivityData_NoNA_by_Weekday, aes(interval, average.steps)) +
        geom_line()+ggtitle("Average Steps per Interval for Weekdays versus Weekends")+
        xlab("Interval") + ylab("Average Amount of Steps") +facet_grid(Weekday~.)

![](PA1_template_files/figure-markdown_strict/NoNA%20data%20set%20with%20weekday%20and%20weekend%20identifier-1.png)
