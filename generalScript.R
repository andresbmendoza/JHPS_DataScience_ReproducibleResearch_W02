#  Loading Libraries
library(readr)
library(dplyr)
library(ggplot2)

#  Load Files:

rawData <- read.csv("Data/activity.csv")                                        # Load csv file from relative directory "Data" in the Working directory

#  2- Histogram of the total number of steps taken each day ##

DaylySteps <- rawData %>%                                                       # Create a DF with summarized Steps each day
  group_by(Date = date) %>% 
  summarize(Steps = sum(steps, na.rm = TRUE))

ggplot(DaylySteps, aes(x= Steps)) +                                             # Create an histogram with ggplot
  geom_histogram(binwidth = 1000, fill = "steelblue", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(x = Steps)), color = "red")+
  geom_vline(aes(xintercept = median(x = Steps)), color = "blue")

print(paste("The Mean in red line above  is: ",                                 # Calc the mean an median to compare
            as.character(round(mean(DaylySteps$Steps), 3)),
            ", The Median in blue line above is:",
            as.character(round(median(DaylySteps$Steps), 3))))

##============================================================================##
#  3- Mean and median number of steps taken each day
CentralMeasures1 <- rawData %>%                                                 # Create a DF with summarized Steps each day
  group_by(date = date) %>% 
  summarize( mean.step = mean(steps, na.rm = TRUE),
             median.step = median(steps, na.rm = TRUE))

CentralMeasures1 <- as.data.frame(CentralMeasures1)
CentralMeasures1$date <- as.Date(CentralMeasures1$date)

ggplot(data= na.omit(CentralMeasures1), aes(x = date, y = mean.step)) +
  geom_area(fill = "steelblue", alpha = 0.5) +
  xlab("date in 2012") +
  ylab("Dayly average number of steps taken each 5 minutes") +
  ggtitle("Average stimated steps measured each 5-minutes interval \n per day")+
  theme(plot.title = element_text(hjust = 0.5))
  

##============================================================================##
#  4- Time series plot of the average number of steps taken

CentralMeasures <- rawData %>%                                                  # Create a DF with summarized Steps each day
  group_by(Interval = interval) %>% 
  summarize( mean.step = mean(steps, na.rm = TRUE),
             median.step = median(steps, na.rm = TRUE))
ggplot(data=CentralMeasures, aes(x = Interval, y = mean.step)) +
  geom_area(fill = "steelblue", alpha = 0.5) +
  xlab("5-minute interval") +
  ylab("average number of steps taken") +
  ggtitle("Pattern of the average steps dayly by 5-minutes interval") +
  theme(plot.title = element_text(hjust = 0.5))

##============================================================================##
#  5- The 5-minute interval that, on average, contains the maximum number of steps

CentralMeasures[which.max(CentralMeasures$median.step),]

##============================================================================##
#  6- Code to describe and show a strategy for imputing missing data
k <- round(sum(is.na(rawData$steps))/length(rawData$steps)*100,2)
print(paste("NAs Values represent the", as.character(k), "% of the total data"))

# about 13.11% of the data are NA values, that is significatively big to biases
# the central measures as mean, median and mode.
# one way to impute is assign the mean data value to the NA.

rawData2 <- rawData 
mean.val <- mean(rawData2$steps, na.rm = TRUE)
# firs will identify the NAs Position, creating a logical vector where Na is true:
for (k in 1:length(rawData2$steps)) {                                           # Check each observation
  if (is.na(rawData2$steps[k])) rawData2$steps[k] <- mean.val                   # subs if is a NA value for the mean od the data
}

##============================================================================##
# 7- Histogram of the total number of steps taken each day after missing values
#    are imputed

DaylySteps2 <- rawData2 %>%                                                      # Create a DF with summarized Steps each day
  group_by(Date = date) %>% 
  summarize(Steps = sum(steps, na.rm = TRUE))

ggplot(DaylySteps2, aes(x= Steps)) +                                            # Create an histogram with ggplot
  geom_histogram(binwidth = 1000,
               fill = "steelblue", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(x = Steps)), color = "red") +
  geom_vline(aes(xintercept = mean(x = Steps)), color = "blue", lty = 4)

print(paste("The Mean in red line above  is: ",
            as.character(round(mean(DaylySteps2$Steps), 3)),
            ", The Median in blue line above is:",
            as.character(round(median(DaylySteps2$Steps), 3))))

##============================================================================##
# 8- Panel plot comparing the average number of steps taken per 5-minute
#    interval across weekdays and weekends


is.weekday <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("Weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("Weekend")
  else
    stop("wrong data date")
}

Data <- rawData2

#Data$date <- as.Date(Data$date)

Data$day <- sapply(as.Date(Data$date), FUN = is.weekday)

Data$day <- as.factor(Data$day)

CentralMeasures2 <- Data %>%                                                 # Create a DF with summarized Steps each day
  group_by(interval, day) %>% 
  summarize( mean = mean(steps) )

ggplot(data=CentralMeasures2,
       aes(x=CentralMeasures2$interval,
           y = CentralMeasures2$mean,
           fill = CentralMeasures2$day)) +
  geom_area(alpha = 0.6) +
  scale_fill_manual(values = c("darkblue","steelblue")) +
  xlab("5-minute interval") +
  ylab("average number of steps taken") +
  ggtitle("Pattern of the average steps on Weeksday or Weekend \n by 5-minutes interval") +
  guides(fill = guide_legend(title = "")) +
  theme(plot.title = element_text(hjust = 0.5))


