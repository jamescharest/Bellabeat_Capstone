# Restart R from scratch
q()

# install the packages we know we will use for data manipulation

install.packages("tidyverse") #basic functions
install.packages("lubridate") #date library
install.packages("dplyr") #data manipulation
install.packages("janitor") #clean names

#install the  pacakges for data visualization
install.packages("ggplot2")
install.packages("ggpubr")

#load all our libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(janitor)
library(ggplot2)
library(ggpubr)

#Upload the data we are going to use as data files

dailyactivity <- read_csv("dailyActivity_merged.csv")

head(dailyactivity)
View(dailyactivity)

#Check for duplicates
sum(duplicated(dailyactivity))

#Change the format of activity date
dailyactivity$ActivityDate <- as.POSIXct(dailyactivity$ActivityDate, format="%m/%d/%Y") 

#Add a column for a day of the week
dailyactivity$weekday <- wday(dailyactivity$ActivityDate, label=TRUE)

#Add a column that shows weekday or weekend
dailyactivity <- dailyactivity %>% 
  mutate(Weekend = case_when(
    weekday == "Sun" ~ 'Weekend' 
    , weekday == "Sat" ~ 'Weekend'
    , TRUE ~ 'Weekday'
  ))

#Change the name of a column, so labels are consistent

dailyactivity <- dailyactivity %>% 
  rename(FairlyActiveDistance = ModeratelyActiveDistance)

#Examine this dataset a bit

summary(dailyactivity)

#Which day has the most records
ggplot(data = dailyactivity)+
  geom_bar(mapping=aes(x=weekday, fill=weekday))

ggplot(dailyactivity, aes(x=weekday, fill=..count..))+
  geom_bar(stat="count")+
  scale_fill_gradient(low="red", high="blue")+
  theme(legend.position = "none")+
  labs(title = "Records per day")


#facetwrap by ID to see how consistent it is for each person
ggplot(data = dailyactivity)+
  geom_bar(mapping=aes(x=weekday, fill=weekday))+
  facet_wrap(~Id)

ggplot(dailyactivity, aes(x=weekday, fill=..count..))+
  geom_bar(stat="count")+
  scale_fill_gradient(low="red", high="blue")+
  theme(legend.position = "none")+
  labs(title = "Records per day")+
  facet_wrap(~Id)+
  theme(axis.text.x = element_text(angle = 90))

#Seems to generally follow the same pattern, with greater usage/records in the middle of the week

#Trying to look just at the types of minutes

dailyminutetype <- select(dailyactivity, c("weekday", "VeryActiveMinutes", 
                   "FairlyActiveMinutes", "LightlyActiveMinutes",
                   "SedentaryMinutes"))

View(dailyminutetype)
head(dailyminutetype)

agg=aggregate(dailyminutetype,
              by = list(dailyminutetype$weekday),
              FUN = mean)

aggminutebyday <- select(agg, -c("weekday")) %>% 
  rename(Weekday = Group.1)

View(aggminutebyday)

#Goal here is to see if there is a major discrepancy between types of minutes by day

install.packages("reshape2")
library(reshape2)

MinutetypeAll <- melt(aggminutebyday,  id.vars = 'Weekday', variable.name = 'minutetype')

ggplot(MinutetypeAll, aes(fill=minutetype, y=value, x=Weekday)) + 
  geom_bar(position="dodge", stat="identity")

#The sedentary Minutes scews the chart a bit, so let's remove that column

aggminutebydayNS <- select(aggminutebyday, -c("SedentaryMinutes"))
View(aggminutebydayNS)

MinutetypeNS <- melt(aggminutebydayNS,  id.vars = 'Weekday', variable.name = 'minutetype')

ggplot(MinutetypeNS, aes(fill=minutetype, y=value, x=Weekday)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(MinutetypeNS, aes(fill=minutetype, y=value, x=Weekday)) + 
  geom_bar(position="stack", stat="identity")

#Seems to be no MAJOR difference between the days
#Does show that there are more nonsedentary minutes on Saturday than rest of days

#What type of users are wearing fitbit devices (More or less active?)
#First aggregate some data on the individuals

aggdailyactivity<- dailyactivity %>% 
  group_by(Id) %>% 
  summarize(Days=n_distinct(ActivityDate), AvgVAMinutes=mean(VeryActiveMinutes),
            AvgFAMinutes=mean(FairlyActiveMinutes), AvgLAMinutes=mean(LightlyActiveMinutes),
            AvgSMinutes=mean(SedentaryMinutes), AvgSteps=mean(TotalSteps))

#Determine how active the users are based on steps

aggdailyactivity<- aggdailyactivity %>% 
  mutate(usertype = case_when(AvgSteps < 5000 ~ "Inactive",
                              AvgSteps >= 5000 & AvgSteps < 7500 ~ "Lightly Active",
                              AvgSteps >= 7500 & AvgSteps < 10000 ~ "Fairly Active",
                              AvgSteps >= 10000 ~ "Very Active"))

#Determine percentage of user types

install.packages("scales")
library(scales)

percentusertype <- aggdailyactivity %>% 
  group_by(usertype) %>% 
  summarise(Number=n()) %>% 
  mutate(percent=Number/33) 

percentusertype <- percentusertype%>% 
  mutate(label=label_percent()(percentusertype$percent))

#Create the hated bar chart:

ggplot(percentusertype, aes(x="", y=percent, fill=usertype))+
  geom_bar(stat = "identity", width=1, color="white")+
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5))+
  labs(title = "Percentage of Users")

#No real difference in types of users who use fitbits


#Time to look at some of the breakdown of the daily data
#focus first on hourly intensities and hourly steps

hourlyintensities <- read_csv("hourlyIntensities_merged.csv")
hourlysteps <- read_csv("hourlySteps_merged.csv")

View(hourlyintensities)
str(hourlyintensities)
View(hourlysteps)
str(hourlysteps)

#Check for duplicates: 
sum(duplicated(hourlyintensities))
sum(duplicated(hourlysteps))

#Create separate columns for date and time
#First need to change to date time

hourlysteps$ActivityHour <- parse_date_time(hourlysteps$ActivityHour, 
                                                  '%m/%d/%Y %I:%M:%S %p')

hourlyintensities$ActivityHour <- parse_date_time(hourlyintensities$ActivityHour, 
                                                  '%m/%d/%Y %I:%M:%S %p')

#Now add separate date and time columns; only want hour

hourlysteps$Date <- as.Date(hourlysteps$ActivityHour)
hourlysteps$Time <- format(as.POSIXct(hourlysteps$ActivityHour),
                           format = '%H')

hourlyintensities$Date <- as.Date(hourlyintensities$ActivityHour)
hourlyintensities$Time <- format(as.POSIXct(hourlyintensities$ActivityHour),
                           format = '%H')

#Combine the two data sets into one

hourlydf <- full_join(hourlyintensities, hourlysteps, 
                      by = c("Id" = "Id", "ActivityHour" = "ActivityHour", 
                             "Date" = "Date", "Time" = "Time"))
#Simplify the df

hourlydf <- select(hourlydf, Id, Date, Time, TotalIntensity, AverageIntensity, StepTotal)
View(hourlydf)

summary(hourlydf)

#When are people most active?

ggplot(data = hourlydf)+
  geom_point(mapping=aes(x=Time, y=TotalIntensity))

ggplot(data = hourlydf)+
  geom_point(mapping=aes(x=Time, y=StepTotal))

ggplot(data = hourlydf)+
  geom_point(mapping=aes(x=Time, y=AverageIntensity))

#Looking at aggregated data only:
aggtime=aggregate(hourlydf,
              by = list(hourlydf$Time),
              FUN = mean)
aggtime <- select(aggtime, -c("Id", "Date", "Time")) %>% 
  rename(Time = Group.1)
summary(aggtime)

ggplot(aggtime, aes(x=Time, y=StepTotal, fill=StepTotal))+
  geom_bar(stat = "identity")+
  scale_fill_gradient(low="red", high="blue")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position = "none")+
  labs(title="Average Steps per Hour")

ggplot(aggtime, aes(x=Time, y=TotalIntensity, fill=TotalIntensity))+
  geom_bar(stat = "identity")+
  scale_fill_gradient(low="red", high="blue")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position = "none")+
  labs(title = "Average Intensity per Hour")



#Is there a difference betweew weekday and weekend?
#Add a column for a day of the week
hourlydf$weekday <- wday(hourlydf$Date, label=TRUE)

#Add a column that shows weekday or weekend
hourlydf <- hourlydf %>% 
  mutate(Weekend = case_when(
    weekday == "Sun" ~ 'Weekend' 
    , weekday == "Sat" ~ 'Weekend'
    , TRUE ~ 'Weekday'
  ))

hourlydfweekday <- hourlydf %>% 
  filter(Weekend == "Weekday")

aggweekday=aggregate(hourlydfweekday,
                  by = list(hourlydfweekday$Time),
                  FUN = mean)
aggweekday <- select(aggweekday, -c("Id", "Date", "Time", "weekday", "Weekend")) %>% 
  rename(Time = Group.1)

hourlydfweekend <- hourlydf %>% 
  filter(Weekend == "Weekend")

aggweekend=aggregate(hourlydfweekend,
                     by = list(hourlydfweekend$Time),
                     FUN = mean)
aggweekend <- select(aggweekend, -c("Id", "Date", "Time", "weekday", "Weekend")) %>% 
  rename(Time = Group.1)

summary(aggweekday)
summary(aggweekend)

#Visualizing it

install.packages("patchwork")
library(patchwork)

WE <- ggplot(aggweekend, aes(x=Time, y=TotalIntensity, fill=TotalIntensity)) + 
  geom_bar(stat = "identity")+
  scale_fill_gradient(low="red", high="blue")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position = "none")+
  labs(title = "Weekends")

WD <- ggplot(aggweekday, aes(x=Time, y=TotalIntensity, fill=TotalIntensity)) + 
  geom_bar(stat = "identity")+
  scale_fill_gradient(low="red", high="blue")+
  theme(axis.text.x = element_text(angle = 90))+
  annotate("rect", xmin = 09, xmax = 17, ymin = 0, ymax = Inf,
           alpha = .1,fill = "blue")+
  theme(legend.position = "none")+
  labs(title = "Weekdays")


WD / WE 

#On weekdays, there is a spike in intensity after the conclusion of the workday

#Looking at sleep data
sleepday <- read_csv("sleepDay_merged.csv")

#Check for Duplicates
sum(duplicated(sleepday))

#Remove Duplicates
sleepday <- sleepday %>%
  distinct()
sum(duplicated(sleepday))

#Also adding weekday/weekend
sleepday$SleepDay <- as.POSIXct(sleepday$SleepDay, format="%m/%d/%Y")
sleepday$Weekday <- wday(sleepday$SleepDay, label=TRUE)
sleepday <- sleepday %>% 
  mutate(Weekend = case_when(
    Weekday == "Sun" ~ 'Weekend' 
    , Weekday == "Sat" ~ 'Weekend'
    , TRUE ~ 'Weekday'
  ))

View(sleepday)

aggsleepday <- sleepday %>% 
  group_by(Id) %>% 
  summarise(Records=n_distinct(SleepDay),
            AvgSleepRecords = mean(TotalSleepRecords),
            AvgMinutesAsleep = mean(TotalMinutesAsleep))

#Any correlation between number of records and sleep time?

ggplot(aggsleepday, aes(x=Records, y=AvgMinutesAsleep))+
  geom_point()+
  geom_smooth()