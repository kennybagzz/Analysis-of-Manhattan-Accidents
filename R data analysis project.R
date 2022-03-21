library(tidyverse)
library(htmlwidgets)
library(lubridate)

#load dataset
nyc_crashes = read.csv("Motor_Vehicle_Collisions_-_Crashes.csv")

#filter dataset to only manhattan
manhattan_crashes = nyc_crashes[nyc_crashes$BOROUGH == "MANHATTAN",]

#then filter dataset for accidents from 2013-2021 since reports from 2012 and 2022 is incomplete
manhattan_crashes$CRASH.DATE = as.Date(manhattan_crashes$CRASH.DATE, "%m/%d/%Y")
manhattan_crashes13_21 = filter(manhattan_crashes, CRASH.DATE > as.Date("2012-12-31") & CRASH.DATE < as.Date("2022-01-01"))

#get rid of any rows with NA
manhattan_crashes13_21 = manhattan_crashes13_21[manhattan_crashes13_21$LATITUDE > 0,]

#convert crash.time column from char to time
manhattan_crashes13_21[['CRASH.TIME']] <- as.POSIXct(manhattan_crashes13_21[['CRASH.TIME']],format = "%H:%M")

#add a year, month, day of the week column for analysis later
manhattan_crashes13_21[, "year"] = format(manhattan_crashes13_21[, "CRASH.DATE"], "%Y")
manhattan_crashes13_21[, "month"] = format(manhattan_crashes13_21[, "CRASH.DATE"], "%m")
manhattan_crashes13_21$weekday <- wday(manhattan_crashes13_21$CRASH.DATE, label=TRUE, abbr=FALSE)

#find out how many accidents a year
manhattan_crashes_by_year = manhattan_crashes13_21 %>% count(year)

#use a line graph to visualize
ggplot(data=manhattan_crashes_by_year, aes(x=year, y=n, group=1)) +
  geom_line()+
  geom_point()+
  ggtitle("Number of accidents per year in Manhattan")

#find out how many accidents each month
manhattan_crashes_by_month = manhattan_crashes13_21 %>% count(month)
#use line graph to visualize
ggplot(data=manhattan_crashes_by_month, aes(x=month, y=n, group=1)) +
  geom_line()+
  geom_point()+
  ggtitle("Number of accidents per month in Manhattan")

#find out how many accidents for each day of the week
manhattan_crashes_by_day = manhattan_crashes13_21 %>% count(weekday)
ggplot(data=manhattan_crashes_by_day, aes(x=weekday, y=n, group=1)) +
  geom_line()+
  geom_point() +
  ggtitle("Number of accidents by day of week in Manhattan")
manhattan_crashes_by_day

#analysis by 1 hr intervals
manhattan_crashes_by_hr = manhattan_crashes13_21 %>% count(CRASH.TIME)

#propotion of accidents with just property damage, proportion with at least 1 injured, no fatalities, proportion of at least 1 killed
manhattan_accident_proportions = manhattan_crashes13_21[manhattan_crashes13_21$NUMBER.OF.PERSONS.KILLED> 0,]

#map of hotzones of accidents. mean of latitude and longitude. one std of both and map it.

#map the min and max of latitude and longitude too
summarise(manhattan_crashes13_21, latitude_min = min(LATITUDE))
manhattan_crashes13_21$LATITUDE

#I have highlighted the intersections that have witnessed at least one accident per week on average
