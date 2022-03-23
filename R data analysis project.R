library(tidyverse)
library(htmlwidgets)
library(lubridate)
library(rmarkdown)
library(ggplot2)
library(dplyr)
library("ggmap")

#function to convert number into percentage
percent <- function(x, digits = 2, format = "f", ...) {      
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}

#load dataset
nyc_crashes = read.csv("Motor_Vehicle_Collisions_-_Crashes.csv") 
#filter dataset to only manhattan
manhattan_crashes = nyc_crashes[nyc_crashes$BOROUGH == "MANHATTAN",]
#then filter dataset for accidents from 2013-2021 since reports from 2012 and 2022 is incomplete
manhattan_crashes$CRASH.DATE = as.Date(manhattan_crashes$CRASH.DATE, "%m/%d/%Y")
cleaned_manhattan_crashes = filter(manhattan_crashes, CRASH.DATE > as.Date("2012-12-31") & CRASH.DATE < as.Date("2022-01-01"))





#add a year, month, day of the week column for analysis later
cleaned_manhattan_crashes[, "year"] = format(cleaned_manhattan_crashes[, "CRASH.DATE"], "%Y")
cleaned_manhattan_crashes[, "month"] = format(cleaned_manhattan_crashes[, "CRASH.DATE"], "%m")
cleaned_manhattan_crashes$weekday <- wday(cleaned_manhattan_crashes$CRASH.DATE, label=TRUE, abbr=FALSE)

#find out how many accidents a year
manhattan_crashes_by_year = cleaned_manhattan_crashes %>% count(year)
#use a line graph to visualize
accidents_per_yr_graph = ggplot(data=manhattan_crashes_by_year, aes(x=year, y=n, group=1)) +
  geom_line()+
  geom_point()+
  ggtitle("Number of accidents per year in Manhattan")
accidents_per_yr_graph



#find out how many accidents each month
manhattan_crashes_by_month = cleaned_manhattan_crashes %>% count(month)
#use line graph to visualize
accidents_per_month_graph = ggplot(data=manhattan_crashes_by_month, aes(x=month, y=n, group=1)) +
  geom_line()+
  geom_point()+
  ggtitle("Number of accidents per month in Manhattan")
accidents_per_month_graph



#find out how many accidents for each day of the week
manhattan_crashes_by_day = cleaned_manhattan_crashes%>% count(weekday)
accidents_per_day_graph = ggplot(data=manhattan_crashes_by_day, aes(x=weekday, y=n, group=1)) +
  geom_line()+
  geom_point() +
  ggtitle("Number of accidents by day of week in Manhattan")
accidents_per_day_graph



#analysis by 1 hr intervals
cleaned_manhattan_crashes$floor_time <- as.POSIXlt(cleaned_manhattan_crashes$CRASH.TIME,format = "%H:%M")
unclassed_floor_time = unclass(cleaned_manhattan_crashes$floor_time)
list_of_floor_times = unclassed_floor_time[3]
vector_of_floor_times = unlist(list_of_floor_times)
rm(unclassed_floor_time,list_of_floor_times)
cleaned_manhattan_crashes$floor_time = vector_of_floor_times


accidents_by_hour= cleaned_manhattan_crashes %>%
  group_by(group = cut(floor_time, breaks = seq(0, 25, 1))) %>%
  summarise(n = n())
sum(accidents_by_hour$n)

accidents_by_hour_graph = ggplot(data=accidents_by_hour, aes(x=group, y=n, group=1)) +
  geom_line()+
  geom_point() +
  ggtitle("People killed by hour in Manhattan")
accidents_by_hour_graph

#propotion of accidents with just property damage, proportion with at least 1 injured, no fatalities, proportion of at least 1 killed
accidents_people_killed = sum(cleaned_manhattan_crashes$NUMBER.OF.PERSONS.KILLED > 0,na.rm = TRUE)
accidents_nokilled_but_injured = sum(cleaned_manhattan_crashes$NUMBER.OF.PERSONS.INJURED > 0 & cleaned_manhattan_crashes$NUMBER.OF.PERSONS.KILLED == 0, na.rm = TRUE)
accidents_property_damage = sum(cleaned_manhattan_crashes$NUMBER.OF.PERSONS.INJURED == 0 & cleaned_manhattan_crashes$NUMBER.OF.PERSONS.KILLED == 0, na.rm = TRUE)

manhattan_crashes_by_year_killed = cleaned_manhattan_crashes %>% count(year, NUMBER.OF.PERSONS.KILLED > 0)
manhattan_crashes_by_year_killed = filter(manhattan_crashes_by_year_killed, `NUMBER.OF.PERSONS.KILLED > 0`  == "TRUE" )
manhattan_crashes_by_year_killed$proportion = manhattan_crashes_by_year_killed$n /manhattan_crashes_by_year$n
manhattan_crashes_by_year_killed$proportion = percent(manhattan_crashes_by_year_killed$proportion)

manhattan_crashes_by_year_injured = cleaned_manhattan_crashes %>% count(year,NUMBER.OF.PERSONS.INJURED > 0 & NUMBER.OF.PERSONS.KILLED == 0)
manhattan_crashes_by_year_injured = filter(manhattan_crashes_by_year_injured, `NUMBER.OF.PERSONS.INJURED > 0 & ...` == "TRUE" )
manhattan_crashes_by_year_injured$proportion = manhattan_crashes_by_year_injured$n/ manhattan_crashes_by_year$n
manhattan_crashes_by_year_injured$proportion = percent(manhattan_crashes_by_year_injured$proportion)

manhattan_crashes_by_year_property = cleaned_manhattan_crashes %>% count(year,NUMBER.OF.PERSONS.INJURED == 0 & NUMBER.OF.PERSONS.KILLED == 0)
manhattan_crashes_by_year_property = filter(manhattan_crashes_by_year_property, `NUMBER.OF.PERSONS.INJURED == 0 & ...` == "TRUE" )
manhattan_crashes_by_year_property$proportion = manhattan_crashes_by_year_property$n /manhattan_crashes_by_year$n
manhattan_crashes_by_year_property$proportion = percent(manhattan_crashes_by_year_property$proportion)

#visualization of how many people killed each year and proportion
accidents_per_year_killed = ggplot(data=manhattan_crashes_by_year_killed, aes(x=year, y=n, group=1)) +
  geom_line()+
  geom_point() +
  ggtitle("People killed per year in Manhattan")
accidents_per_year_killed

accidents_per_year_killed_proportions = ggplot(data=manhattan_crashes_by_year_killed, aes(x=year, y=proportion, group=1)) +
  geom_line()+
  geom_point() +
  ggtitle("People killed per year in Manhattan proportions")
accidents_per_year_killed_proportions


#map of hotzones of accidents. mean of latitude and longitude. one std of both and map it.
#get rid of any rows with NA
manhattan_crashes_no_NAs = na.omit(cleaned_manhattan_crashes )
#get rid of accidents with latitude and longitude = 0 
manhattan_crashes_no_missing_coordinates = manhattan_crashes_no_NAs[manhattan_crashes_no_NAs$LATITUDE > 0,]
manhattan_crashes_lat_lon= manhattan_crashes_no_missing_coordinates[manhattan_crashes_no_missing_coordinates$LATITUDE > 40.700620 & manhattan_crashes_no_missing_coordinates$LATITUDE <40.877069,]
manhattan_crashes_lat_lon= manhattan_crashes_lat_lon[manhattan_crashes_lat_lon$LONGITUDE < -73.913870 & manhattan_crashes_lat_lon$LONGITUDE > -74.021283,]
latitude_mean = mean(manhattan_crashes_lat_lon$LATITUDE, na.rm = TRUE)
latitude_sd = sd(manhattan_crashes_lat_lon$LATITUDE, na.rm = TRUE)
longitude_mean = mean(manhattan_crashes_lat_lon$LONGITUDE, na.rm = TRUE)
longitude_sd = sd(manhattan_crashes_lat_lon$LONGITUDE, na.rm = TRUE)


#map the min and max of latitude and longitude too
latitude_min = min(manhattan_crashes_lat_lon$LATITUDE)
latitude_max = max(manhattan_crashes_lat_lon$LATITUDE)
longitude_min = min(manhattan_crashes_lat_lon$LONGITUDE)
longitude_max = max(manhattan_crashes_lat_lon$LONGITUDE)



density_map =ggplot(manhattan_crashes_lat_lon, aes(x = LONGITUDE, y = LATITUDE)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = 2,
                 geom = "polygon", data = manhattan_crashes_lat_lon) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')
density_map

#I have highlighted the intersections that have witnessed at least one accident per week on average
accidents_group_by_latlong= manhattan_crashes_lat_lon %>%
  group_by(latitude = cut(LATITUDE, breaks = seq(latitude_min, latitude_max+264/364000*3, 264/364000*3),dig.lab = 8), longitude = cut(LONGITUDE, breaks = seq(longitude_min, longitude_max+ 750/364000*3, 750/364000*3),dig.lab = 8)) %>%
  summarise(n = n())
accidents_group_by_latlong$accidents_per_week = accidents_group_by_latlong$n/468  


  