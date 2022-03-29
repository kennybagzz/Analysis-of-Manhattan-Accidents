library(tidyverse)
library(htmlwidgets)
library(lubridate)
library(rmarkdown)
library(ggplot2)
library(dplyr)
library("ggmap")
library(stringr)
library(ggrepel)

#function to convert number into percentage
percent <- function(x, digits = 2, format = "f", ...) {      
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}
accidents_per_year_block = function(lat, long){
  p<- filter(accidents_group_by_latlongyear,middle_latitude == lat & middle_longitude == long)
  g= ggplot(data=p, aes(x=year, y=accidents_per_week))+
    geom_line()+
    geom_point(size = 4)+
    ylab("Accidents Per Week") +
    xlab("Year") + 
    theme(plot.title = element_text(size=28))+
    theme(axis.title = element_text(size = 24))+
    theme(axis.text = element_text(size = 16))+
    geom_label_repel(aes(label = n),size = 6)+
    ggtitle("Number of Accidents Per Week at this location")
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
  geom_point(size = 4)+
  ylab("Total") +
  xlab("Year") + 
  theme(plot.title = element_text(size=28))+
  theme(axis.title = element_text(size = 24))+
  theme(axis.text = element_text(size = 16))+
  geom_label_repel(aes(label = n),size = 6)+
  ggtitle("Number of Accidents Per Year in Manhattan")
accidents_per_yr_graph



#find out how many accidents each month
manhattan_crashes_by_month = cleaned_manhattan_crashes %>% count(month)
#use line graph to visualize
accidents_per_month_graph = ggplot(data=manhattan_crashes_by_month, aes(x=month, y=n, group=1)) +
  geom_line()+
  geom_point(size = 4)+
  ylab("Total") +
  xlab("Month") + 
  theme(plot.title = element_text(size=28))+
  theme(axis.title = element_text(size = 24))+
  theme(axis.text = element_text(size = 16))+
  geom_label_repel(aes(label = n),size = 6)+
  scale_x_discrete(labels=c("Jan","Feb","Mar",'Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))+
  ggtitle("Number of Accidents Per Month in Manhattan")
accidents_per_month_graph



#find out how many accidents for each day of the week
manhattan_crashes_by_day = cleaned_manhattan_crashes%>% count(weekday)
accidents_per_day_graph = ggplot(data=manhattan_crashes_by_day, aes(x=weekday, y=n, group=1)) +
  geom_line()+
  geom_point(size = 4) +
  ylab("Total") +
  xlab("Day of Week") +
  theme(plot.title = element_text(size=28))+
  theme(axis.title = element_text(size = 24))+
  theme(axis.text = element_text(size = 16))+
  geom_label_repel(aes(label = n),size = 6)+
  ggtitle("Number of Accidents by Day of Week in Manhattan")
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

accidents_by_hour_graph = ggplot(data=accidents_by_hour, aes(x=group, y=n, group = 1)) +
  geom_line()+
  geom_point() +
  scale_x_discrete(labels=as.character(c(seq(0,23))))+
  ylab("Total") +
  xlab("Time (24 hour clock)") +
  ggtitle("Accidents by Hour in Manhattan")
accidents_by_hour_graph

#propotion of accidents with just property damage, proportion with at least 1 injured, no fatalities, proportion of at least 1 killed
accidents_people_killed = sum(cleaned_manhattan_crashes$NUMBER.OF.PERSONS.KILLED > 0,na.rm = TRUE)
accidents_nokilled_but_injured = sum(cleaned_manhattan_crashes$NUMBER.OF.PERSONS.INJURED > 0 & cleaned_manhattan_crashes$NUMBER.OF.PERSONS.KILLED == 0, na.rm = TRUE)
accidents_property_damage = sum(cleaned_manhattan_crashes$NUMBER.OF.PERSONS.INJURED == 0 & cleaned_manhattan_crashes$NUMBER.OF.PERSONS.KILLED == 0, na.rm = TRUE)

manhattan_crashes_by_year_killed = cleaned_manhattan_crashes %>% count(year, NUMBER.OF.PERSONS.KILLED > 0)
manhattan_crashes_by_year_killed = filter(manhattan_crashes_by_year_killed, `NUMBER.OF.PERSONS.KILLED > 0`  == "TRUE" )
options(digits = 1)
manhattan_crashes_by_year_killed$proportion = (manhattan_crashes_by_year_killed$n /manhattan_crashes_by_year$n) * 100
manhattan_crashes_by_year_killed$proportion = round(manhattan_crashes_by_year_killed$proportion,digits =2)
manhattan_crashes_by_year_killed$proportion = percent(manhattan_crashes_by_year_killed$proportion)

manhattan_crashes_by_year_injured = cleaned_manhattan_crashes %>% count(year,NUMBER.OF.PERSONS.INJURED > 0 & NUMBER.OF.PERSONS.KILLED == 0)
manhattan_crashes_by_year_injured = filter(manhattan_crashes_by_year_injured, `NUMBER.OF.PERSONS.INJURED > 0 & ...` == "TRUE" )
manhattan_crashes_by_year_injured$proportion = (manhattan_crashes_by_year_injured$n/ manhattan_crashes_by_year$n)*100
manhattan_crashes_by_year_injured$proportion = round(manhattan_crashes_by_year_injured$proportion,digits =2)
manhattan_crashes_by_year_injured$proportion = percent(manhattan_crashes_by_year_injured$proportion)


manhattan_crashes_by_year_property = cleaned_manhattan_crashes %>% count(year,NUMBER.OF.PERSONS.INJURED == 0 & NUMBER.OF.PERSONS.KILLED == 0)
manhattan_crashes_by_year_property = filter(manhattan_crashes_by_year_property, `NUMBER.OF.PERSONS.INJURED == 0 & ...` == "TRUE" )
options(digits = 4)
manhattan_crashes_by_year_property$proportion = (manhattan_crashes_by_year_property$n /manhattan_crashes_by_year$n) * 100
manhattan_crashes_by_year_property$proportion = round(manhattan_crashes_by_year_property$proportion, digits=1)
manhattan_crashes_by_year_property$proportion = percent(manhattan_crashes_by_year_property$proportion)
as.numeric_version(manhattan_crashes_by_year_property$proportion)

#visualization of how many people killed each year and proportion
accidents_per_year_killed = ggplot(data=manhattan_crashes_by_year_killed, aes(x=year, y=n, group=1)) +
  geom_line()+
  geom_point(size = 4) +
  ylab("Total") +
  xlab("Year") + 
  theme(plot.title = element_text(size=28))+
  theme(axis.title = element_text(size = 24))+
  theme(axis.text = element_text(size = 16))+
  geom_label_repel(aes(label = n),size = 5)+
  ggtitle("Accidents Involving At Least One Fatality")
accidents_per_year_killed

accidents_per_year_killed_proportions = ggplot(data=manhattan_crashes_by_year_killed, aes(x=year, y=proportion, group=1)) +
  geom_line()+
  geom_point(size = 4) +
  ylab("Proportion (%)") +
  xlab("Year") + 
  theme(plot.title = element_text(size=24))+
  theme(axis.title = element_text(size = 24))+
  theme(axis.text = element_text(size = 16))+
  geom_label_repel(aes(label = proportion),size = 5)+
  ggtitle("Proportions of Accidents Involving At Least One Fatality ")
accidents_per_year_killed_proportions

accidents_per_year_injured = 
  ggplot(data=manhattan_crashes_by_year_injured, aes(x=year, y=n, group=1)) +
  geom_line()+
  geom_point(size = 4) +
  ylab("Total Accidents") +
  xlab("Year") + 
  theme(plot.title = element_text(size=20))+
  theme(axis.title = element_text(size = 24))+
  theme(axis.text = element_text(size = 16))+
  geom_label_repel(aes(label = n),size = 5)+
  ggtitle("Accidents Involving At Least One Injury and No Fatalities")
accidents_per_year_injured

accidents_per_year_injured_proportion = 
  ggplot(data=manhattan_crashes_by_year_injured, aes(x=year, y=proportion, group=1)) +
  geom_line()+
  geom_point(size = 4) +
  ylab("Proportion (%)") +
  xlab("Year") + 
  theme(plot.title = element_text(size=18))+
  theme(axis.title = element_text(size = 24))+
  theme(axis.text = element_text(size = 16))+
  geom_label_repel(aes(label = proportion),size = 5)+
  ggtitle("Proportions of Accidents Involving At Least One Injury and No Fatalities")
accidents_per_year_injured_proportion

accidents_per_year_property =
  ggplot(data=manhattan_crashes_by_year_property, aes(x=year, y=n, group=1)) +
  geom_line()+
  geom_point(size = 4) +
  ylab("Total Accidents") +
  xlab("Year") + 
  theme(plot.title = element_text(size=28))+
  theme(axis.title = element_text(size = 24))+
  theme(axis.text = element_text(size = 16))+
  geom_label_repel(aes(label = n),size = 5)+
  ggtitle("Accidents Involving Only Property Damage")
accidents_per_year_property

accidents_per_year_property_proportion = 
  ggplot(data=manhattan_crashes_by_year_property, aes(x=year, y=proportion, group=1)) +
  geom_line()+
  geom_point(size = 4) +
  ylab("Proportion (%)") +
  xlab("Year") + 
  theme(plot.title = element_text(size=22))+
  theme(axis.title = element_text(size = 24))+
  theme(axis.text = element_text(size = 16))+
  geom_label_repel(aes(label = proportion),size = 5)+
  ggtitle("Proportions of Accidents Involving Only Property Damage")
accidents_per_year_property_proportion

idc = ggplot(data=manhattan_crashes_by_year_killed, aes(x=year, y=n, group=1)) +
  geom_line()+
  geom_line(data=manhattan_crashes_by_year_injured, aes(x=year,y=n)) + 
  geom_line(data=manhattan_crashes_by_year_property, aes(x=year, y=n))+
  geom_point(size = 4) +
  geom_point(data = manhattan_crashes_by_year_injured,size = 4)+
  ylab("Proportion (%)") +
  xlab("Year") + 
  theme(plot.title = element_text(size=22))+
  theme(axis.title = element_text(size = 24))+
  theme(axis.text = element_text(size = 16))+
  geom_label_repel(aes(label = n),size = 4)+
  scale_y_continuous(breaks = seq(0, 40000, by = 2500))+
  ggtitle("Proportions of Accidents Involving Only Property Damage")
idc
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
  group_by(latitude = cut(LATITUDE, breaks = seq(latitude_min, latitude_max+264/364000*4, 264/364000*4),dig.lab = 8), longitude = cut(LONGITUDE, breaks = seq(longitude_min, longitude_max+ 264/364000*4, 264/364000*4),dig.lab = 8)) %>%
  summarise(n = n())
accidents_group_by_latlong$accidents_per_week = accidents_group_by_latlong$n/468  
accidents_group_by_latlong = na.omit(accidents_group_by_latlong)

accidents_group_by_latlongyear = manhattan_crashes_lat_lon %>%
  group_by(latitude = cut(LATITUDE, breaks = seq(latitude_min, latitude_max+264/364000*3, 264/364000*3),dig.lab = 8,), longitude = cut(LONGITUDE, breaks = seq(longitude_min, longitude_max+ 264/364000*3, 264/364000*3),dig.lab = 8),year) %>%
  summarise(n = n())
accidents_group_by_latlongyear$accidents_per_week = accidents_group_by_latlongyear$n/52  

e =accidents_group_by_latlongyear[,"latitude"]
aaa = as.vector(e$latitude)
baa = gsub("\\(|\\]","", aaa)
caa = unlist(strsplit(baa,","))
daa = caa[seq(1,nrow(accidents_group_by_latlongyear) *2, by = 2)]
options(digits=9)
eaa = as.numeric(daa)
accidents_group_by_latlongyear$latitude1 = eaa

f =accidents_group_by_latlongyear[,"latitude"]
add = as.vector(e$latitude)
bdd = gsub("\\(|\\]","", add)
cdd = unlist(strsplit(bdd,","))
ddd = cdd[seq(2,nrow(accidents_group_by_latlongyear) *2, by = 2)]
options(digits=9)
edd = as.numeric(ddd)
accidents_group_by_latlongyear$latitude2 = edd

g =accidents_group_by_latlongyear[,"longitude"]
aqq = as.vector(g$longitude)
bqq = gsub("\\(|\\]","", aqq)
cqq = unlist(strsplit(bqq,","))
dqq = cqq[seq(1,nrow(accidents_group_by_latlongyear) *2, by = 2)]
options(digits=9)
eqq = as.numeric(dqq)
accidents_group_by_latlongyear$longitude1 = eqq

h =accidents_group_by_latlongyear[,"longitude"]
aww = as.vector(g$longitude)
bww = gsub("\\(|\\]","", aww)
cww = unlist(strsplit(bww,","))
dww = cww[seq(2,nrow(accidents_group_by_latlongyear) *2, by = 2)]
options(digits=9)
eww = as.numeric(dww)
accidents_group_by_latlongyear$longitude2 = eww

accidents_group_by_latlongyear$middle_latitude = (accidents_group_by_latlongyear$latitude1 +accidents_group_by_latlongyear$latitude2)/2

accidents_group_by_latlongyear$middle_longitude = (accidents_group_by_latlongyear$longitude1 + accidents_group_by_latlongyear$longitude2)/2



a =accidents_group_by_latlong[1,1]
a
axx = as.vector(a$latitude[1])[1]
axx
bxx = gsub("\\(|\\]","", axx)
bxx

cxx = unlist(strsplit(bxx,","))
cxx
dxx = as.numeric(cxx[1])
dxx


getting_latlong_from_dataframe = function(df,df_column_name,one_or_two, new_column_name) {
  options(digits=9)
  a= gsub("\\(|\\]","",as.vector(df[,df_column_name]))
  df[new_column_name] = a[seq(one_or_two,nrow(df)*2, by = 2)] %>%
   as.numeric()
}
nrow(accidents_group_by_latlong) *2

getting_latlong_from_dataframe(accidents_group_by_latlong,"latitude", 1, "latitude1")

a =accidents_group_by_latlong[,"latitude"]
axx = as.vector(a$latitude)
bxx = gsub("\\(|\\]","", axx)
cxx = unlist(strsplit(bxx,","))
dxx = cxx[seq(1,nrow(accidents_group_by_latlong) *2, by = 2)]
options(digits=9)
exx = as.numeric(dxx)
accidents_group_by_latlong$latitude1 = exx

b =accidents_group_by_latlong[,"latitude"]
azz = as.vector(a$latitude)
bzz = gsub("\\(|\\]","", azz)
czz = unlist(strsplit(bzz,","))
dzz = czz[seq(2,nrow(accidents_group_by_latlong) *2, by = 2)]
options(digits=9)
ezz = as.numeric(dzz)
accidents_group_by_latlong$latitude2 = ezz

c =accidents_group_by_latlong[,"longitude"]
avv= as.vector(c$longitude)
bvv = gsub("\\(|\\]","", avv)
cvv = unlist(strsplit(bvv,","))
dvv = cvv[seq(1,nrow(accidents_group_by_latlong) *2, by = 2)]
options(digits=9)
evv = as.numeric(dvv)
accidents_group_by_latlong$longitude1 = evv

d =accidents_group_by_latlong[,"longitude"]
abb= as.vector(c$longitude)
bbb = gsub("\\(|\\]","", abb)
cbb = unlist(strsplit(bbb,","))
dbb = cbb[seq(2,nrow(accidents_group_by_latlong) *2, by = 2)]
options(digits=9)
ebb = as.numeric(dbb)
accidents_group_by_latlong$longitude2 = ebb

accidents_group_by_latlong$middle_latitude = (accidents_group_by_latlong$latitude1 +accidents_group_by_latlong$latitude2)/2

accidents_group_by_latlong$middle_longitude = (accidents_group_by_latlong$longitude1 + accidents_group_by_latlong$longitude2)/2



hg = filter(accidents_group_by_latlong, accidents_per_week >0.95)

hgb

vas = manhattan_crashes_lat_lon %>%
  group_by(latitude = LATITUDE, longitude = LONGITUDE )%>%
  summarise(n = n())

vas$accidentsperweek = vas$n/ 468

asd = mapply(accidents_per_year_block, accidents_group_by_latlong$middle_latitude,accidents_group_by_latlong$middle_longitude)
             

view(asd[1])
             
             
             
