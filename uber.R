library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
library(data.table)

colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")

files = list.files()
uber <- rbindlist(lapply(list.files(), fread))           
head(uber)


#formatting the Date/Time column and then creating day, month, year columns as factors

uber$`Date/Time` = as.POSIXct(uber$`Date/Time`, format = "%m/%d/%Y %H:%M:%S")

uber$Time <- format(as.POSIXct(uber$`Date/Time`, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
uber$Date.Time <- ymd_hms(uber$`Date/Time`)
uber$day <- factor(day(uber$`Date/Time`))
uber$month <- factor(month(uber$`Date/Time`, label = TRUE))
uber$year <- factor(year(uber$`Date/Time`))
uber$dayofweek <- factor(wday(uber$`Date/Time`, label = TRUE))

uber$hour = factor(hour(hms(uber$Time)))
uber$minute = factor(minute(hms(uber$Time)))
uber$second = factor(second(hms(uber$Time)))

#Plotting trips by hours
uber_hour = uber %>%
  group_by(hour) %>%
  summarise(Total = n())

datatable(uber_hour)

ggplot(uber_hour, aes(hour, Total)) +
  geom_bar(stat = 'identity', fill = 'green', color = 'yellow') +
  ggtitle('Trips Every Hour') +
  theme(legend.position = 'none') +
  scale_y_continuous(labels = comma)

uber_month_hour <- uber %>%
  group_by(month,hour) %>%
  summarise(Total_rides = n())

ggplot(uber_month_hour, aes(hour, Total_rides, fill = month))+
  geom_bar(stat = 'identity') +
  ggtitle('Trips for Every Hour in a Month') +
  theme(legend.position = 'none') +
  scale_y_continuous(labels = comma)

day_trips = uber %>%
  group_by(day) %>%
  summarise(Total = n())
datatable(day_trips)

ggplot(day_trips, aes(day, Total)) +
  geom_bar(stat = 'identity') +
  ggtitle('Trips Everyday') +
  theme(legend.position = 'none') +
  scale_y_continuous(labels = comma)

day_month_trip = uber %>%
  group_by(month, day) %>%
  summarise(Total = n())

ggplot(day_month_trip, aes(day, Total, fill = month)) +
  geom_bar(stat = 'identity') +
  ggtitle('Trips By Day and Month')+
  theme(legend.position = 'none')+
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = colors)

#Now let's find Number of trips per month

uber_month = uber %>%
  group_by(month) %>%
  summarise(Total = n())

ggplot(uber_month, aes(month, Total, fill = month)) +
  geom_bar(stat = 'identity') +
  ggtitle('Total Trips in a Month') +
  theme(legend.position = 'none') +
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = colors)

ggplot(uber, aes(Base)) +
  geom_bar(fill = 'blue')+
  scale_y_continuous(labels = comma) +
  ggtitle('Trips By Bases')

#Trips per Days of the week
ggplot(uber, aes(Base, fill = dayofweek))+
  geom_bar(position = 'dodge')+
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = colors)

#Now let's try some heatmaps
uber_day_hour = uber%>%
  group_by(day, hour) %>%
  summarise(Total = n())

ggplot(uber_day_hour, aes(day, hour, fill = Total))+
  geom_tile(color = 'black')+
  ggtitle('Hour and Day Heatmap')

#Now let create some map visualizations
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

ggplot(uber, aes(Lon, Lat, color = Base)) +
  geom_point(size = 1)+
     scale_x_continuous(limits = c(min_long, max_long)) +
     scale_y_continuous(limits = c(min_lat, max_lat))
  


