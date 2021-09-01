library(tidyverse)
library(janitor)
library(lubridate)

#COLLECT DATASET
##data from (https://divvy-tripdata.s3.amazonaws.com/index.html)
aug_2020 <- read_csv('202008-divvy-tripdata.csv')
sep_2020 <- read_csv('202009-divvy-tripdata.csv')
oct_2020 <- read_csv('202010-divvy-tripdata.csv')
nov_2020 <- read_csv('202011-divvy-tripdata.csv')
dec_2020 <- read_csv('202012-divvy-tripdata.csv')
jan_2021 <- read_csv('202101-divvy-tripdata.csv')
feb_2021 <- read_csv('202102-divvy-tripdata.csv')
mar_2021 <- read_csv('202103-divvy-tripdata.csv')
apr_2021 <- read_csv('202104-divvy-tripdata.csv')
may_2021 <- read_csv('202105-divvy-tripdata.csv')
jun_2021 <- read_csv('202106-divvy-tripdata.csv')
jul_2021 <- read_csv('202107-divvy-tripdata.csv')

#stack dataset together into one big dataframe
bike_rides <- rbind(aug_2020,sep_2020,oct_2020,nov_2020,dec_2020,jan_2021,feb_2021,mar_2021,apr_2021,may_2021,jun_2021,jul_2021)

#rename columns
bike_rides <- bike_rides %>% rename(trip_id=ride_id,
                                bike_type = rideable_type,
                                user_type=member_casual)

#remove unnecessary columns in analysis (latitude, longitude)
bike_rides <- bike_rides %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))

#CLEAN UP AND PREPARE DATA
##convert date/time stamp to date/time
bike_rides$started_at <- lubridate::ymd_hms(bike_rides$started_at)
bike_rides$ended_at <- lubridate::ymd_hms(bike_rides$ended_at)

##add column for time
bike_rides$start_time <- lubridate::hms(bike_rides$started_at)
bike_rides$end_time <- lubridate::hms(bike_rides$ended_at)

##add columns for date, month, day, year, day_of_week
bike_rides$date <- as.Date(bike_rides$started_at)  
bike_rides$month <- format(as.Date(bike_rides$date), "%m")
bike_rides$day <- format(as.Date(bike_rides$date), "%d")
bike_rides$year <- format(as.Date(bike_rides$date), "%Y")
bike_rides$day_of_week <- format(as.Date(bike_rides$date), "%A")

##calculate ride_length for all trip (in seconds)
bike_rides$ride_length <- difftime(bike_rides$ended_at,bike_rides$started_at)

#make sure ride_length is numeric
is.factor(bike_rides$ride_length)
bike_rides$ride_length <- as.numeric(as.character(bike_rides$ride_length))
is.numeric(bike_rides$ride_length)

##remove all negative values from ride_length (negative means bike had taken out for quality check)
bike_rides_v2 <- bike_rides[!(bike_rides$start_station_name == "HQ QR" | bike_rides$ride_length<0),]
##remove empty rows
bike_rides_v2 <- bike_rides_v2 %>% remove_empty("rows")

#DESCRIPTIVE ANALYSIS
##decriptive analysis on ride_length
summary(bike_rides_v2$ride_length)

##compare member and casual users
###average user ride time by member vs casual
aggregate(bike_rides_v2$ride_length ~ bike_rides_v2$user_type, FUN = mean)

###median user ride time by member vs casual
aggregate(bike_rides_v2$ride_length ~ bike_rides_v2$user_type, FUN = median)

###maximum user ride time by member vs casual
aggregate(bike_rides_v2$ride_length ~ bike_rides_v2$user_type, FUN = max)

###minimum user ride time by member vs casual
aggregate(bike_rides_v2$ride_length ~ bike_rides_v2$user_type, FUN = min)

###average user ride time by each day for members vs casual
aggregate(bike_rides_v2$ride_length ~ bike_rides_v2$user_type + bike_rides_v2$day_of_week, FUN = mean)

###fix order of the days
bike_rides_v2$day_of_week <- ordered(bike_rides_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

###average user ride time by each day for members vs casual (days ordered)
avg_time_day <- aggregate(bike_rides_v2$ride_length ~ bike_rides_v2$user_type + bike_rides_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
summary_trip <- bike_rides_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(user_type, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(user_type, weekday)								# sorts

# visualize the number of rides by user type
bike_rides_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(user_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(user_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = user_type)) +
  geom_col(position = "dodge")

# visualize average ride time by user type
bike_rides_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(user_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(user_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = user_type)) +
  geom_col(position = "dodge")

#EXPORT SUMMARY FILE
write.csv(bike_rides_v2, file='bike_rides.csv')
write.csv(avg_time_day, file='avg_time_day.csv')
write.csv(summary_trip, file='summary_trip.csv')


