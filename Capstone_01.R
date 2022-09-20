# Business Task: The task is to come up with recommendations for stakeholders to design marketing strategies
# to convert casual riders to annual members.
# Load the necessary packages
library(tidyverse)
library(lubridate)
library(ggpubr)


# Set working directory
setwd("F:/02_COURSES/06_DATA ANALYTICS/01_Google Data Analytics Certificate/08_Capstone/Case_Study_01/Data/csv")
# OR
setwd("F:/02_COURSES/06_DATA ANALYTICS/01_Google Data Analytics Certificate/08_Capstone/Case_Study_01/Data/xls")

# Load individual files 
tripdata_202108 <- read.csv("202108-divvy-tripdata.csv")
tripdata_202109 <- read.csv("202109-divvy-tripdata.csv")
tripdata_202110 <- read.csv("202110-divvy-tripdata.csv")
tripdata_202111 <- read.csv("202111-divvy-tripdata.csv")
tripdata_202112 <- read.csv("202112-divvy-tripdata.csv")
tripdata_202201 <- read.csv("202201-divvy-tripdata.csv")
tripdata_202202 <- read.csv("202202-divvy-tripdata.csv")
tripdata_202203 <- read.csv("202203-divvy-tripdata.csv")
tripdata_202204 <- read.csv("202204-divvy-tripdata.csv")
tripdata_202205 <- read.csv("202205-divvy-tripdata.csv")
tripdata_202206 <- read.csv("202206-divvy-tripdata.csv")
tripdata_202207 <- read.csv("202207-divvy-tripdata.csv")

# Combine all data frames into one
tripdata <- rbind(tripdata_202108, tripdata_202109, tripdata_202110, tripdata_202111, tripdata_202112,
                  tripdata_202201, tripdata_202202, tripdata_202203, tripdata_202204, tripdata_202205,
                  tripdata_202206, tripdata_202207)

# Free the global environment
rm(tripdata_202108, tripdata_202109, tripdata_202110, tripdata_202111, tripdata_202112,
   tripdata_202201, tripdata_202202, tripdata_202203, tripdata_202204, tripdata_202205,
   tripdata_202206, tripdata_202207)

# Create csv for future
write.csv(tripdata, "F:/02_COURSES/06_DATA ANALYTICS/01_Google Data Analytics Certificate/08_Capstone/Case_Study_01/Data/csv/tripdata.csv")



# Optimization
tripdata <- read.csv("202201-divvy-tripdata.csv")
# OR
tripdata <- read.csv("tripdata_sample.csv")

# Study of tripdata
View(tripdata)
summary(tripdata)
str(tripdata)
glimpse(tripdata)
colSums(is.na(tripdata))

# We have NA values in end_lat and end_lng

# Number of unique entries
tripdata %>% 
  distinct(ride_id) %>% 
  summarise(total=n())

# Number of entries with empty start or end station info
tripdata %>% 
  filter(start_station_name == ""  | end_station_name == ""  | 
           start_station_id == ""  | end_station_id =="") %>% 
  summarise(total=n())

# Keeping only required columns
tripdata <- tripdata %>% 
  select(ride_id, rideable_type, started_at, ended_at, member_casual)

# Calculate trip_length

tripdata <- tripdata %>% 
  mutate(trip_length = round(difftime(ended_at, started_at, units='mins'), digits = 2))

# OR

tripdata <- tripdata %>% 
  mutate(trip_length = round(difftime(mdy_hms(tripdata$ended_at), mdy_hms(tripdata$started_at), units='mins'), digits = 2))

# Count the occurrences of negative trip length
tripdata %>% 
  filter(trip_length < 0) %>% 
  summarise(n = n())

# delete the entries with negative trip length
tripdata <- tripdata %>% 
  filter(trip_length >= 0)

# Study trip_length
tripdata %>% 
  summarise(min = min(trip_length), max = max(trip_length), mean = mean(trip_length), sd= sd(trip_length))
quantile(tripdata$trip_length)
quantile(tripdata$trip_length, 0.99)

plot(as.integer(tripdata$trip_length))

hist(as.integer(tripdata$trip_length))

# Extract trip start day
tripdata <- tripdata %>% 
  mutate (day_of_week = wday(tripdata$started_at, label=TRUE, abbr=FALSE))
# OR
tripdata <- tripdata %>% 
  mutate (day_of_week = wday(mdy_hms(tripdata$started_at), label=TRUE, abbr=FALSE))

# Extract the month
tripdata <- tripdata %>% 
  mutate (trip_month = month(mdy_hms(ended_at), label = TRUE, abbr=FALSE))

# Extract trip_start hour
tripdata <- tripdata %>% 
  mutate (trip_start_hour = format(mdy_hms(ended_at), format = "%H"))


# Separate date and time

tripdata <- tripdata %>% 
  mutate(start_date = format(mdy_hms(started_at), format = "%Y-%m-%d"),
    start_time = format(mdy_hms(started_at), format = "%H:%M:%S"),
    end_date = format(mdy_hms(ended_at), format = "%Y-%m-%d"),
    end_time = format(mdy_hms(ended_at), format = "%H:%M:%S")) %>% 
  select(!ended_at & !started_at)


# Number of entries where trip didnt end on the same day
tripdata %>% 
  filter(!(start_date == end_date)) %>% 
  summarise(n =n())

# Tibble 
tripdata %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(rides = n()) %>% 
  mutate(percentage = rides/sum(rides)*100) %>% 
  arrange(-rides)

# Analysis

# Variable one : Rideable type
tripdata %>% 
  group_by(rideable_type) %>% 
  summarise(total=n()) %>% 
  mutate(per = round((total/sum(total)*100), digits = 2))

# Number of rides by rideable type
tripdata %>% 
  group_by(rideable_type) %>% 
  summarise(total=n()) %>% 
  mutate(per = round((total/sum(total)*100), digits = 2)) %>%
  ggplot(aes(x = reorder(rideable_type, -total), y = total, fill = rideable_type, label = per))+
  theme_bw()+
  geom_col(alpha = 0.5)+
  geom_text(position = position_stack(vjust = 0.9))+
  labs(x = "Rideable Type",
       y = "No of rides",
       title = "Number of rides by rideable type")

# Pie chart: Number of rides by rideable type
tripdata %>% 
  group_by(rideable_type) %>% 
  summarise(total=n()) %>% 
  mutate(per = round((total/sum(total)*100), digits = 2)) %>%
  ggplot(aes(x = "", y = total, fill = rideable_type, label = per))+
  theme_void()+
  geom_bar(stat = "identity", alpha = 0.8)+
  geom_text(position = position_stack(vjust = 0.5))+
  coord_polar("y", start=90)+
  labs(title = "Number of rides by rideable type")

# Number of rides by start_station_name
tripdata %>% 
  filter(!start_station_name == "") %>%
  group_by(start_station_name) %>% 
  summarise(total=n()) %>% 
  mutate(per = round((total/sum(total)*100), digits = 2)) %>% 
  arrange(-per)

tripdata %>% 
  filter(!start_station_name == "") %>%
  filter(member_casual == "member") %>% 
  group_by(start_station_name) %>% 
  summarise(total=n()) %>% 
  mutate(per = round((total/sum(total)*100), digits = 2)) %>% 
  arrange(-per)

tripdata %>% 
  filter(!start_station_name == "") %>%
  filter(member_casual == "casual") %>% 
  group_by(start_station_name) %>% 
  summarise(total=n()) %>% 
  mutate(per = round((total/sum(total)*100), digits = 2)) %>% 
  arrange(-per)

# Number of rides by end_station_name
tripdata %>% 
  filter(!end_station_name == "") %>%   
  group_by(end_station_name) %>% 
  summarise(total=n()) %>% 
  mutate(per = round((total/sum(total)*100), digits = 2)) %>% 
  arrange(-per)

tripdata %>% 
  filter(!end_station_name == "") %>% 
  filter(member_casual == "member") %>%   
  group_by(end_station_name) %>% 
  summarise(total=n()) %>% 
  mutate(per = round((total/sum(total)*100), digits = 2)) %>% 
  arrange(-per)

tripdata %>% 
  filter(!end_station_name == "") %>%
  filter(member_casual == "casual") %>%    
  group_by(end_station_name) %>% 
  summarise(total=n()) %>% 
  mutate(per = round((total/sum(total)*100), digits = 2)) %>% 
  arrange(-per)

# Number of rides by member_casual
tripdata %>% 
  group_by(member_casual) %>% 
  summarise(total=n()) %>% 
  mutate(per = round((total/sum(total)*100), digits = 2)) %>% 
  ggplot(aes(x = reorder(member_casual, -total), y = total, fill = member_casual, label = per))+
  theme_bw()+
  geom_col(alpha = 0.5)+
  geom_text(position = position_stack(vjust = 0.9))+
  labs(x = "Member_Casual",
       y = "No of rides",
       title = "Number of rides by Member_Casual")

# Pie Chart :Number of rides by member_casual
tripdata %>% 
  group_by(member_casual) %>% 
  summarise(total=n()) %>% 
  mutate(per = round((total/sum(total)*100), digits = 2)) %>% 
  ggplot(aes(x = "", y = total, fill = member_casual, label = per))+
  theme_void()+
  geom_bar(stat = "identity", alpha = 0.8)+
  geom_text(position = position_stack(vjust = 0.5))+
  coord_polar("y", start=90)+
  labs(title = "Number of rides by member_Casual")

# Trip length
plot(tripdata$trip_length)
boxplot(tripdata$trip_length)

tripdata %>% 
  summarise(min = min(trip_length), max = max(trip_length), mean = mean(trip_length))


quantile(tripdata$trip_length)
quantile(tripdata$trip_length, 0.99)

tripdata %>% 
  filter(trip_length < quantile(trip_length, 0.99)) %>% 
  summarise(min = min(trip_length), max = max(trip_length), 
            mean = mean(trip_length), median = median(trip_length),
            sd = sd(trip_length))

tripdata %>% 
  filter(trip_length < quantile(trip_length, 0.99)) %>% 
  ggplot(aes(x = as.numeric(trip_length))) +
  theme_bw()+
  geom_histogram(binwidth = 1, fill = "light blue")+
  geom_vline(xintercept = mean(tripdata$trip_length), col = "red", lwd = 1)+
  geom_vline(xintercept = median(tripdata$trip_length), col = "blue", lwd = 1)+
  annotate("text", 
           x = 30,
           y = 9000,
           label = paste("Mean =", (round(mean(tripdata$trip_length), digits = 2))),
           col = "red",
           size = 4)+
  annotate("text",
           x = 30,
           y = 8000,
           label = paste("Median =", (round(median(tripdata$trip_length), digits = 2))),
           col = "blue",
           size = 4)+
  labs(x = "Trip length",
       y = "No of rides",
       title = "Histogram: Trip length")

# Number of rides by day of week
tripdata %>% 
  group_by(day_of_week) %>% 
  summarise(total=n()) %>% 
  mutate(per = round((total/sum(total)*100), digits = 2)) %>% 
  ggplot(aes(x = day_of_week, y = total, fill = day_of_week, label = total))+
  theme_bw()+
  geom_col(alpha = 0.5, show.legend = FALSE)+
  geom_text(position = position_stack(vjust = 1.05))+
  labs(x = "Day of week",
       y = "No of rides",
       title = "Number of rides by day of week")

# Number of rides by hour
hist(as.integer(tripdata$trip_start_hour))

boxplot(as.integer(tripdata$trip_start_hour))

ggplot(data = tripdata, aes(x = trip_start_hour))+
  theme_bw()+
  geom_bar(alpha = 0.7)+
  labs(x = "Hour",
       y = "No of rides",
       title = "Number of rides by hour")


# Number of rides by month
tripdata %>% 
  group_by(trip_month) %>% 
  summarise(total=n()) %>% 
  mutate(per = round((total/sum(total)*100), digits = 2)) %>% 
  ggplot(aes(x = trip_month, y = total, fill = trip_month, label = total))+
  theme_bw()+
  geom_col(alpha = 0.5, show.legend = FALSE)+
  geom_text(position = position_stack(vjust = 0.7), size = 3)+
  labs(x = "Month",
       y = "No of rides",
       title = "Number of rides by month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=0.5))

# Bi-variate

# Rideable type and member_casual
ggplot(data = tripdata, aes(x = rideable_type, fill = member_casual))+
  theme_bw()+
  geom_bar(alpha = 0.8, position = "dodge")+
  geom_text(stat='count', position = position_dodge(width = 0.9), vjust = -0.1, aes(label=..count..))+
  labs(x = "Rideable type",
       y = "No of rides",
       title = "Number of rides by rideable type")

# Trip length and member_casual

members <- tripdata %>% 
  filter(trip_length < quantile(trip_length, 0.99)) %>% 
  filter(member_casual == "member") %>% 
  select(trip_length)

casuals <- tripdata %>% 
  filter(trip_length < quantile(trip_length, 0.99)) %>% 
  filter(member_casual == "casual") %>% 
  select(trip_length)

members_tl <- ggplot(data = members, aes(x = as.numeric(trip_length))) +
  theme_bw()+
  geom_histogram(binwidth = 1, fill = "#56B4E9")+
  geom_vline(xintercept = mean(members$trip_length), col = "red", lwd = 1)+
  geom_vline(xintercept = median(members$trip_length), col = "blue", lwd = 1)+
  labs(x = "Trip length",
       y = "No of rides",
       subtitle = "Subset: Members")+
  annotate("text", 
           x = 40,
           y = 8000,
           label = paste("Mean =", (round(mean(members$trip_length), digits = 2))),
           col = "red",
           size = 4)+
  annotate("text", 
           x = 40,
           y = 7300,
           label = paste("Median =", (round(median(members$trip_length), digits = 2))),
           col = "blue",
           size = 4)

casuals_tl <- ggplot(data = casuals, aes(x = as.numeric(trip_length))) +
  theme_bw()+
  geom_histogram(binwidth = 1, fill = "#FF9999")+
  geom_vline(xintercept = median(casuals$trip_length), col = "blue", lwd = 1)+
  geom_vline(xintercept = mean(casuals$trip_length), col = "red", lwd = 1)+
  labs(x = "Trip length",
       y = "No of rides",
       subtitle = "Subset: Casuals")+
  annotate("text", 
           x = 40,
           y = 1300,
           label = paste("Mean =", (round(mean(casuals$trip_length), digits = 2))),
           col = "red",
           size = 4)+
  annotate("text", 
           x = 40,
           y = 1200,
           label = paste("Median =", (round(median(casuals$trip_length), digits = 2))),
           col = "blue",
           size = 4)

trip_l <- ggarrange(members_tl, casuals_tl)

annotate_figure(trip_l, top = text_grob("Histogram: Trip length", face = "bold", size = 12))

# Full data
tripdata %>% 
  group_by(member_casual) %>% 
  summarise(min = min(trip_length), max = max(trip_length), 
            mean = mean(trip_length), median = median(trip_length),
            sd = sd(trip_length))

# 99 percentile
tripdata %>% 
  filter(trip_length < quantile(trip_length, 0.99)) %>% 
  group_by(member_casual) %>% 
  summarise(min = min(trip_length), max = max(trip_length), 
            mean = mean(trip_length), median = median(trip_length),
            sd = sd(trip_length))

tripdata %>% 
  filter(trip_length < quantile(trip_length, 0.99)) %>%
  filter(member_casual == "member") %>% 
  summarise(min = min(trip_length), max = max(trip_length), 
            mean = mean(trip_length), median = median(trip_length),
            sd = sd(trip_length))

# Day of week, member_casual
ggplot(data = tripdata, aes(x = day_of_week, fill = member_casual))+
  theme_bw()+
  geom_bar(position = 'dodge', alpha = 0.7)+
  geom_text(stat = 'count', size = 4, position = position_dodge(width = 0.9), vjust = -0.1, aes(label=..count..))+
  labs(x = "Day of week",
       y = "No of rides",
       title = "Number of rides by day of week")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=0.5))

# Number of rides by hour, member casual
ggplot(data = tripdata, aes(x = trip_start_hour, fill = member_casual))+
  theme_bw()+
  geom_bar(alpha = 0.7, position = 'dodge')+
  labs(x = "Hour",
       y = "No of rides",
       title = "Number of rides by hour")


ggplot(data = tripdata, aes(x = trip_start_hour, group = member_casual, col = member_casual))+
  theme_bw()+
  geom_line(stat = 'count', lwd = 1)+
  labs(x = "Hour",
       y = "No of rides",
       title = "Number of rides by hour")

# Number of rides by month, member casual(bar)
ggplot(data = tripdata, aes(x = trip_month, fill = member_casual))+
  theme_bw()+
  geom_bar(alpha = 0.7, position = "dodge")+
  geom_text(stat = 'count', position = position_dodge(width = 0.9), vjust = -0.1, aes(label=..count..))+
  labs(x = "Month",
       y = "No of rides",
       title = "Number of rides by month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=0.5))

# Number of rides by month, member casual(line)
ggplot(data = tripdata, aes(x = trip_month, group = member_casual, col = member_casual))+
  theme_bw()+
  geom_line(stat = 'count', lwd = 1)+
  labs(x = "Month",
       y = "No of rides",
       title = "Number of rides by hour")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=0.5))

# Multi variate
# trip length, rideable type, member casual

tripdata %>% 
  filter(trip_length < quantile(trip_length, 0.99)) %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(min = min(trip_length), max = max(trip_length), 
            mean = mean(trip_length), median = median(trip_length),
            sd = sd(trip_length))

# Average trip length
tripdata %>% 
  filter(trip_length < quantile(trip_length, 0.99)) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(min = min(trip_length), max = max(trip_length), 
            mean = round(mean(trip_length), digits = 2), median = median(trip_length),
            sd = sd(trip_length)) %>% 
  ggplot(aes(x = day_of_week, y = mean, fill = member_casual, label = mean))+
  geom_col(position = "dodge")+
  geom_text(size = 3, position = position_dodge(width = 0.9), vjust = -0.1)+
  labs(x = "Day of week",
       y = "Average trip length",
       title = "Average trip length")

# Average trip length
tripdata %>% 
  filter(trip_length < quantile(trip_length, 0.99)) %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(min = min(trip_length), max = max(trip_length), 
            mean = round(mean(trip_length), digits = 2), median = median(trip_length),
            sd = sd(trip_length)) %>% 
  ggplot(aes(x = rideable_type, y = mean, fill = member_casual, label = mean))+
  geom_col(position = "dodge")+
  geom_text(size = 4, position = position_dodge(width = 0.9), vjust = -0.1)+
  labs(x = "Rideable type",
       y = "Average trip length",
       title = "Average trip length")

tripdata %>% 
  filter(trip_length < quantile(trip_length, 0.99)) %>% 
  group_by(member_casual, trip_start_hour) %>% 
  summarise(min = min(trip_length), max = max(trip_length), 
            mean = round(mean(trip_length), digits = 2), median = median(trip_length),
            sd = sd(trip_length)) %>% 
  ggplot(aes(x = trip_start_hour, y = mean, fill = member_casual, label = mean))+
  geom_col(position = "dodge")+
  geom_text(size = 3, position = position_dodge(width = 0.9), vjust = -0.1)+
  labs(x = "Hour",
       y = "Average trip length",
       title = "Average trip length")

tripdata %>% 
  filter(trip_length < quantile(trip_length, 0.99)) %>% 
  group_by(trip_month, member_casual) %>%
  summarise(min = min(trip_length), max = max(trip_length), 
            mean = round(mean(trip_length), digits = 2), median = median(trip_length),
            sd = sd(trip_length)) %>% 
  ggplot(aes(x = trip_month, y = mean, fill = member_casual, label = mean))+
  geom_col(position = "dodge")+
  geom_text(size = 3, position = position_dodge(width = 0.9), vjust = -0.1)+
  labs(x = "Trip month",
       y = "Average trip length",
       title = "Average trip length")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=0.5))

tripdata %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(min = min(trip_length), max = max(trip_length), 
            mean = mean(trip_length), median = median(trip_length),
            sd = sd(trip_length))

mean_01 <- tripdata %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(mean = mean(trip_length))

median_01 <- tripdata %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(median = median(trip_length))

tripdata %>% 
  filter(trip_length < quantile(trip_length, 0.99)) %>% 
  ggplot(aes(as.numeric(trip_length), fill = member_casual))+
  geom_histogram(binwidth =  1)+
  geom_point(data = mean_01, aes(x =mean, y = 2000), col = "red")+
  geom_point(data = median_01, aes(x =median, y = 2000), col = "blue")+
  facet_grid(~member_casual~rideable_type)+
  labs(x = "Trip length",
       y = "No of rides",
       title = "Title")

tripdata %>% 
  filter(trip_length < quantile(trip_length, 0.99)) %>% 
  ggplot(aes(as.numeric(trip_length), fill = member_casual))+
  geom_histogram(binwidth =  1, position = "dodge")+
  facet_wrap(~day_of_week)+
  labs(x = "Trip length",
       y = "No of rides",
       title = "Title")

tripdata %>% 
  filter(trip_length < quantile(trip_length, 0.99)) %>% 
  ggplot(aes(as.numeric(trip_length), fill = member_casual))+
  geom_histogram(binwidth =  1, position = "dodge")+
  facet_wrap(~trip_month)+
  labs(x = "Trip length",
       y = "No of rides",
       title = "Title")

tripdata %>% 
  filter(trip_length < quantile(trip_length, 0.99)) %>% 
  ggplot(aes(as.numeric(trip_length), fill = member_casual))+
  geom_histogram(binwidth =  1, position = "dodge")+
  facet_wrap(~rideable_type)+
  labs(x = "Trip length",
       y = "No of rides",
       title = "Title")

# Hour, day, member_casual
ggplot(data = tripdata, aes(x = trip_start_hour, fill = member_casual))+
  theme_bw()+
  geom_bar(alpha = 0.7)+
  labs(x = "Hour",
       y = "No of rides",
       title = "Title")+
  facet_grid(~day_of_week~member_casual)


# OR
ggplot(data = tripdata, aes(x = trip_start_hour, fill = member_casual))+
  theme_bw()+
  geom_bar(alpha = 0.7, position = "dodge")+
  labs(x = "Hour",
       y = "No of rides",
       title = "Title")+
  facet_wrap(~day_of_week)

# OR
ggplot(data = tripdata, aes(x = trip_start_hour, group = member_casual, col = member_casual))+
  theme_bw()+
  geom_line(stat = 'count', lwd = 1)+
  labs(x = "Hour",
       y = "No of rides",
       title = "Number of rides by hour")+
  facet_wrap(~day_of_week)


ggplot(data = tripdata, aes(x = trip_start_hour, group = member_casual, col = member_casual))+
  theme_bw()+
  geom_line(stat = 'count', lwd = 1)+
  labs(x = "Hour",
       y = "No of rides",
       title = "Number of rides by hour")+
  facet_wrap(~rideable_type)

ggplot(data = tripdata, aes(x = trip_start_hour, group = member_casual, col = member_casual))+
  theme_bw()+
  geom_line(stat = 'count', lwd = 1)+
  labs(x = "Hour",
       y = "No of rides",
       title = "Number of rides by hour")+
  facet_grid(~day_of_week~rideable_type)

ggplot(data = tripdata, aes(x = trip_start_hour, group = member_casual, col = member_casual))+
  theme_bw()+
  geom_line(stat = 'count', lwd = 1)+
  labs(x = "Hour",
       y = "No of rides",
       title = "Number of rides by hour")+
  facet_grid(~trip_month~day_of_week)

# trip length, day, member_casual
ggplot(data = tripdata, aes(x = day_of_week, fill = member_casual))+
  theme_bw()+
  geom_bar(alpha = 0.7)+
  geom_text(stat='count',position = position_stack(vjust = 0.5), size = 4, aes(label=..count..))+
  labs(x = "Hour",
       y = "No of rides",
       title = "Day of week")+
  facet_grid(~rideable_type~member_casual)
 

# Month, day_of_week, member_casual
ggplot(data = tripdata, aes(x = day_of_week, group = member_casual, col = member_casual))+
  theme_bw()+
  geom_line(stat = 'count', lwd = 1)+
  labs(x = "Day of week",
       y = "No of rides",
       title = "Number of rides by hour")+
  facet_wrap(~trip_month)

# Month, Hour, member_casual
ggplot(data = tripdata, aes(x = trip_start_hour, group = member_casual, col = member_casual))+
  theme_bw()+
  geom_line(stat = 'count', lwd = 1)+
  labs(x = "Hour",
       y = "No of rides",
       title = "Number of rides by hour")+
  facet_wrap(~trip_month)

# Month, Rideable Type, member_casual
ggplot(data = tripdata, aes(x = rideable_type, fill = member_casual))+
  theme_bw()+
  geom_bar(position = "dodge")+
  geom_text(stat = 'count', size = 3, position = position_dodge(width = 0.9), vjust = -0.1, aes(label=..count..))+
  labs(x = "Rideable Type",
       y = "No of rides",
       title = "Number of rides by hour")+
  facet_wrap(~trip_month)




# Raw

ggplot(data =tripdata, aes(x = day_of_week))+
  geom_bar() 

ggplot(data =tripdata, aes(x = member_casual, fill = rideable_type))+
  geom_bar()

ggplot(data =tripdata, aes(x = day_of_week, fill =member_casual))+
  geom_bar()

ggplot(data =tripdata, aes(x = rideable_type, fill = member_casual))+
  geom_bar()

# Rides distributed over hour of the day
ggplot(data =tripdata, aes(x = trip_start_hour, fill = member_casual))+
  theme_bw()+
  geom_bar(position = "dodge")+
  labs(x = "Hour of day",
       y = "No of rides",
       title = "Rides distributed over hour of the day",
       subtitle = "Hour of day vs No of rides")

#Rides distributed over day of the week
ggplot(data =tripdata, aes(x = day_of_week, fill = member_casual))+
  theme_bw()+
  geom_bar(position = "dodge")+
  labs(x = "Day of week",
       y = "No of rides",
       title = "Rides distributed over day of the week",
       subtitle = "Day of week vs No of rides")


#
ggplot(data =tripdata, aes(x = trip_length))+
  theme_bw()+
  geom_histogram()
 
# Number of rides by hour


