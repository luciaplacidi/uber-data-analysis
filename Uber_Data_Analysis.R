# Load packages
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(tidyverse)
library(DT)
library(scales)
library(viridis)
library(viridisLite)

# Colour vector
colors = c("#EDAFB8", "#E8DDB5", "#BFCBC3", "#95B8D1", "#7E91AC", "#666A86", "#333333")
colors

# Import data
apr <- read.csv("uber-raw-data-apr14.csv")
may <- read.csv("uber-raw-data-may14.csv")
jun <- read.csv("uber-raw-data-jun14.csv")
jul <- read.csv("uber-raw-data-jul14.csv")
aug <- read.csv("uber-raw-data-aug14.csv")
sep <- read.csv("uber-raw-data-sep14.csv")

# Combine data
data <- rbind(apr, may, jun, jul, aug, sep)

# Preview data
head(data)

# Appropriate formatting for date.time column
data$Date.Time <- as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S")

data$Time <- format(as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S"),
                    format="%H:%M:%S")

data$Date.Time <- ymd_hms(data$Date.Time)

# Separate day, month, year into separate columns
data$day <- factor(day(data$Date.Time))
data$month <- factor(month(data$Date.Time, label = TRUE))
data$year <- factor(year(data$Date.Time))
data$dayofweek <- factor(wday(data$Date.Time, label = TRUE))

# Add time variables
data$second = factor(second(hms(data$Time)))
data$minute = factor(minute(hms(data$Time)))
data$hour = factor(hour(hms(data$Time)))

# Plot trips by hour
hour_data <- data%>%
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 
datatable(hour_data)

ggplot(hour_data, aes(hour, Total)) + 
  geom_bar(stat="identity", 
           fill="#95B8D1") + 
  ggtitle("Trips Every Hour") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)

# Plot trips by hour and month
month_hour_data <- data %>% group_by(month, hour) %>%  dplyr::summarize(Total = n())

ggplot(month_hour_data, aes(hour, Total, fill=month)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Hour and Month") + 
  scale_y_continuous(labels = comma)

# Plot trips during every day of the month
day_data <- data %>% group_by(day) %>% dplyr::summarize(Trips = n())
day_data

ggplot(day_data, aes(day, Trips)) + 
  geom_bar(stat = "identity", fill = "#95B8D1") +
  ggtitle("Trips by day of the month") + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma)

# Trips by day of the week and month
day_month_data <- data %>% group_by(dayofweek, month) %>% dplyr::summarize(Trips = n())
day_month_data

ggplot(day_month_data, aes(dayofweek, Trips, fill = month)) + 
  geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
  ggtitle("Trias by Day and Month") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

# Number of trips during months of the year
month_data <- data %>%
  group_by(month) %>%
  dplyr::summarize(Total = n()) 
datatable(month_data)

ggplot(month_data, aes(month, Total, fill = month)) + 
  geom_bar(stat = "Identity") + 
  ggtitle("Trips In a Month") + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

# Number of trips taken on day of the week and month
month_weekday <- data %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())

ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)


# Heatmap Visualizations by Hour, Day, Month

# Heatmap by Day and Hour

day_hour_data <- data %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())
datatable(day_hour_data)

ggplot(day_hour_data, aes(day, hour, fill = Total)) + 
  geom_tile(color = "white") + 
  ggtitle("Heat Map by Hour and Day")

# Heatmap by Day and Month

month_day_data <- data %>% group_by(month, day) %>% dplyr::summarize(Trips = n())
month_day_data

ggplot(month_day_data, aes(day, month, fill = Trips)) + 
  geom_tile(color = "white") + 
  ggtitle("Heat Map by Month and Day")


# Heatmap by Week and Month
ggplot(day_month_data, aes(dayofweek, month, fill = Trips)) + 
  geom_tile(color = "white") + 
  ggtitle("Heat Map by Month and Day")


# Creating a Map Visualization of NYC trips

manhattan_map <- get_stamenmap(bbox = c(left = -74.0479, bottom = 40.6829, right = -73.9067, top = 40.81),
                               zoom = 14, maptype = "toner-lite")

ggmap(manhattan_map) +
  scale_fill_viridis(option = 'plasma') +
  geom_bin2d(data=data, aes(x=Lon, y=Lat), bins=60, alpha=0.6) +
  labs(x='Longitude', y='Latitude', fill='white')
