#### Preamble ####
# Purpose: GSS Tables and Graphs for the Paper
# Author: Mingjia Chen, Catherine Punnoose, Tianen (Evan) Hao
# Date: 11 March 2024 
# Contact: mingjia.chen@mail.utoronto.ca 
# License: MIT
# Pre-requisites: R 4.3.2, cleaned_Data.csv, cleaned_categorized_data.csv

#download the packages if necessary, then load the packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(janitor,here,dplyr,tidyverse,knitr,kableExtra,ggplot2)

# Load the psych package
library(janitor)
library(tidyverse)
library(knitr)
library(dplyr)
library(kableExtra)
library(here)
library(ggplot2)

# Overview of the Categorized Data 
data <- read.csv(here::here("inputs/data/cleaned_categorized_data.csv"))
# Delete the column
data <- data[c(2:ncol(data))]
colnames(data)[1] <- "Work Hours/Years"
# Get rid of the first letter X for each of the years
colnames(data) <- gsub("X", "", colnames(data))

data_selected <- data[c("Work Hours/Years", "1998","2008","2021","Total")]
kable(data_selected,row.names = FALSE)|>
  kable_styling() |>
  row_spec(6, hline_after = TRUE)

# Overview of the 1998 Survey Data 
data <- read.csv(here:here("inputs/data/cleaned_categorized_data.csv"))
data <- data[1:6,]

ggplot(data, aes(x = "", y = X1998, fill = work_hours)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Set3") +  # Use a color palette
  theme_void() +  # Remove background and gridlines
  labs(title = "1998 Survey Results") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

# Overview of the 2008 Survey Data 
data <- read.csv(here:here("inputs/data/cleaned_categorized_data.csv"))
data <- data[1:6,]

ggplot(data, aes(x = "", y = X2008, fill = work_hours)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Set3") +  # Use a color palette
  theme_void() +  # Remove background and gridlines
  labs(title = "2008 Survey Results") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

# Overview of the 2021 Survey Data 
data <- read.csv(here:here("inputs/data/cleaned_categorized_data.csv"))
data <- data[1:6,]

ggplot(data, aes(x = "", y = X2021, fill = work_hours)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Set3") +  # Use a color palette
  theme_void() +  # Remove background and gridlines
  labs(title = "2021 Survey Results") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

# Average Working Hours Comparison between Financial Events
data <- read.csv(here:here("inputs/data/cleaned_data.csv"))
# Get rid of the first letter X for each of the years
colnames(data)[1] <- "work_hours"
colnames(data) <- gsub("X", "", colnames(data))

#Histogram of average working hours for 2008, 2021, 2022, and Total
hist_data <- select(data, work_hours,"2010","2018","2021")
hist_data <- hist_data[5:94,]

num <- 3 #number of years
sum <- rep(0,num)
total_people <- rep(0,num)
for(i in 1:num){
  total_people[i] <- sum(hist_data[, i+1])
}
hist_data[, 1] <- sapply(hist_data[, 1], as.numeric)
hist_data[90,1] <- 90

sum[1] <- sum(hist_data$"2010" * hist_data$work_hours)
sum[2] <- sum(hist_data$"2018" * hist_data$work_hours)
sum[3] <- sum(hist_data$"2021" * hist_data$work_hours)


averages <- round(sum/total_people) 
years <- c("2010","2018","2021")

average_hours <- data.frame(cbind(averages,years))

ggplot(average_hours,aes(x=years,y=averages)) +
  geom_bar(stat="identity") + 
  theme_minimal() + # Make the theme neater
  labs(x = "Year", y = "Average Working Hour") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom")

# Categorical Working Hour Comparison Between the Years
data <- read.csv(here:here("inputs/data/cleaned_categorized_data.csv"))
data <- data[c(2:ncol(data))]
# Get rid of the first letter X for each of the years
colnames(data) <- gsub("X", "", colnames(data))

Data_1998 <- select(data,work_hours,`1998`)
ggplot(Data_1998,aes(x=work_hours,y=`1998`)) +
  geom_bar(stat="identity") + 
  theme_minimal() + # Make the theme neater
  labs(x = "Working Hours", y = "Number of Reports") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom")

Data_2008 <- select(data,work_hours,`2008`)
ggplot(Data_2008,aes(x=work_hours,y=`2008`)) +
  geom_bar(stat="identity") + 
  theme_minimal() + # Make the theme neater
  labs(x = "Working Hours", y = "Number of Reports") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom")

Data_2021 <- select(data,work_hours,`2021`)
ggplot(Data_2021,aes(x=work_hours,y=`2021`)) +
  geom_bar(stat="identity") + 
  theme_minimal() + # Make the theme neater
  labs(x = "Working Hours", y = "Number of Reports") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom")

Data_Total <- select(data,work_hours,Total)
ggplot(Data_Total,aes(x=work_hours,y= Total)) +
  geom_bar(stat="identity") + 
  theme_minimal() + # Make the theme neater
  labs(x = "Working Hours", y = "Number of Reports") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom")

# Nonresponse Rates
# Calculate percentages
data <- read.csv(here:here("inputs/data/cleaned_categorized_data.csv"))
data <- data[c(2:ncol(data))]
data <- data[1:6,]

#colnames(data) <- paste(colnames(data),"Year",sep="_") 

colnames(data)<- as.character(colnames(data))

#data_long <- data %>% 
#  pivot_longer(cols = everything(), names_to = "WorkHours", values_to #= "Response")

# Select the 'No Response' row and remove the first column which is not needed for the plot
no_response_data <- data[data$work_hours == 'No Response', -1]

# We also need to remove the 'Total' column as we did in the Python example
no_response_data <- no_response_data[-length(no_response_data)]

# Calculate total responses per year excluding the 'work_hours' and 'Total' columns

data <- read.csv("cleaned_categorized_data.csv")
data <- data[c(3:ncol(data))]

total_responses_per_year <- as.vector(data[7,])
total_responses_per_year <- unlist(total_responses_per_year)

# Calculate non-response rate as a percentage
non_response_rate <- (no_response_data / total_responses_per_year) * 100

years <- colnames(non_response_rate)
years <- gsub("X", "", years)

non_response_rate <-as.vector(non_response_rate)
non_response_rate <-unlist(non_response_rate)
non_response_data <- data.frame(years,non_response_rate)

# Plot using ggplot2
ggplot(non_response_data, aes(x = years,y=non_response_rate)) +
  geom_bar(stat = "identity",fill = "skyblue") +
  theme_minimal() +
  labs(title = "Non-Response Rate by Year", x = "Year", y = "Percentage of Non-Responses") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

