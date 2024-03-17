#### Preamble ####
# Purpose: Clean GSS Dataset
# Author: Mingjia Chen, Catherine Punnoose, Tianen (Evan) Hao
# Date: 11 March 2024 
# Contact: mingjia.chen@mail.utoronto.ca 
# License: MIT
# Pre-requisites: R 4.3.2, cropped_data.csv

#### Workspace setup ####
library(janitor)
library(tidyverse)
library(knitr)
library(dplyr)
library(kableExtra)
library(here)

#### Clean data ####
# load dataset that has been cropped for analysis
data <- read.csv(here::here("data/raw_data/cropped_data.csv"))

# the first column of the dataset is the names of the rows
# so we renamed the row names based on first column and 
#deleted the column at the end
row_number <- nrow(data)
for (i in 1:row_number) {
  rownames(data)[i] = data[i,1]
}

# Delete the column
data <- data[c(2:ncol(data))]

# Get rid of the first letter X for each of the years
colnames(data) <- gsub("X", "", colnames(data))

# Rename part of the row names 
#(i.e. change .i Inapplicable into Inapplicable for better
#data representation) 
rownames(data)[1:4] <- c("Inapplicable","No answer","Do not Know/Cannot Choose","Skipped on Web")

# Save the cleaned data
write.csv(data, "data/analysis_data/cleaned_data.csv")

#### Categorize Data #####
colnames(data)[1] <- "work_hours"
colnames(data) <- gsub("X", "", colnames(data))
cate_data <- data

year <- colnames(data)
Hours <- c("No Response","0-20","21-40","41-60","61-80","80+","Total")

col_number <- ncol(cate_data) -1

#summarize all the responses in one category
filtered_df1 <- cate_data %>%
  filter(work_hours < 20) 

tweenties <- rep(0, 35)
for(i in 1:col_number){
  tweenties[i] <- sum(filtered_df1 [, i+1])
}

filtered_df2 <- cate_data %>%
  filter(work_hours < 40) %>%
  filter(work_hours > 20)

forties <- rep(0, 35)
for(i in 1:col_number){
  forties[i] <- sum(filtered_df2[, i+1])
}

filtered_df3 <- cate_data %>%
  filter(work_hours < 60) %>%
  filter(work_hours > 40)

sixties <- rep(0, 35)
for(i in 1:col_number){
  sixties[i] <- sum(filtered_df3 [, i+1])
}

filtered_df4 <- cate_data %>%
  filter(work_hours < 80) %>%
  filter(work_hours > 60)

eighties <- rep(0, 35)
for(i in 1:col_number){
  eighties[i] <- sum(filtered_df4[, i+1])
}

filtered_df5 <- cate_data %>%
  filter(work_hours > 80) 
filtered_df5 <- filtered_df5[5:14,]

more <- rep(0, 35)
for(i in 1:col_number){
  more[i] <- sum(filtered_df5[, i+1])
}

filtered_df6 <- cate_data %>%
  filter(work_hours > 80) 
filtered_df6 <- filtered_df6[1:4,]

No_Response <- rep(0, 35)
for(i in 1:col_number){
  No_Response[i] <- sum(filtered_df6[, i+1])
}

#remove unwanted dataframes
rm(filtered_df1,filtered_df2,filtered_df3,filtered_df4,filtered_df5,filtered_df6)

total <- data[95,2:35]

#combine all the data
cate_data <- rbind(No_Response,tweenties,forties,sixties,eighties,more,total)
cate_data <- data.frame(cbind(Hours,cate_data))
colnames(cate_data) <- year

write.csv(cate_data,"data/analysis_data/cleaned_categorized_data.csv")
