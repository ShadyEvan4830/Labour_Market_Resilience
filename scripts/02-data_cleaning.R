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
library(haven)

#### Clean data ####
# load dataset that has been cropped for analysis
data <- read.csv(here::here("data/raw_data/cropped_data.csv"))

colnames(data)[1] <- "work_hours"

# Get rid of the first letter X for each of the years
colnames(data) <- gsub("X", "", colnames(data))

# Rename part of the row names 
#(i.e. change .i Inapplicable into Inapplicable for better
#data representation) 
data[1:4,1] <- c("Inapplicable","No answer","Do not Know/Cannot Choose","Skipped on Web")

# Save the cleaned data
write.csv(data, "data/analysis_data/cleaned_data.csv")

#### Categorize Data #####
cate_data <- data

year <- colnames(data)
Hours <- c("No Response","0-20","21-40","41-60","61-80","80+","Total")

col_number <- ncol(cate_data)-1

#summarize all the responses in one category
#0-20
filtered_df1 <- cate_data[5:25,]

tweenties <- rep(0, 36)
for(i in 1:col_number){
  tweenties[i] <- sum(filtered_df1 [, i+1])
}

#21-40
filtered_df2 <- cate_data[26:45,]

forties <- rep(0, 36)
for(i in 1:col_number){
  forties[i] <- sum(filtered_df2[, i+1])
}

#41-60
filtered_df3 <- cate_data[46:65,]

sixties <- rep(0, 36)
for(i in 1:col_number){
  sixties[i] <- sum(filtered_df3 [, i+1])
}

#61-80
filtered_df4 <- cate_data[66:85,]

eighties <- rep(0, 36)
for(i in 1:col_number){
  eighties[i] <- sum(filtered_df4[, i+1])
}

# 80+
filtered_df5 <- cate_data[86:94,]

more <- rep(0, 36)
for(i in 1:col_number){
  more[i] <- sum(filtered_df5[, i+1])
}

filtered_df6 <- cate_data[1:4,]

No_Response <- rep(0, 36)
for(i in 1:col_number){
  No_Response[i] <- sum(filtered_df6[, i+1])
}

#remove unwanted dataframes
rm(filtered_df1,filtered_df2,filtered_df3,filtered_df4,filtered_df5,filtered_df6)

total <- data[95,2:36]


#combine all the data
cate_data <- rbind(No_Response,tweenties,forties,sixties,eighties,more,total)
cate_data <- data.frame(cbind(Hours,cate_data))
colnames(cate_data) <- year


write.csv(cate_data,"data/analysis_data/cleaned_categorized_data.csv")

##### Additional Data Set for Result Section ######
#Code Referenced from GSS website
#https://gssdataexplorer.norc.org/MyGSS
library(foreign)
read.dct <- function(dct, labels.included = "yes") {
  temp <- readLines(dct)
  temp <- temp[grepl("_column", temp)]
  switch(labels.included,
         yes = {
           pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+)[a-z]\\s+(.*)"
           classes <- c("numeric", "character", "character", "numeric", "character")
           N <- 5
           NAMES <- c("StartPos", "Str", "ColName", "ColWidth", "ColLabel")
         },
         no = {
           pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+).*"
           classes <- c("numeric", "character", "character", "numeric")
           N <- 4
           NAMES <- c("StartPos", "Str", "ColName", "ColWidth")
         })
  temp_metadata <- setNames(lapply(1:N, function(x) {
    out <- gsub(pattern, paste("\\", x, sep = ""), temp)
    out <- gsub("^\\s+|\\s+$", "", out)
    out <- gsub('\"', "", out, fixed = TRUE)
    class(out) <- classes[x] ; out }), NAMES)
  temp_metadata[["ColName"]] <- make.names(gsub("\\s", "", temp_metadata[["ColName"]]))
  temp_metadata
}

read.dat <- function(dat, metadata_var, labels.included = "yes") {
  read.table(dat, col.names = metadata_var[["ColName"]])
}


GSS_metadata <- read.dct(here::here("data/raw_data/GSS.dct"))
GSS_ascii <- read.dat(here::here("data/raw_data/GSS.dat"), GSS_metadata)
attr(GSS_ascii, "col.label") <- GSS_metadata[["ColLabel"]]
additional_data <- GSS_ascii

additional_data <- select(additional_data, AGE, SEX, YEAR)

write.csv(additional_data,"data/analysis_data/supplementary_data.csv")



