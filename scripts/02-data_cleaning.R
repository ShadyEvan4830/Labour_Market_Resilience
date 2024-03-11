#### Preamble ####
# Purpose: Clean Study 2 Dataset
# Author: Mingjia Chen, Hadi Ahmad, Tianen (Evan) Hao
# Date: 11 February 2024
# Contact: mingjia.chen@mail.utoronto.ca 
# License: MIT
# Pre-requisites: R 4.3.2

#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(haven)

#### Clean data ####
data <- read_sav("inputs/data/FW satisfaction-Study 2-data.sav")

data <- mutate(data,
               jobsat_3r = case_when(
                 jobsat_3 == 1 ~ 7,
                 jobsat_3 == 2 ~ 6,
                 jobsat_3 == 3 ~ 5,
                 jobsat_3 == 4 ~ 4,
                 jobsat_3 == 5 ~ 3,
                 jobsat_3 == 6 ~ 2,
                 jobsat_3 == 7 ~ 1,
                 TRUE ~ jobsat_3
               ),
               jobsat_5r = case_when(
                 jobsat_5 == 1 ~ 7,
                 jobsat_5 == 2 ~ 6,
                 jobsat_5 == 3 ~ 5,
                 jobsat_5 == 4 ~ 4,
                 jobsat_5 == 5 ~ 3,
                 jobsat_5 == 6 ~ 2,
                 jobsat_5 == 7 ~ 1,
                 TRUE ~ jobsat_5
               ),
               jobsat2_3r = case_when(
                 jobsat2_3 == 1 ~ 7,
                 jobsat2_3 == 2 ~ 6,
                 jobsat2_3 == 3 ~ 5,
                 jobsat2_3 == 4 ~ 4,
                 jobsat2_3 == 5 ~ 3,
                 jobsat2_3 == 6 ~ 2,
                 jobsat2_3 == 7 ~ 1,
                 TRUE ~ jobsat2_3
               ),
               jobsat2_5r = case_when(
                 jobsat2_5 == 1 ~ 7,
                 jobsat2_5 == 2 ~ 6,
                 jobsat2_5 == 3 ~ 5,
                 jobsat2_5 == 4 ~ 4,
                 jobsat2_5 == 5 ~ 3,
                 jobsat2_5 == 6 ~ 2,
                 jobsat2_5 == 7 ~ 1,
                 TRUE ~ jobsat2_5
               ),
               selfest_3r = case_when(
                 selfest_3 == 1 ~ 7,
                 selfest_3 == 2 ~ 6,
                 selfest_3 == 3 ~ 5,
                 selfest_3 == 4 ~ 4,
                 selfest_3 == 5 ~ 3,
                 selfest_3 == 6 ~ 2,
                 selfest_3 == 7 ~ 1,
                 TRUE ~ selfest_3
               ),
               selfest_5r = case_when(
                 selfest_5 == 1 ~ 7,
                 selfest_5 == 2 ~ 6,
                 selfest_5 == 3 ~ 5,
                 selfest_5 == 4 ~ 4,
                 selfest_5 == 5 ~ 3,
                 selfest_5 == 6 ~ 2,
                 selfest_5 == 7 ~ 1,
                 TRUE ~ selfest_5
               ),
               selfest_8r = case_when(
                 selfest_8 == 1 ~ 7,
                 selfest_8 == 2 ~ 6,
                 selfest_8 == 3 ~ 5,
                 selfest_8 == 4 ~ 4,
                 selfest_8 == 5 ~ 3,
                 selfest_8 == 6 ~ 2,
                 selfest_8 == 7 ~ 1,
                 TRUE ~ selfest_8
               ),
               selfest_9r = case_when(
                 selfest_9 == 1 ~ 7,
                 selfest_9 == 2 ~ 6,
                 selfest_9 == 3 ~ 5,
                 selfest_9 == 4 ~ 4,
                 selfest_9 == 5 ~ 3,
                 selfest_9 == 6 ~ 2,
                 selfest_9 == 7 ~ 1,
                 TRUE ~ selfest_9
               ),
               selfest_10r = case_when(
                 selfest_10 == 1 ~ 7,
                 selfest_10 == 2 ~ 6,
                 selfest_10 == 3 ~ 5,
                 selfest_10 == 4 ~ 4,
                 selfest_10 == 5 ~ 3,
                 selfest_10 == 6 ~ 2,
                 selfest_10 == 7 ~ 1,
                 TRUE ~ selfest_10
               ),
               fw_16r = case_when(
                 fw_16 == 1 ~ 6,
                 fw_16 == 2 ~ 5,
                 fw_16 == 3 ~ 4,
                 fw_16 == 4 ~ 3,
                 fw_16 == 5 ~ 2,
                 fw_16 == 6 ~ 1,
                 TRUE ~ fw_16
               ),
               fw_17r = case_when(
                 fw_17 == 1 ~ 6,
                 fw_17 == 2 ~ 5,
                 fw_17 == 3 ~ 4,
                 fw_17 == 4 ~ 3,
                 fw_17 == 5 ~ 2,
                 fw_17 == 6 ~ 1,
                 TRUE ~ fw_17
               ),
               fw_21r = case_when(
                 fw_21 == 1 ~ 6,
                 fw_21 == 2 ~ 5,
                 fw_21 == 3 ~ 4,
                 fw_21 == 4 ~ 3,
                 fw_21 == 5 ~ 2,
                 fw_21 == 6 ~ 1,
                 TRUE ~ fw_21
               ),
               fw_22r = case_when(
                 fw_22 == 1 ~ 6,
                 fw_22 == 2 ~ 5,
                 fw_22 == 3 ~ 4,
                 fw_22 == 4 ~ 3,
                 fw_22 == 5 ~ 2,
                 fw_22 == 6 ~ 1,
                 TRUE ~ fw_22
               )
)

# Compute means for each group of variables
data$FWDfw <- rowMeans(data[, c("fw_1", "fw_2", "fw_3", "fw_4", "fw_5")])
data$FWDagency <- rowMeans(data[, c("fw_6", "fw_7", "fw_8", "fw_9")])
data$FWDmoral <- rowMeans(data[, c("fw_10", "fw_11", "fw_12", "fw_13", "fw_14", "fw_15")])
data$FWDpower <- rowMeans(data[, c("fw_16r", "fw_17r", "fw_18")])
data$FWDresp <- rowMeans(data[, c("fw_19", "fw_20")])
data$FWDlimit <- rowMeans(data[, c("fw_21r", "fw_22r")])
data$FWDall <- rowMeans(data[, c("fw_1", "fw_2", "fw_3", "fw_4", "fw_5", 
                                 "fw_10", "fw_11", "fw_12", "fw_13", "fw_14", "fw_15", 
                                 "fw_19", "fw_20", "fw_16r", "fw_17r", "fw_18", 
                                 "fw_21r", "fw_22r")])
data$FWDfwagency <- rowMeans(data[, c("fw_1", "fw_2", "fw_3", "fw_4", "fw_5", 
                                      "fw_6", "fw_7", "fw_8", "fw_9")])
data$FWself <- rowMeans(data[, c("fw_6", "fw_18", "fw_8", "fw_21r", "fw_3", "fw_7", "fw_22r", "fw_9")])
data$FWgeneral <- rowMeans(data[, c("fw_1", "fw_2", "fw_4", "fw_5", "fw_10", "fw_11", "fw_12", "fw_13",
                                    "fw_14", "fw_15", "fw_16r", "fw_17r", "fw_19", "fw_20")])
data$jobsatT1 <- rowMeans(data[, c("jobsat_1","jobsat_2","jobsat_3r","jobsat_4","jobsat_5r")])
data$jobsatT2 <- rowMeans(data[, c("jobsat2_1", "jobsat2_2","jobsat2_3r", "jobsat2_4","jobsat2_5r")])

data$jobautT1 <- rowMeans(data[, c("jobaut_3", "jobaut_4","jobaut_5")])
data$jobautT2 <- rowMeans(data[, c("jobaut2_3", "jobaut2_4","jobaut2_5")])


# Assign variable labels
variable.labels <- c("FWDfw" = "FW T1 Free will subscale",
                     "FWDagency" = "FW T1 personal agency subscale",
                     "FWDmoral" = "FW T1 Moral responsibility subscale",
                     "FWDpower" = "FW T1 Higher power control subscale",
                     "FWDresp" = "FW T1 Personal responsibility subscale",
                     "FWDlimit" = "FW T1 Personal limitations subscale",
                     "FWDall" = "FW T1 both scales",
                     "FWDfwagency" = "FW T1 agency and free will subscales",
                     "FWself" = "Free will beliefs - personal T1",
                     "FWgeneral" = "Free will beliefs - general T1",
                     "jobsatT1" = "Job satisfaction(T1)",
                     "jobsatT2" = "Job satisfaction(T2)",
                     "jobautT1" = "Job autonomy(T1)",
                     "jobautT2" = "Job autonomy(T2)"
                     )

# Assign variable labels to the variables
attr(data$FWDfw, "label") <- variable.labels["FWDfw"]
attr(data$FWDagency, "label") <- variable.labels["FWDagency"]
attr(data$FWDmoral, "label") <- variable.labels["FWDmoral"]
attr(data$FWDpower, "label") <- variable.labels["FWDpower"]
attr(data$FWDresp, "label") <- variable.labels["FWDresp"]
attr(data$FWDlimit, "label") <- variable.labels["FWDlimit"]
attr(data$FWDall, "label") <- variable.labels["FWDall"]
attr(data$FWDfwagency, "label") <- variable.labels["FWDfwagency"]
attr(data$FWself, "label") <- variable.labels["FWself"]
attr(data$FWgeneral, "label") <- variable.labels["FWgeneral"]
attr(data$jobsatT1, "label") <- variable.labels["jobsatT1"]
attr(data$jobsatT2, "label") <- variable.labels["jobsatT2"]
attr(data$jobautT1, "label") <- variable.labels["jobautT1"]
attr(data$jobautT2, "label") <- variable.labels["jobautT2"]



#### Save data ####
write.csv(data, "outputs/data/FW_satisfaction-Study_2-data.csv")


#### Create summary tables for descriptive graphs

#satisfaction (T1)
jobsat_data <- data[,c("jobsat_1","jobsat_2","jobsat_3r","jobsat_4","jobsat_5r")]

# The Code below is generated by chatGPT 4.0 (see details in LLM usage)

# Convert data to long format for ggplot2
data_long <- jobsat_data %>% 
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response")

# Calculate frequencies
data_freq <- data_long %>% 
  group_by(Question, Response) %>% 
  summarise(Frequency = n()) %>% 
  ungroup()

# Calculate percentages
data_freq <- data_freq %>% 
  group_by(Question) %>% 
  mutate(Percent = Frequency / sum(Frequency) * 100) %>% 
  ungroup()

data_freq  <- mutate(data_freq ,
                     Question = case_when(
                       Question == "jobsat_1" ~ "Scale 1",
                       Question == "jobsat_2" ~ "Scale 2",
                       Question == "jobsat_3r" ~ "Scale 3",
                       Question == "jobsat_4" ~ "Scale 4",
                       Question == "jobsat_5r" ~ "Scale 5",
                       TRUE ~  Question 
                     )
)
write.csv(data_freq, "outputs/data/jobsatT1_data_freq.csv")

#satisfaction (T2)
jobsat2_data <- data[,c("jobsat2_1","jobsat2_2","jobsat2_3r","jobsat2_4","jobsat2_5r")]

# The Code below is generated by chatGPT 4.0 (see details in LLM usage)

# Convert data to long format for ggplot2
data2_long <- jobsat2_data %>% 
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response")

# Calculate frequencies
data2_freq <- data2_long %>% 
  group_by(Question, Response) %>% 
  summarise(Frequency = n()) %>% 
  ungroup()

# Calculate percentages
data2_freq <- data2_freq %>% 
  group_by(Question) %>% 
  mutate(Percent = Frequency / sum(Frequency) * 100) %>% 
  ungroup()

data2_freq  <- mutate(data2_freq ,
                      Question = case_when(
                        Question == "jobsat2_1" ~ "Scale 1",
                        Question == "jobsat2_2" ~ "Scale 2",
                        Question == "jobsat2_3r" ~ "Scale 3",
                        Question == "jobsat2_4" ~ "Scale 4",
                        Question == "jobsat2_5r" ~ "Scale 5",
                        TRUE ~  Question 
                      )
)
write.csv(data2_freq, "outputs/data/jobsatT2_data_freq.csv")


#Belief in Free will
fw_data <- data[,c("fw_1", "fw_2", "fw_3", "fw_4", "fw_5", 
                        "fw_6", "fw_7", "fw_8", "fw_9")]

# The Code below is generated by chatGPT 4.0 (see details in LLM usage)

# Convert data to long format for ggplot2
FWdata_long <- fw_data%>% 
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response")

# Calculate frequencies
FWdata_freq <- FWdata_long %>% 
  group_by(Question, Response) %>% 
  summarise(Frequency = n()) %>% 
  ungroup()

# Calculate percentages
FWdata_freq <- FWdata_freq %>% 
  group_by(Question) %>% 
  mutate(Percent = Frequency / sum(Frequency) * 100) %>% 
  ungroup()

FWdata_freq <- mutate(FWdata_freq ,
                      Question = case_when(
                        Question == "fw_1" ~ "Scale 1",
                        Question == "fw_2" ~ "Scale 2",
                        Question == "fw_3" ~ "Scale 3",
                        Question == "fw_4" ~ "Scale 4",
                        Question == "fw_5" ~ "Scale 5",
                        Question == "fw_6" ~ "Scale 6",
                        Question == "fw_7" ~ "Scale 7",
                        Question == "fw_8" ~ "Scale 8",
                        Question == "fw_9" ~ "Scale 9",
                        TRUE ~  Question 
                      )
)
write.csv(FWdata_freq, "outputs/data/FW_data_freq.csv")

