#### Preamble ####
# Purpose: Study 2 Table Creating
# Author: Mingjia Chen, Hadi Ahmad, Tianen (Evan) Hao
# Date: 17 February 2024
# Contact: mingjia.chen@mail.utoronto.ca 
# License: MIT
# Pre-requisites: R 4.3.2

#download the packages if necessary, then load the packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(haven, psych,dplyr,tidyverse, knitr,kableExtra)

# Load the psych package
library(psych)
library(dplyr)
library(tidyverse)
library(knitr)

data <- read_csv("FW_satisfaction-Study_2-data.csv")

##### Table 1. Study 2 Means, Standard Deviations, and Correlations################################################################################
# Select the variables for correlation analysis
selected_variables <-data[, c("jobsat1", "jobsat2", "FWDfwagency", "jobaut", "jobaut2", "locus", "ess_kind", "selfest", "selfeff", "selfcontrol")]

# Compute pairwise correlations
correlation_matrix <- cor(selected_variables, use = "pairwise.complete.obs")

# modifying the format so it matches to the correlation table like in paper
## making the upper right part zero

## Add Mean and SD
M <- c(mean(selected_variables$jobsat1), mean(selected_variables$jobsat2,na.rm = TRUE), mean(selected_variables$FWDfwagency),
       mean(selected_variables$jobaut,na.rm = TRUE),mean(selected_variables$jobaut2,na.rm = TRUE),mean(selected_variables$locus),
       mean(selected_variables$ess_kind),mean(selected_variables$selfest),mean(selected_variables$selfeff),
       mean(selected_variables$selfcontrol))

SD <- c(sd(selected_variables$jobsat1), sd(selected_variables$jobsat2,na.rm = TRUE), sd(selected_variables$FWDfwagency),
       sd(selected_variables$jobaut,na.rm = TRUE),sd(selected_variables$jobaut2,na.rm = TRUE),sd(selected_variables$locus),
       sd(selected_variables$ess_kind),sd(selected_variables$selfest),sd(selected_variables$selfeff),
       sd(selected_variables$selfcontrol))

#choose desired rows
correlation_matrix <- correlation_matrix[,c('jobsat1','jobsat2','FWDfwagency','jobaut','jobaut2')]

correlation_matrix<- cbind(M, SD, correlation_matrix)

#rename columns and rows in r
colnames(correlation_matrix)[colnames(correlation_matrix) == "jobsat1"] <- "Job satisfaction (T1)"
colnames(correlation_matrix)[colnames(correlation_matrix) == "jobsat2"] <- "Job satisfaction (T2)"
colnames(correlation_matrix)[colnames(correlation_matrix) == "FWDfwagency"] <- "Belief in free will (T1)"
colnames(correlation_matrix)[colnames(correlation_matrix) == "jobaut"] <- "Job autonomy (T1)"
colnames(correlation_matrix)[colnames(correlation_matrix) == "jobaut2"] <- "Job autonomy (T2)"

rownames(correlation_matrix)[rownames(correlation_matrix) == "jobsat1"] <- "Job satisfaction (T1)"
rownames(correlation_matrix)[rownames(correlation_matrix) == "jobsat2"] <- "Job satisfaction (T2)"
rownames(correlation_matrix)[rownames(correlation_matrix) == "FWDfwagency"] <- "Belief in free will (T1)"
rownames(correlation_matrix)[rownames(correlation_matrix) == "jobaut"] <- "Job autonomy (T1)"
rownames(correlation_matrix)[rownames(correlation_matrix) == "jobaut2"] <- "Job autonomy (T2)"
rownames(correlation_matrix)[rownames(correlation_matrix) == "locus"] <- "Trait locus of control (T1)"
rownames(correlation_matrix)[rownames(correlation_matrix) == "ess_kind"] <- "Implicit beliefs (T1)"
rownames(correlation_matrix)[rownames(correlation_matrix) == "selfest"] <- "Trait self-esteem (T1)"
rownames(correlation_matrix)[rownames(correlation_matrix) == "selfeff"] <- "Job self-efficacy (T1)"
rownames(correlation_matrix)[rownames(correlation_matrix) == "selfcontrol"] <- "Trait self-control (T1)"


#### Table 2 (Appendix) - Correlations between free will subscales and job satisfaction ########################################
#Correlations between free will subscales and job satisfaction
# Select the variables for correlation analysis
selected_variables_2 <-data[, c("jobsat1", "jobsat2", "FWDfw", "FWDagency", "FWDmoral","FWDresp",
                              "FWDpower","FWDlimit","FWDall","FWDfwagency","FWself","FWgeneral")]

# Compute pairwise correlations
correlation_matrix_2 <- cor(selected_variables_2, use = "pairwise.complete.obs")

#choose desired rows and columns
correlation_matrix_2 <- correlation_matrix_2[c('FWDfwagency','FWDall','FWDfw','FWDagency','FWDmoral','FWDpower',
                                               "FWDresp",'FWDlimit','FWself','FWgeneral'),]
correlation_matrix_2 <- correlation_matrix_2[,c('jobsat1','jobsat2')]

#rename columns and rows 
colnames(correlation_matrix_2)[colnames(correlation_matrix_2) == "jobsat1"] <- "Job satisfaction T1"
colnames(correlation_matrix_2)[colnames(correlation_matrix_2) == "jobsat2"] <- "Job satisfaction T2"

rownames(correlation_matrix_2)[rownames(correlation_matrix_2) == "FWDfwagency"] <- "FW T1 Agency and free will subscales"
rownames(correlation_matrix_2)[rownames(correlation_matrix_2) == "FWDall"] <- "FW T1 All subscales combined"
rownames(correlation_matrix_2)[rownames(correlation_matrix_2) == "FWDfw"] <- "FW T1 Free will subscale"
rownames(correlation_matrix_2)[rownames(correlation_matrix_2) == "FWDagency"] <- "FW T1 Personal agency subscale"
rownames(correlation_matrix_2)[rownames(correlation_matrix_2) == "FWDmoral"] <- "FW T1 Moral responsibility subscale"
rownames(correlation_matrix_2)[rownames(correlation_matrix_2) == "FWDpower"] <- "FW T1 Higher power control subscale (R)"
rownames(correlation_matrix_2)[rownames(correlation_matrix_2) == "FWDresp"] <- "FW T1 Personal responsibility subscale"
rownames(correlation_matrix_2)[rownames(correlation_matrix_2) == "FWDlimit"] <- "FW T1 Personal limitations subscale (R)"

rownames(correlation_matrix_2)[rownames(correlation_matrix_2) == "FWself"] <- "FW T1 - Personal"
rownames(correlation_matrix_2)[rownames(correlation_matrix_2) == "FWgeneral"] <- "FW T1 - General"

#### Table 3-Controlling for demographics#####################################################################################
selected_variables_3 <-data[, c("FWgeneral","jobsat1", "jobsat2","age","gender")]

# Compute pairwise correlations
correlation_matrix_3 <- cor(selected_variables_3, use = "pairwise.complete.obs")
#choose desired rows and columns
correlation_matrix_3 <- correlation_matrix_3[,c('FWgeneral','jobsat1','jobsat2')]

#rename columns and rows 
colnames(correlation_matrix_3)[colnames(correlation_matrix_3) == "FWgeneral"] <- "Belief in free will (T1)"
colnames(correlation_matrix_3)[colnames(correlation_matrix_3) == "jobsat1"] <- "Job satisfaction (T1)"
colnames(correlation_matrix_3)[colnames(correlation_matrix_3) == "jobsat2"] <- "Job satisfaction (T2)"

rownames(correlation_matrix_3)[rownames(correlation_matrix_3) == "FWgeneral"] <- "Belief in free will (T1)"
rownames(correlation_matrix_3)[rownames(correlation_matrix_3) == "jobsat1"] <- "Job satisfaction (T1)"
rownames(correlation_matrix_3)[rownames(correlation_matrix_3) == "jobsat2"] <- "Job satisfaction (T2)"
rownames(correlation_matrix_3)[rownames(correlation_matrix_3) == "age"] <- "Age"
rownames(correlation_matrix_3)[rownames(correlation_matrix_3) == "gender"] <- "Gender"
