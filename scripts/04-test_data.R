#### Preamble ####
# Purpose: Basic tests codes for dataset.
# Author: Mingjia Chen, Hadi Ahmad, Tianen (Evan) Hao
# Date: 17 February 2024 
# Contact: mingjia.chen@mail.utoronto.ca 
# License: MIT
# Pre-requisites: 
  # 00-simulate_data.R
  # 01-data_cleaning.R

#### Workspace setup ####
library(tidyverse)

#### Test data ####

data$gender|>
  unique() == c(0, 1)

data$fw_belief |>
  unique() |> length() ==6

data$age|> min() >= 12
data$year |> max() <= 60

data$jobsat|> class() == "numeric"
