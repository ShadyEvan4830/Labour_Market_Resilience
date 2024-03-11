#### Preamble ####
# Purpose: Simulate the dataset for code testing
# Author: Mingjia Chen, Hadi Ahmad, Tianen (Evan) Hao
# Date: 17 February 2024 
# Contact: mingjia.chen@mail.utoronto.ca 
# License: MIT
# Pre-requisites: none


#### Workspace setup ####
library(tidyverse)


#### Simulate data ####
set.seed(114514)
num_observations = 100

simulated_data <-
  tibble(
    fw_belief =  runif(n = num_observations, min = 1, max = 6) |> round(),
    jobsat = runif(n = num_observations, min = 1, max = 7) |> round(),
    jobaut = runif(n=num_observations, min=1, max = 7) |> round(),
    fwdPower =  runif(n = num_observations, min = 1, max = 6) |> round(),
    fwdSelf = runif(n = num_observations, min = 1, max = 7) |> round(),
    age = runif(n=num_observations, min=12, max = 60) |> round(),
    gender = runif(n=num_observations, min=0, max = 1) |> round()
  )
