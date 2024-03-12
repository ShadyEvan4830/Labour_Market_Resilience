#### Preamble ####
# Purpose:  Simulate the dataset for code testing
# Author: Mingjia Chen, Catherine Punnoose, Tianen (Evan) Hao
# Date: 11 March 2024 
# Contact: mingjia.chen@mail.utoronto.ca 
# License: MIT
# Pre-requisites: none

#### Workspace setup ####
library(tibble)
library(dplyr)


#### Simulate data ####
# Define years and work hours categories
years <- c(1972, 1973, 1974, 1975, 1976, 1977, 1978, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2021, 2022)
work_hours_categories <- c("No Response", "0-20", "21-40", "41-60", "61-80")

# Create the initial tibble with work_hours column
simulated_data <- tibble(work_hours = work_hours_categories)

# Add each year as a new column with simulated data
set.seed(123) # For reproducibility
for(year in years) {
  simulated_data[[as.character(year)]] <- runif(n = length(work_hours_categories), min = 50, max = 2000)
}

# Correct test for a specific year's range of values using appropriate column referencing
test_year_range <- all(simulated_data[["2022"]] >= 50 & simulated_data[["2022"]] <= 2000)
print(paste("Test Year Range for 2022: ", test_year_range))

# Round the numbers into without decimals
simulated_data[,c(2:43)] <- round(simulated_data[,c(2:43)])


