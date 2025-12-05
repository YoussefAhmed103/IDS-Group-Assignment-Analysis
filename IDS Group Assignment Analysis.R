# Loading R Packages
library(tidyverse)
library(ggplot2)
library(dplyr)

# Assignment brief 
# • Perform exploratory data analysis to assess how 6 continents in the world 
# (all continents except Antarctica) are faring with respect to the following specific targets 
# set out by the UN:

# 2. By 2020, substantially reduce the proportion of youth not in employment, education or 
# training. 

# You may wish to compare across the continents, as well as assess their individual 
# progress across time. You may also wish to further divide up a continent 
# (e.g., into more developed vs. less developed countries). 

# Loading Datasets
continents_data <- read.csv("data sets/continents-according-to-our-world-in-data.csv")

youth_neet <- read.csv("data sets/youth-not-in-education-employment-training.csv")

income_groups <- read.csv("data sets/CLASS_2025_10_07.csv")

# Selecting Specific Data Columns

income_groups_essential <- income_groups %>% select(Economy, Code, Income.group)

continents_data_essential <- continents_data %>% select(Entity, Code, Continent)

# Checking and Replacing Non-Corresponding Primary Keys
setequal(youth_neet$Code, income_groups_essential$Code)

anti_join(youth_neet, income_groups_essential, by = "Code")
anti_join(youth_neet_classification, continents_data_essential, by = "Code")

youth_neet$Code <- recode(youth_neet$Code, 'OWID_KOS' = 'XKX')
continents_data_essential$Code <- recode(continents_data_essential$Code, 'OWID_KOS' = 'XKX')

# Joining Datasets and Removing Missing-Variable Observations 

youth_neet_classification <- youth_neet %>% left_join(income_groups_essential)

youth_neet_essential <- youth_neet_classification %>% select(
  Entity, Code, Year, Share.of.youth.not.in.education..employment.or.training..total....of.youth.population., Income.group
  )

youth_neet_complete <- youth_neet_essential %>% 
  left_join(continents_data_essential) %>% filter(Income.group != '')
