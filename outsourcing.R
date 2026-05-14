
library(tidyverse)
setwd("/Users/tuva/Documents/Studier/Master/Masteroppgave/r-coding/data/")
load("data_combined.Rda")

# Reducing the dataset to only containing private sector actors:
data_outsourcing <- data_combined %>% 
  filter(actor == "Private Sector Actors") %>% 
  group_by(country, year) %>% 
  summarise(outsourcing_index = n(),
            .groups = "drop")%>% 
  complete(country, 
           year, 
           fill = list(outsourcing_index = 0))

# Testing to see if everything is correct: 
# table(outsourcing_indicator$country)
# 
# test <- outsourcing_indicator %>% 
#   filter(country == "France")

# Making this into a ratio:

# Finding the total number of tasks per policy: 
data_total_tasks <- data_combined %>% 
  group_by(country, year) %>% 
  summarise(total_tasks = n(),
            .groups = "drop")

# Checking that it is correct:
# table(data_no_tasks$country,
#       data_no_tasks$year)

# test <- data_no_tasks %>%
#   filter(country == "France")

# joining the total tasks and the tasks by private actors:
data_tasks_outsourcing <- data_outsourcing %>% 
  left_join(data_total_tasks, by = c("country", "year")) %>% 
  mutate(outsourcing_index = as.numeric(outsourcing_index),
         total_tasks = as.numeric(total_tasks))

# As percentage 0-100:
data_tasks_outsourcing <- data_tasks_outsourcing %>%
  mutate(outsourcing_pp = (100* (outsourcing_index/total_tasks)))

# As ratio 0-1:
data_tasks_outsourcing <- data_tasks_outsourcing %>% 
  mutate(outsourcing_ratio = (outsourcing_index/total_tasks))


data_tasks_outsourcing <- data_tasks_outsourcing %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    outsourcing_lag = dplyr::lag(outsourcing_index, 1),
    outsourcing_nom_diff = outsourcing_index - outsourcing_lag) 

# table(outsourcing_indicator$country)

setwd("/Users/tuva/Documents/Studier/Master/Masteroppgave/r-coding/data/")
save(data_tasks_outsourcing, file = "data_tasks_outsourcing.Rda")

























