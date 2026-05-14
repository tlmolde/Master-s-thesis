# number of actors 
library(tidyverse)
setwd("/Users/tuva/Documents/Studier/Master/Masteroppgave/r-coding/data/")
load("data_combined.Rda")

data_no_actors <- data_combined %>% 
  group_by(country, year) %>% 
  distinct(actor) %>% 
  summarise(no_actors = n())


# Recoding the variable into growth

data_no_actors <- data_no_actors %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    no_actors_lag = dplyr::lag(no_actors, 1),
    no_actors_diff = no_actors - no_actors_lag) 

# Indicator of average numbers of actors per policy:

data_no_actors_test <- data_combined %>% 
  group_by(target_instrument, country, year) %>% 
  distinct(actor) %>% 
  summarise(no_actors_pol = n()) %>% 
  ungroup()

data_no_actors_test <- data_no_actors_test %>% 
  group_by(country, year) %>% 
  summarise(mean_actors = mean(no_actors_pol)) %>% 
  ungroup()

data_no_actors <- data_no_actors %>% 
  left_join(data_no_actors_test, by = c("country", "year"))

# Saving the dataset 
setwd("/Users/tuva/Documents/Studier/Master/Masteroppgave/r-coding/data/")
save(data_no_actors, file = "data_no_actors.Rda")




