# Data preparation 

library(janitor)
library(tidyverse)
library(readxl)

#### Norway ####
data_NOR <- read_excel("/Users/tuva/Documents/Studier/Master/Masteroppgave/r-coding/data/v6_implementation_env_norway.xlsx")

data_NOR <- data_NOR %>% 
  clean_names() %>% 
  select(year,
         item_no,
         instrument_no,
         actor, 
         administrative_decision_making, 
         delivery_of_im_material_goods_and_services,
         monitoring_reporting,
         inspections,
         supervision,
         planning_participation_evaluation,
         direction) %>% 
  mutate(administrative_decision_making = 
           as.numeric(administrative_decision_making),
         year = as.numeric(year),
         delivery_of_im_material_goods_and_services = 
           as.numeric(delivery_of_im_material_goods_and_services),
         monitoring_reporting = as.numeric(monitoring_reporting),
         inspections = as.numeric(inspections),
         supervision = as.numeric(supervision),
         planning_participation_evaluation = 
           as.numeric(planning_participation_evaluation)) %>% 
  pivot_longer(
    cols = !c(year, item_no, instrument_no, actor, direction), 
    names_to = "task", 
    values_to = "count") %>% 
  mutate(count = if_else(direction == 2 & count == 1, -1, count)) %>% 
  unite("target_instrument", item_no, instrument_no, 
        sep = "_", 
        remove = FALSE) %>% 
  select(-c(item_no, instrument_no, direction)) %>% 
  group_by(year, target_instrument, actor, task) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>% 
  complete(
    nesting(actor, task, target_instrument),  
    year = 1976:2024,
    fill = list(count = 0)) %>% 
  arrange(target_instrument, actor, task, year) %>%  
  group_by(target_instrument, actor, task) %>% 
  mutate(cum_count = cumsum(count)) %>% 
  ungroup() %>% 
  filter(cum_count != 0)

#### Denmark ####

data_DK <- read_excel("/Users/tuva/Documents/Studier/Master/Masteroppgave/r-coding/data/v3_implementation_env_denmark.xlsx")

data_DK <- data_DK %>% 
  clean_names() %>% 
  select(year,
         item_no,
         instrument_no,
         actor, 
         administrative_decision_making, 
         delivery_of_im_material_goods_and_services,
         monitoring_reporting,
         inspections,
         supervision,
         planning_participation_evaluation,
         direction) %>% 
  mutate(administrative_decision_making = 
           as.numeric(administrative_decision_making),
         year = as.numeric(year),
         delivery_of_im_material_goods_and_services = 
           as.numeric(delivery_of_im_material_goods_and_services),
         monitoring_reporting = as.numeric(monitoring_reporting),
         inspections = as.numeric(inspections),
         supervision = as.numeric(supervision),
         planning_participation_evaluation = 
           as.numeric(planning_participation_evaluation)) %>% 
  pivot_longer(
    cols = !c(year, item_no, instrument_no, actor, direction), 
    names_to = "task", 
    values_to = "count") %>% 
  mutate(count = if_else(direction == 2 & count == 1, -1, count)) %>% 
  unite("target_instrument", item_no, instrument_no, 
        sep = "_", 
        remove = FALSE) %>% 
  select(-c(item_no, instrument_no, direction)) %>% 
  group_by(target_instrument, actor, task, year) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>% 
  complete(
    nesting(actor, task, target_instrument),  
    year = 1976:2024,
    fill = list(count = 0)) %>% 
  arrange(target_instrument, actor, task, year) %>%  
  group_by(target_instrument, actor, task) %>% 
  mutate(cum_count = cumsum(count)) %>% 
  ungroup() %>% 
  filter(cum_count != 0)

#### France ####

data_FR <- read_excel("/Users/tuva/Documents/Studier/Master/Masteroppgave/r-coding/data/v3_implementation_env_France.xlsx")

data_FR <- data_FR %>% 
  clean_names() %>% 
  select(year,
         item_no,
         instrument_no,
         actor, 
         administrative_decision_making, 
         delivery_of_im_material_goods_and_services,
         monitoring_reporting,
         inspections,
         supervision,
         planning_participation_evaluation,
         direction) %>% 
  mutate(administrative_decision_making = 
           as.numeric(administrative_decision_making),
         year = as.numeric(year),
         delivery_of_im_material_goods_and_services = 
           as.numeric(delivery_of_im_material_goods_and_services),
         monitoring_reporting = as.numeric(monitoring_reporting),
         inspections = as.numeric(inspections),
         supervision = as.numeric(supervision),
         planning_participation_evaluation = 
           as.numeric(planning_participation_evaluation)) %>% 
  pivot_longer(
    cols = !c(year, item_no, instrument_no, actor, direction), 
    names_to = "task", 
    values_to = "count") %>% 
  mutate(count = if_else(direction == 2 & count == 1, -1, count)) %>% 
  unite("target_instrument", item_no, instrument_no, 
        sep = "_", 
        remove = FALSE) %>% 
  select(-c(item_no, instrument_no, direction)) %>% 
  group_by(target_instrument, actor, task, year) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>% 
  complete(
    nesting(actor, task, target_instrument),  
    year = 1976:2024,
    fill = list(count = 0)) %>% 
  arrange(target_instrument, actor, task, year) %>%  
  group_by(target_instrument, actor, task) %>% 
  mutate(cum_count = cumsum(count)) %>% 
  ungroup() %>% 
  filter(cum_count != 0)

#### Germany ####

data_GER <- read_excel("/Users/tuva/Documents/Studier/Master/Masteroppgave/r-coding/data/v3_implementation_env_Germany.xlsx")

data_GER2 <- data_GER %>% 
  clean_names() %>% 
  select(year,
         item_no,
         instrument_no,
         actor, 
         administrative_decision_making, 
         delivery_of_im_material_goods_and_services,
         monitoring_reporting,
         inspections,
         supervision,
         planning_participation_evaluation,
         direction) %>% 
  mutate(administrative_decision_making = 
           as.numeric(administrative_decision_making),
         year = as.numeric(year),
         delivery_of_im_material_goods_and_services = 
           as.numeric(delivery_of_im_material_goods_and_services),
         monitoring_reporting = as.numeric(monitoring_reporting),
         inspections = as.numeric(inspections),
         supervision = as.numeric(supervision),
         planning_participation_evaluation = 
           as.numeric(planning_participation_evaluation)) %>% 
  pivot_longer(
    cols = !c(year, item_no, instrument_no, actor, direction), 
    names_to = "task", 
    values_to = "count") %>% 
  mutate(count = if_else(direction == 2 & count == 1, -1, count)) %>% 
  unite("target_instrument", item_no, instrument_no, 
        sep = "_", 
        remove = FALSE) %>% 
  select(-c(item_no, instrument_no, direction)) %>% 
  group_by(target_instrument, actor, task, year) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>% 
  complete(
    nesting(actor, task, target_instrument),  
    year = 1976:2024,
    fill = list(count = 0)) %>% 
  arrange(target_instrument, actor, task, year) %>%  
  group_by(target_instrument, actor, task) %>% 
  mutate(cum_count = cumsum(count)) %>% 
  ungroup() %>% 
  filter(cum_count != 0)

#### Joining the datasets ####
data_GER2$country <- "Germany"
data_NOR$country <- "Norway"
data_DK$country <- "Denmark"
data_FR$country <- "France"

data_combined <- rbind(data_DK, data_NOR, data_FR, data_GER2)

setwd("/Users/tuva/Documents/Studier/Master/Masteroppgave/r-coding/data/")
save(data_combined, file = "data_combined.Rda")
