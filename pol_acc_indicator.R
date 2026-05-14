# creating the policy accumulation indicator 

library(tseries)
library(PolicyPortfolios)
library(tidyverse)
library(conflicted)
conflicts_prefer(dplyr::filter())

setwd("/Users/tuva/Documents/Studier/Master/Masteroppgave/r-coding/data/")
load("data_combined.Rda")

# subsetting and reshaping the data to make policy portfolios:
data_combined_reduced <- data_combined %>% 
  group_by(country, year) %>% 
  distinct(target_instrument) %>% 
  separate_wider_delim(target_instrument, 
                       "_", 
                       names = c("target", 
                                 "instrument"))

# Adding a new variable covered with all values 1 
data_combined_reduced$covered <- 1
data_combined_reduced$Sector <- "environment"

# filling out the spaces in the portfolio with covered = 0:
data_complete <- data_combined_reduced %>%
  mutate(instrument = as.numeric(instrument)) %>% 
  mutate(target = as.numeric(target))

# expand the dataset to include all targets and instruments combinations:
data_expanded <- data_complete %>%
  ungroup() %>%
  expand(
    nesting(country, year),
    target = 1:48,
    instrument = 101:113
  ) %>%
  left_join(data_complete, by = c("country", "year", "target", "instrument")) %>%
  mutate(covered = coalesce(covered, 0))

table(data_expanded$country)

#### Visualising portfolios #####
# checking visuals of denmarks portfolio:
data_norway <- data_complete %>%
  filter(country == "Norway") %>%
  filter(year %in% c(2000, 2024)) %>% 
  filter(target != 77)

NOR_2000_2024 <- ggplot(data_norway, aes(x = target,
                        y = instrument,
                        fill = factor(covered))) +
  geom_tile() +
  scale_fill_manual(
    name   = "covered",
    values = c("0" = "white",
               "1" = "black"))+
  labs(x = "Targets",
       y = "Instruments",
       caption = "The figure shows the policy portfolio of Norway in 2000 and 2024.") +
  theme_classic()+
  theme(
    panel.border = element_rect(color = "black",
                                linewidth = 1),
    legend.position = "none",
    plot.caption = element_text(hjust = 0),
    axis.text.x = element_text(angle = 90),
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "grey90"))+
  scale_x_continuous(breaks = seq(0,48,1))+
  scale_y_continuous(breaks = seq(100,113,1))+
  facet_wrap(~year,
             ncol = 1)

ggsave("portfolio_NOR_2000_2024.pdf",
       plot   = NOR_2000_2024,
       width  = 7,
       height = 5)

#### calculating the indicator #####

data_coverage_pp <- data_expanded %>% 
  group_by(country, year) %>% 
  summarise(coverage_pp = 100*mean(covered)) %>% 
  ungroup()

table(data_coverage_pp$country)

# Differentiating: 
data_coverage_pp <- data_coverage_pp %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    coverage_lag = dplyr::lag(coverage_pp, 1),
    coverage_diff = coverage_pp - coverage_lag
  ) 

#### Saving the new dataset #####

setwd("/Users/tuva/Documents/Studier/Master/Masteroppgave/r-coding/data/")
save(data_coverage_pp, file = "data_coverage_pp.Rda")
