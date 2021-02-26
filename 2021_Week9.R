######################### WEEK 9 ##############################################
library(tidytuesdayR)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(GGally)
library(hrbrthemes)
###############################################################################
tuesdata <- tidytuesdayR::tt_load('2021-02-23')
earn <- tuesdata$earn
employed <- tuesdata$employed
###############################################################################

earn <- earn %>% 
  mutate(year = as.factor(year))
earn$year <- factor(earn$year, levels = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"))

earn_white <- earn %>% 
  filter(race == "White")

earn_black <- earn %>% 
  filter(race == "Black or African American")

earn_asian <- earn %>% 
  filter(race == "Asian")

earn_hispanic <- earn %>% 
  filter(ethnic_origin == "Hispanic or Latino")

## una vez que tengo los df, saco los promedios por edades
earn_hispanic <- earn_hispanic %>% 
  select(!n_persons) %>% 
  filter(sex != "Both Sexes" &
           age != "16 years and over" &
           age != "25 years and over") %>% 
  group_by(sex, race, ethnic_origin, age, year) %>% 
  summarise(mean = mean(median_weekly_earn))

earn_asian <- earn_asian %>% 
  select(!n_persons) %>% 
  filter(sex != "Both Sexes"&
           age != "16 years and over" &
           age != "25 years and over") %>% 
  group_by(sex, race, ethnic_origin, age, year) %>% 
  summarise(mean = mean(median_weekly_earn))

earn_black <- earn_black %>% 
  select(!n_persons) %>% 
  filter(sex != "Both Sexes" &
           age != "16 years and over" &
           age != "25 years and over") %>% 
  group_by(sex, race, ethnic_origin, age, year) %>% 
  summarise(mean = mean(median_weekly_earn))

earn_white <- earn_white %>% 
  select(!n_persons) %>% 
  filter(sex != "Both Sexes" &
           age != "16 years and over" &
           age != "25 years and over"
  ) %>% 
  group_by(sex, race, ethnic_origin, age, year) %>% 
  summarise(mean = mean(median_weekly_earn))


############################### HISPANXS #####################################
## pivot_wider con año
earn_hispanic <- earn_hispanic %>% 
  pivot_wider(names_from = year,
              values_from = mean)

earn_hispanic <-earn_hispanic %>% 
  mutate(age = as.factor(age))

## faceted lineplot
sex.labs <- c("Men", "Women")
names(sex.labs) <- c("1", "2")

ggparcoord(earn_hispanic,
           columns = 5:15,
           groupColumn = 4,
           showPoints = TRUE,
           title = "Mean Earnings of Hispanxs and Latinxs by age group",
           alphaLines = 0.4)+
  facet_wrap(~sex,
             labeller = labeller(sex = sex.labs))+
  theme_ipsum()+
  xlab("Year")+
  ylab("")+
  labs(colour = "Age group")


########################## WHITES ###########################################

earn_white <- earn_white %>% 
  pivot_wider(names_from = year,
              values_from = mean)

earn_white <-earn_white %>% 
  mutate(age = as.factor(age))

## faceted lineplot
ggparcoord(earn_white,
           columns = 5:15,
           groupColumn = 4,
           showPoints = TRUE,
           title = "Mean Earnings of White Americans by age group",
           alphaLines = 0.4)+
  facet_wrap(~sex,
             labeller = labeller(sex = sex.labs))+
  theme_ipsum()+
  xlab("Year")+
  ylab("")+
  labs(colour = "Age group")


########################## BLACK OR AFRICAN AMERICAN###########################

earn_black <- earn_black %>% 
  pivot_wider(names_from = year,
              values_from = mean)

earn_black <-earn_black %>% 
  mutate(age = as.factor(age))

## faceted lineplot
ggparcoord(earn_black,
           columns = 5:15,
           groupColumn = 4,
           showPoints = TRUE,
           title = "Mean Earnings of Black or African Americans by age group",
           alphaLines = 0.4)+
  facet_wrap(~sex,
             labeller = labeller(sex = sex.labs))+
  theme_ipsum()+
  xlab("Year")+
  ylab("")+
  labs(colour = "Age group")

########################## ASIAN ###########################################

earn_asian <- earn_asian %>% 
  pivot_wider(names_from = year,
              values_from = mean)

earn_asian <-earn_asian %>% 
  mutate(age = as.factor(age))

## faceted lineplot
ggparcoord(earn_asian,
           columns = 5:15,
           groupColumn = 4,
           showPoints = TRUE,
           title = "Mean Earnings of Asian Americans by age group",
           alphaLines = 0.4)+
  facet_wrap(~sex,
             labeller = labeller(sex = sex.labs))+
  theme_ipsum()+
  xlab("Year")+
  ylab("")+
  labs(colour = "Age group")
