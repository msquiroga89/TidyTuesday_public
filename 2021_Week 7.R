######################## TidyTuesday Week 7 2021 ##############################
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load(2021, week = 7)
lifetime_earn <- tuesdata$lifetime_earn
homeowner <- tuesdata$home_owner
income_aggregate <- tuesdata$income_aggregate
income_distribution <- tuesdata$income_distribution
income_limits <- tuesdata$income_limits
income_mean <- tuesdata$income_mean
income_time <- tuesdata$income_time
lifetime_wealth <- tuesdata$lifetime_wealth
student_debt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv')
retirement <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/retirement.csv')

###############################################################################
library(dplyr)
library(tidyverse)
library(ggplot2)
library(unikn)
library(scales)
library(hrbrthemes)

### Wealth Median Evolution | Scatterplot
lifetime_wealth %>% 
  filter(type == "Median") %>% 
  ggplot(aes(x = year, y = wealth_lifetime, group = race, color = race)) +
  geom_line(size = 1)+
  geom_point(size = 2)+
  expand_limits( x = c(1983,NA), y = c(0,NA)) +
   labs(
    title = "Wealth Median Evolution",
    caption = "Source: Urban Institute & US Census",
    y = "Dollars",
    x = NULL)+
  theme(plot.title = element_text(size=14, face = "bold", hjust= -2),
        plot.caption = element_text(hjust=5)) +
  scale_color_discrete(name="Race")+
  scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3)) +
  theme_classic()


### Highest and Lowest Income by Race in 2019

# filter by year, race, lowest & highest quintile and 2019 dollars.
income_summary <- income_mean %>% 
  filter(year == "2019", race %in% c("Asian Alone", "Black Alone", "Hispanic",
                                     "White Alone"), 
         income_quintile %in% c("Lowest", "Highest"), 
         dollar_type == "2019 Dollars") %>% 
  select(year, race, income_quintile, income_dollars)

# delete duplicated rows
income_summary <- income_summary[-c(3,4),]

# from longer to wider df
income_sum_wider <- income_summary %>% 
  pivot_wider(
    names_from = "income_quintile", values_from = "income_dollars"
  )

# plot
ggplot(income_sum_wider) +
  geom_segment(aes(x=race, xend=race, y=Lowest, yend=Highest), color="grey", 
               size = 2) +
  geom_point(aes(x=race, y=Lowest), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point(aes(x=race, y=Highest), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3)) +
  coord_flip()+
  theme_ipsum() +
  labs(
    title = "Highest and Lowest Income by Race in 2019",
    caption = "Source: Urban Institute & US Census")+
  theme(legend.position = "none",
        plot.title = element_text(size=12, hjust = -1.5)) +
  xlab("")+
  ylab("")


### Income Distribution by Race in 2019

# filter year, race
income_mean_summ <- income_distribution %>% 
  filter(year == "2019" & race %in% c("White Alone", "Hispanic (Any Race)", "Black Alone", "Asian Alone")) %>% 
  select(race, income_mean, income_bracket, income_distribution)

# Set factor level
income_mean_summ$income_bracket <- factor(income_mean_summ$income_bracket,
                                          levels = c("Under $15,000", 
                                                     "$15,000 to $24,999",
                                                     "$25,000 to $34,999",
                                                     "$35,000 to $49,999",
                                                     "$50,000 to $74,999",
                                                     "$75,000 to $99,999",
                                                     "$100,000 to $149,999",
                                                     "$150,000 to $199,999",
                                                     "$200,000 and over"))
income_mean_summ$race <- factor(income_mean_summ$race,
                                levels = c("Asian Alone", "White Alone",
                                           "Hispanic (Any Race)", "Black Alone"))

# plot
ggplot(income_mean_summ[order(income_mean_summ$income_bracket,decreasing=T),], 
       aes(fill=income_bracket, y=income_distribution, x=race))+
  geom_bar(position="stack", stat="identity")+
  labs(
    title = "Income distribution by Race in 2019",
    caption = "Source: Urban Institute & US Census")+
  xlab("")+
  ylab("")+ 
  theme(plot.title = element_text(size=14, face = "bold", hjust= 2),
        axis.text.x = element_blank(),
        plot.caption = element_text(hjust=4)) +
   scale_fill_manual(values = usecol(pal = pal_unikn_pair), name = "Income Bracket")+
  scale_y_reverse()+
  coord_flip()
