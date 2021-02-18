########################## TidyTuesday Week 8 2021 ###########################
library(tidyverse)
library(dplyr)
library(stringr)
library(tidytuesdayR)
library(ggplot2)
tuesdata <- tidytuesdayR::tt_load(2021, week = 8)
freed_slaves <- tuesdata$freed_slaves
census <- tuesdata$census
furniture <- tuesdata$furniture
city_rural <- tuesdata$city_rural
income <- tuesdata$income
occupation <- tuesdata$occupation
conjugal <- tuesdata$conjugal
georgia_pop <- tuesdata$georgia_pop
##############################################################################

### Challenge 05

# longer df
income_longer <- income %>% 
  pivot_longer(cols = 3:7,
               names_to = "Item",
               values_to = "Value")

# set as factor
income_longer <- income_longer %>% 
  mutate_if(is.character, as.factor)

# set factor order
income_longer$Item <- factor(income_longer$Item, levels = c("Other", "Tax", "Clothes", "Food", "Rent"))
income_longer$Class <- factor(income_longer$Class, levels = c("Over $1000",
                                                              "$750-1000", 
                                                              "$500-750",
                                                              "$400-500",
                                                              "$300-400",
                                                              "$200-300",
                                                              "$100-200"
))

# plot
ggplot(income_longer, aes(x = Class, y = Value, fill = Item, label = Value))+
  geom_bar(position="fill", stat="identity", width = 0.75)+
  coord_flip()+
  labs(fill = "Annual Expenditure")+
  ggtitle(label = "Income and Expenditure of 150 Negro Families in Atlanta, GA., USA")+
  xlab("")+
  ylab("")+
  theme(
    plot.title=element_text(family='', 
                            face='bold', 
                            colour='black', 
                            size= 40,
                            hjust= 0.25
    )
  )+
  scale_fill_manual(values = c("#eee7df", "#75B7C0", "#d49384" ,"#90748b", "#24241c"),
                    labels = c("OTHER EXPENSES AND SAVINGS.",
                               "DIRECT TAXES.",
                               "CLOTHES.",
                               "FOOD.",
                               "RENT."))+
  guides(color = guide_legend(override.aes = list(size = 0.25)))+
  theme_minimal()