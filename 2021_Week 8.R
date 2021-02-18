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

### Challenge 04

# df longer
freed_slaves_long <- freed_slaves %>% 
  pivot_longer(cols = 2:3,
               names_to="Status",
               values_to="Percentage")

# percentage calc
freed_slaves_long <- freed_slaves_long %>% 
  group_by(Year, Status) %>% 
  summarise(n = sum(Percentage)) %>% 
  mutate(Percentage = n / sum(n))

# plot
ggplot(freed_slaves_long, aes(x=Year, y = Percentage, fill = Status))+
  geom_area()+
  labs(
    title = "PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES . \n PROPORTION DES NÈGRES LIBRES ET DES ESCLAVES EN AMÉRIQUE .",
    subtitle = "DONE BY ATLANTA UNIVERSITY .",
    caption = "@_msquiroga for #TidyTuesday, Recreation of W.E.B. DuBois data viz"
  )+
  scale_fill_manual(values= c("chartreuse4", "black"))+
  scale_x_continuous(breaks = round(seq(min(freed_slaves_long$Year), max(freed_slaves_long$Year), by = 10),1),
                     position = "top") +
  theme_wsj()+
  theme(
    plot.title = element_text(size=18,
                              hjust=0.5),
    plot.subtitle = element_text(size = 14,
                                 hjust=0.5),
    plot.caption = element_text(size=10,
                                hjust =0.75),
    legend.position = "none",
    axis.title.y = element_blank()      )+
  annotate("text", x = 1825, y = 0.75, label = "SLAVES \n ESCLAVES", color = "white", fontface = "bold", size = 7)+
  annotate("text", x = 1825, y = 0.93, label = "FREE - LIBRE", color = "black", fontface = "bold", size = 7)



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