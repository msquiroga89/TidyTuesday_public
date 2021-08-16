################################ Week 33 ######################################
#' Los datos de esta semana pertenecen al Bureau of Economics Analysis.


# Setup -------------------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(hrbrthemes)
library(streamgraph)
library(htmlwidgets)

tuesdata <- tidytuesdayR::tt_load('2021-08-10')
investment <- tuesdata$investment
chain_investment <- tuesdata$chain_investment
ipd <- tuesdata$ipd


# Exploratory Analysis ----------------------------------------------------

glimpse(investment)
glimpse(chain_investment) # cuánto cambió a lo largo del tiempo, actualizado
glimpse(ipd)

#' Evidentemente acá hay conceptos económicos que no manejo, incluso mirando la
#' documentación. Me voy a quedar con el primer dataset, para mirar solamente
#' las inversiones a grandes rasgos.

investment %>% group_by(category) %>% count()
investment %>% group_by(meta_cat) %>% count()


# Gráficos ----------------------------------------------------------------

#' El primer gráfico que me interesa ver es cómo evolucionó la inversión federal
#' en educación a lo largo de los años. Para eso, voy a realizar un gráfico de líneas
#' de todas las categorías, resaltando solamente educación.

investment %>% 
  filter(category=="Federal") %>% 
  mutate(highligth = ifelse(meta_cat=="Education", "Education", "Other")) %>% 
  group_by(meta_cat, year) %>% 
  ggplot(aes(x = year, y = gross_inv, group = meta_cat, size = highligth))+
  geom_line()+
  scale_color_manual(values = c("#69b3a2", "lightgrey"))+
  scale_size_manual(values=c(1.5,0.2))+
  theme_ipsum()+
  theme(legend.position = "none")+
  labs(x="", y="",
       title="Evolución de la inversión federal en educación",
       caption = "Data: Bureau of Economic Analysis | Plot: @_msquiroga | #TidyTuesday")
  

#' Otra cuestión interesante puede ser mirar cómo evolucionaron todos los tipos de
#' inversiones en educación, según procedencia.

graph <- investment %>% 
  filter(meta_cat == "Education") %>% 
  streamgraph(., key = "category", value = "gross_inv", date="year") %>% 
  sg_legend(TRUE, "Fuente de inversión")
saveWidget(graph, file=paste0(getwd(), "stream.html"))
