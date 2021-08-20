################################## Week 34 ######################################
# El dataset de esta semana está conformado por los comandos de voz de StarTrek.#
#################################################################################


# Setup -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 34)
computer <- tuesdata$computer
library(tidyverse)
library(ggthemes)


# Graph -------------------------------------------------------------------

#' El objetivo es hacer un cuadro que muestre los tipos de enunciados que se emiten,
#' divididos según quién los emite, si una computadora o una persona. Elegí el 
#' gráfico lollipop porque es una alternativa al gráfico de barras que me permite
#' modificar más elementos visuales. La lógica de fondo es la misma (y, para 
#' algunas personas, es un gráfico más confuso y menos exacto que las barras).

computer <- computer %>% 
  mutate_all(.funs = tolower) %>%  # para unificar filas
  mutate_if(is.character, as.factor)

computer$char_type <- factor(computer$char_type, levels = c("computer", "person"), 
                             labels = c("Computadora", "Persona")) # ordena

computer %>% 
  group_by(char_type, type) %>% 
  summarise(count = n()) %>%  # cantidad de comandos
  mutate(type = reorder_within(type, count, char_type)) %>% # ordenados por cantidad
  ggplot(aes(x=reorder(type, count), y = count))+
  geom_point(color = "red")+
  geom_segment(aes(x=reorder(type, count), xend =type, y=0, yend=count, color = "red"))+
  facet_wrap(~char_type, 
             scales = "free", # para que no haya espacios vacíos entre las facetas
             labeller = labeller( # título del facet
               Computadora = computer,
               Persona = person))+
  coord_flip()+
  scale_y_reordered()+ # reordena los elementos dentro del facet
  scale_x_discrete(labels = c("response___Computadora" = "Respuesta",
                              "alert___Computadora" = "Alerta",
                              "info___Computadora" = "Información",
                              "countdown___Computadora" = "Cuenta Regresiva",
                              "clarification___Computadora" = "Clarificación",
                              "progress___Computadora" = "Progreso",
                              "conversation___Computadora" = "Conversación",
                              "command___Persona" = "Orden",
                              "wake word___Persona" = "Palabra Raíz",
                              "statement___Persona" = "Declaración",
                              "question___Persona" = "Pregunta",
                              "conversation___Persona" = "Conversación",
                              "password___Persona" = "Contraseña",
                              "comment___Persona" = "Comentario"))+
  theme_few()+
  theme(plot.background = element_rect(fill = "black"), # fondo de la imagen
        panel.background = element_rect(fill = "black"), # fondo del gráfico
        axis.text = element_text(color = "white", size = 12), # texto de los ejes
        plot.title = element_text(color = "white", size = 20), # título del gráfico
        plot.caption = element_text(color = "white"), # fuente
        legend.position = "none", # quito el cuadro de referencias
        text = element_text(family = "Kefa"), # tipografía de todo el gráfico
        panel.grid.major.x = element_line(size = 4), # achica el espacio entre ticks
        strip.text = element_text(size = 12) # tamaño de títlo de facet
  )+
  labs(x = "", y = "",
       title = "¿Qué tipo de enunciados genera cada uno?",
       caption = "Fuente: Speech Interactions | http://www.speechinteraction.org/TNG/ | @_msquiroga"
    )

