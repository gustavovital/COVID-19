# Analise dos resultados das tabelas em relação ao covid-19
# 
# Autor: @gustavoovital
# Data: 18/03/2020

# pacotes ----

library(tidyverse)
library(ggthemes)
library(wesanderson)

# base de dados ----

data <- readRDS('data.rds')
data_wider <- readRDS('data_wider.rds')

# analise grafica ----

# Sintomas ----




# China ----

data_wider %>% 
  filter(`Country/Region` == 'Malaysia') %>% 
  ggplot(aes(Date)) +
  geom_line(aes(y = Confirmed)) +
  geom_line(aes(y = Deaths)) +
  geom_line(aes(y = Recovered)) 

data %>% 
  filter(Case == 'Confirmed') %>% 
  group_by(`Country/Region`) %>%
  filter(Date == as.Date('2020-03-17')) %>% 
  # summarise(Confirmados = sum(Confirmed),
  #           Recuperados = sum(Recovered),
  #           Mortes = sum(Deaths)) %>% 
  arrange(desc(Total)) %>% 
  head(30) %>% 
  ggplot(aes(x = reorder(`Country/Region`, -Total), y = Total)) +
  geom_col() +
  coord_flip()

data_wider %>% 
  group_by(Date) %>%
  summarise(Confirmados = sum(Confirmed),
            Recuperados = sum(Recovered),
            Mortes = sum(Deaths)) %>% 
  ggplot(aes(x = Date)) +
  geom_area(aes(y = Confirmados, fill = 'Confirmados'), alpha = .2) +
  geom_area(aes(y = Recuperados, fill = 'Recuperados'), alpha = .2) +
  geom_area(aes(y = Mortes, fill = 'Mortes'), alpha = .6) +
  
  geom_line(aes(y = Confirmados, colour = 'Confirmados'), size = 1) +
  geom_line(aes(y = Recuperados, colour = 'Recuperados'), size = 1) +
  geom_line(aes(y = Mortes, colour = 'Mortes'), size = 1) +
  
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 3), name = NULL) +
  scale_colour_manual(values = wes_palette("GrandBudapest1", n = 3), name = NULL) +
  
  labs(title = 'Evolução do COVID-19 no Mundo', x = NULL, subtitle = 'De Fevereiro à Março, de acordo com o caso', y = NULL) +

  theme_hc() +
  
  theme(plot.title = element_text(size = 30, family = 'Bookman', colour = 'gray26'),
        plot.subtitle = element_text(size = 23, family = 'Bookman', colour = 'gray46'),
        legend.position = 'bottom') +
  
  gganimate::transition_reveal(Date) -> gif_mundo

gganimate::animate(gif_mundo, fps = 10, width = 700)

