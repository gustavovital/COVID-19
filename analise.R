# Analise dos resultados das tabelas em relação ao covid-19
# 
# Autor: @gustavoovital
# Data: 18/03/2020

# pacotes ----

library(tidyverse)
library(ggthemes)
library(wesanderson)
library(ggforce)
library(gganimate)

# base de dados ----

source('dados.R')

data <- readRDS('data.rds')
data_wider <- readRDS('data_wider.rds')

# analise grafica ----

# China ativos x casos ----

data_wider %>% 
  filter(`Country/Region` == 'China') %>% 
  ggplot(aes(x = Date)) +
  geom_area(aes(y = Ativos, fill = 'Casos Ativos'), alpha = .8) +
  geom_area(aes(y = Recovered, fill = 'Recuperados'), alpha = .5) +
  geom_area(aes(y = Confirmed, fill = 'Casos Totais'), alpha = .3) +
  geom_area(aes(y = Deaths, fill = 'Mortos'), alpha = .3) +
  
  geom_line(aes(y = Ativos, colour = 'Casos Ativos')) +
  geom_line(aes(y = Recovered, colour = 'Recuperados')) +
  geom_line(aes(y = Confirmed, colour = 'Casos Totais')) +
  geom_line(aes(y = Deaths, colour = 'Mortos')) +
  
  scale_x_date(date_labels = "%d-%b", date_breaks = "1 week") +
  
  geom_vline(xintercept = subset(data_wider, Ativos == 58108)$Date, size = .1, colour = 'gray5', alpha = 1) +
  
  scale_fill_manual(values = wes_palette('Rushmore1'), name = NULL) +
  scale_colour_manual(values = wes_palette('Rushmore1'), name = NULL) +
  
  annotate("text", x = subset(data_wider, Ativos == 58108)$Date, y = 65000, 
           label = "Pico da Pandemia na China", hjust = -.06, family = 'Bookman', colour = 'gray25') +
  
  
  labs(title = 'Evolução do COVID-19 na China', subtitle = 'Casos Ativos, Totais, e Recuperados', x = NULL, y = NULL,
       caption = 'Fonte: CSSEGISandData\nElaboração: @gustavoovital') +
  
  theme_hc() +
  theme(legend.position = 'bottom',
        text = element_text(family = 'Bookman', colour = 'gray45'),
        plot.title = element_text(size = 23, colour = 'gray25'),
        plot.subtitle = element_text(size = 17),
        plot.caption = element_text(size = 15, colour = 'gray45')) +
  transition_reveal(Date) -> gif
  

gif_china <- animate(gif, width = 700)
magick::image_write(gif_china, path="china.gif")
    



# China ----

data_wider %>% 
  filter(`Country/Region` == 'China') %>% 
  ggplot(aes(Recovered, colour = `Country/Region`, size = Confirmed/Deaths)) +
  geom_path(aes(y = Confirmed), alpha = .3) +
  geom_point(aes(y = Confirmed), alpha = .3) +
  theme_bw() +
  theme(legend.position = 'bottom')

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
  
  labs(title = 'Evolução do COVID-19 no Mundo', x = NULL, subtitle = 'De Fevereiro à Março, de acordo com o caso',
       y = NULL, caption = 'Fonte: CSSEGISandData\nElaboração: @gustavoovital') +

  theme_hc() +
  
  theme(plot.title = element_text(size = 30, family = 'Bookman', colour = 'gray26'),
        plot.subtitle = element_text(size = 23, family = 'Bookman', colour = 'gray46'),
        legend.position = 'bottom') +
  
  gganimate::transition_reveal(Date)

# Brasil, EUA, Italia ----

data_wider %>% 
  filter(`Country/Region` == 'Italy' | `Country/Region` == 'US' | `Country/Region` == 'Brazil') -> compared

compared %>% 
  filter(`Country/Region` == 'Brazil', Confirmed > 99) -> confirmed_brazil

confirmed_brazil$days <- 1:(nrow(confirmed_brazil))

compared %>% 
  filter(`Country/Region` == 'US', Confirmed > 99) -> confirmed_US

confirmed_US$days <- 1:(nrow(confirmed_US))

compared %>% 
  filter(`Country/Region` == 'Italy', Confirmed > 99) -> confirmed_italy

confirmed_italy$days <- 1:(nrow(confirmed_italy))

Confirmed <- tibble(Days = c(confirmed_brazil$days, confirmed_US$days, confirmed_italy$days),
                    Country = c(confirmed_brazil$`Country/Region`, confirmed_US$`Country/Region`, confirmed_italy$`Country/Region`),
                    Confirmed = c(confirmed_brazil$Confirmed, confirmed_US$Confirmed, confirmed_italy$Confirmed))



ggplot(NULL) +
  geom_bar(aes(y = Confirmed, fill = 'EUA', x = days), data = confirmed_US, stat = "identity", alpha = .8) +
  geom_bar(aes(y = Confirmed, fill = 'Italia', x = days), data = confirmed_italy, stat = "identity", alpha = .5) +
  geom_bar(aes(y = Confirmed, fill = 'Brasil', x = days), data = confirmed_brazil, stat = "identity", alpha = .8) +
  scale_fill_manual(values = wes_palette('GrandBudapest1'), name = NULL) +
  labs(title = expression(bold('Novos Casos de CoronaVirus')~'(> 100 Casos)'), subtitle = 'Comparação entre Itália x Brasil x EUA', x = 'Dias a partir do 100 caso confirmado', y = NULL,
       caption = 'Fonte: CSSEGISandData\nElaboração: @gustavoovital') +
  # geom_mark_ellipse(aes(filter = Confirmed == 19100, label = 'EUA:', y = 19100, x = days, description = '19100 casos confirmados no 18 dia'), data = confirmed_US) +
  # geom_mark_ellipse(aes(filter = Confirmed == 12462 & days == 18, label = 'Itália:', y = 12462, x = days, description = '12462 casos confirmados no 18 dia'), data = confirmed_italy) +
  # geom_mark_ellipse(aes(filter = Confirmed == 793 & days == 8, label = 'Dia 8:', y = 793, x = days, description = '793 casos no Brasil, 1694 casos na Itália, 959 nos EUA'), data = confirmed_brazil) +
  
  theme_hc() +
  theme(plot.title.position = 'plot',
        text = element_text(family = 'Bookman'),
        plot.title = element_text(size = 25, colour = 'gray25'),
        plot.subtitle = element_text(size = 25, colour = 'gray45'),
        plot.caption = element_text(size = 15, colour = 'gray45'))
  
