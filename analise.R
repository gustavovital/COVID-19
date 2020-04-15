# Analise dos resultados das tabelas em relação ao covid-19
# 
# Autor: @gustavoovital
# Data: 18/03/2020

# pacotes ----

library(tidyverse)
library(ggthemes)
library(wesanderson)
library(gganimate)
library(extrafont)

options(scipen = 999)

# base de dados ----

# source('dados.R')

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
           label = "Pico da Pandemia na China", hjust = -.06, family = 'Montserrat', colour = 'gray25') +
  
  
  labs(title = 'Evolução do COVID-19 na China', subtitle = 'Casos Ativos, Totais, e Recuperados', x = NULL, y = NULL,
       caption = 'Fonte: CSSEGISandData\nElaboração: @gustavoovital') +
  
  theme_hc() +
  theme(legend.position = 'bottom',
        text = element_text(family = 'Montserrat', colour = 'gray45'),
        plot.title = element_text(size = 23, colour = 'gray25'),
        plot.subtitle = element_text(size = 17),
        plot.caption = element_text(size = 15, colour = 'gray45'),
        plot.title.position = 'plot') 
  

# korea do sul ativos x casos ----

data_wider %>% 
  filter(`Country/Region` == 'Korea, South', Date > as.Date('2020-02-17')) %>% 
  ggplot(aes(x = Date)) +
  geom_area(aes(y = Ativos, fill = 'Casos Ativos'), alpha = .8) +
  geom_area(aes(y = Recovered, fill = 'Recuperados'), alpha = .5) +
  geom_area(aes(y = Confirmed, fill = 'Casos Totais'), alpha = .3) +
  geom_area(aes(y = Deaths, fill = 'Mortos'), alpha = 1) +
  
  geom_line(aes(y = Ativos, colour = 'Casos Ativos')) +
  geom_line(aes(y = Recovered, colour = 'Recuperados')) +
  geom_line(aes(y = Confirmed, colour = 'Casos Totais')) +
  geom_line(aes(y = Deaths, colour = 'Mortos')) +
  
  scale_x_date(date_labels = "%d-%b", date_breaks = "1 week") +
  
  geom_vline(xintercept = subset(data_wider, Ativos == 7577)$Date, size = .1, colour = 'gray5', alpha = 1) +
  
  scale_fill_manual(values = wes_palette('Royal2'), name = NULL) +
  scale_colour_manual(values = wes_palette('Royal2'), name = NULL) +
  
  annotate("text", x = subset(data_wider, Ativos == 7577)$Date, y = 8900, 
           label = "Pico da Pandemia na\n Coreia do Sul", hjust = -.06, family = 'Bookman', colour = 'gray25') +
  
  
  labs(title = 'Evolução do COVID-19 na Coreia do Sul', subtitle = 'Casos Ativos, Totais, e Recuperados', x = NULL, y = NULL,
       caption = 'Fonte: CSSEGISandData\nElaboração: @gustavoovital') +
  
  theme_hc() +
  theme(legend.position = 'bottom',
        text = element_text(family = 'Bookman', colour = 'gray45'),
        plot.title = element_text(size = 23, colour = 'gray25'),
        plot.subtitle = element_text(size = 17),
        plot.caption = element_text(size = 15, colour = 'gray45'),
        plot.title.position = 'plot') 

# 1000 casos, e depois? ----

data_wider %>% 
  filter(`Country/Region` == 'Italy' | 
           `Country/Region` == 'US' | 
           `Country/Region` == 'Brazil'| 
           `Country/Region` == 'Spain' | 
           `Country/Region` == 'Iran'| 
           `Country/Region` == 'France') -> data_1000

data_1000 %>%  filter(`Country/Region` == 'US', Confirmed > 20000) -> confirmed_US
data_1000 %>%  filter(`Country/Region` == 'Italy', Confirmed > 20000) -> confirmed_Italy
data_1000 %>%  filter(`Country/Region` == 'Brazil', Confirmed <= 20000) -> confirmed_Brazil
data_1000 %>%  filter(`Country/Region` == 'Spain', Confirmed > 20000) -> confirmed_Spain
data_1000 %>%  filter(`Country/Region` == 'Iran', Confirmed > 20000) -> confirmed_Iran
data_1000 %>%  filter(`Country/Region` == 'France', Confirmed > 20000) -> confirmed_France

confirmed_Brazil <-  confirmed_Brazil$Confirmed

data_fore <- matrix(ncol = 6, nrow = (length(confirmed_Brazil) + nrow(confirmed_US)))

data_fore[1:length(confirmed_Brazil), 1] <- confirmed_Brazil
data_fore[81:102, 2] <- confirmed_US$Confirmed
data_fore[81:102, 3] <- confirmed_Italy$Confirmed[1:22]
data_fore[81:102, 4] <- confirmed_Spain$Confirmed[1:22]
data_fore[81:102, 5] <- confirmed_Iran$Confirmed[1:22]
data_fore[81:102, 6] <- confirmed_France$Confirmed[1:22]


colnames(data_fore) <- c('Brazil', 'US', 'Italy', 'Spain', 'Iran', 'France')
data_fore <- as.data.frame(data_fore)

data_fore$Dias <- c(rep(NA, 80), seq(1:22))
data_fore$Indice <- seq(1:nrow(data_fore))


data_fore %>% 
  ggplot(aes(Indice)) +
  geom_line(aes(y = Brazil, colour = 'Brasil'), size = 2, alpha = .4) + 
  geom_line(aes(y = US, colour = 'EUA'), size = 2, alpha = .4) +
  geom_line(aes(y = Italy, colour = 'Itália'), size = 2, alpha = .4) +
  geom_line(aes(y = France, colour = 'França'), size = 2, alpha = .4) +
  geom_line(aes(y = Spain, colour = 'Espanha'), size = 2, alpha = .4) +
  geom_line(aes(y = Iran, colour = 'Iran'), size = 2, alpha = .4) +
  
  scale_colour_manual(values = viridis::inferno(8), name = '') +
  
  labs(title = expression(bold('Evolução dos Casos de Coronavirus')~'(Pós 20.000 Casos Confirmados)'),
       subtitle = 'Brasil, EUA, Iran, Espanha, França, Itália',
       x = 'Dias após o 20.000 caso', y = 'Número de Casos',
       caption = 'Fonte: CSSEGISandData\nElaboração: @gustavoovital') +
  
  geom_vline(xintercept = 80.5, alpha = .05, size = .8) +
  geom_hline(yintercept = 20000, alpha = .05, size = .8) +
  
  scale_x_continuous(limits = c(75, 100),
                     breaks = 75:100,
                     label = c(rep('', 6), 1:20)) +
  
  theme_hc() +
  theme(plot.title.position = 'plot',
        text = element_text(family = 'times'),
        plot.title = element_text(size = 20, colour = 'gray20'),
        plot.subtitle = element_text(size = 18, colour = 'gray40'),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 18, hjust = 0, vjust=-1, colour = 'gray20'),
        axis.title.y = element_text(size = 18, colour = 'gray20'),
        axis.text.x = element_text(size = 15, colour = 'gray40'),
        axis.text.y = element_text(size = 15, colour = 'gray40'),
        plot.caption = element_text(size = 20, colour = 'gray25'),
        legend.text = element_text(color = "gray40", size = 15))



  # Brasil, EUA, Italia ----

data_wider %>% 
  filter(`Country/Region` == 'Italy' | 
           `Country/Region` == 'US' | 
           `Country/Region` == 'Brazil'| 
           `Country/Region` == 'Spain' | 
           `Country/Region` == 'Germany'| 
           `Country/Region` == 'France') -> compared

compared %>% filter(`Country/Region` == 'Brazil', Confirmed > 99) -> confirmed_brazil
compared %>% filter(`Country/Region` == 'Spain', Confirmed > 99) -> confirmed_Spain
compared %>% filter(`Country/Region` == 'Germany', Confirmed > 99) -> confirmed_Germany
compared %>% filter(`Country/Region` == 'France', Confirmed > 99) -> confirmed_France
compared %>% filter(`Country/Region` == 'US', Confirmed > 99) -> confirmed_US
compared %>% filter(`Country/Region` == 'Italy', Confirmed > 99) -> confirmed_italy

confirmed_brazil$days <- 1:(nrow(confirmed_brazil))
confirmed_US$days <- 1:(nrow(confirmed_US))
confirmed_italy$days <- 1:(nrow(confirmed_italy))
confirmed_Spain$days <- 1:(nrow(confirmed_Spain))
confirmed_France$days <- 1:(nrow(confirmed_France))
confirmed_Germany$days <- 1:(nrow(confirmed_Germany))


Confirmed <- tibble(Days = c(confirmed_brazil$days, confirmed_US$days, confirmed_italy$days, confirmed_Spain$days, confirmed_France$days, confirmed_Germany$days),
                    Country = c(confirmed_brazil$`Country/Region`, confirmed_US$`Country/Region`, confirmed_italy$`Country/Region`, confirmed_Spain$`Country/Region`, confirmed_France$`Country/Region`, confirmed_Germany$`Country/Region`),
                    Confirmed = c(confirmed_brazil$Confirmed, confirmed_US$Confirmed, confirmed_italy$Confirmed, confirmed_Spain$Confirmed, confirmed_France$Confirmed, confirmed_Germany$Confirmed))



ggplot(NULL) +
  geom_bar(aes(y = Confirmed, fill = 'EUA', x = days), data = confirmed_US, stat = "identity",  alpha = .3, size = .8) +
  geom_bar(aes(y = Confirmed, fill = 'Italia', x = days), data = confirmed_italy, stat = "identity",  alpha = .3, size = .8) +
  geom_bar(aes(y = Confirmed, fill = 'Brasil', x = days), data = confirmed_brazil, stat = "identity",  alpha = .9, size = .8) +
  geom_bar(aes(y = Confirmed, fill = 'Espanha', x = days), data = confirmed_Spain, stat = "identity",  alpha = .3, size = .8) +
  geom_bar(aes(y = Confirmed, fill = 'França', x = days), data = confirmed_France, stat = "identity",  alpha = .3, size = .8) +
  geom_bar(aes(y = Confirmed, fill = 'Alemanha', x = days), data = confirmed_Germany, stat = "identity",  alpha = .3, size = .8) +
  
  scale_fill_manual(values = viridis::viridis(7), name = NULL) +
  labs(title = expression(bold('Novos Casos de CoronaVirus')~'(> 100 Casos)'), subtitle = 'Comparação entre Itália x Brasil x EUA', x = 'Dias a partir do 100 caso confirmado', y = NULL,
       caption = 'Fonte: CSSEGISandData\nElaboração: @gustavoovital') +
  theme_bw() +
  theme(plot.title.position = 'plot',
        text = element_text(family = 'Bookman'),
        plot.title = element_text(size = 25, colour = 'gray25'),
        plot.subtitle = element_text(size = 25, colour = 'gray45'),
        plot.caption = element_text(size = 15, colour = 'gray45'))
  

data_wider %>% 
  filter(`Country/Region` == 'Italy' | `Country/Region` == 'US' | `Country/Region` == 'Brazil'| `Country/Region` == 'Spain' | `Country/Region` == 'Germany'| `Country/Region` == 'France') -> compared

compared <- compared %>% 
  mutate(Mortalidade = Deaths/Confirmed,
         Mortalidade_fct = ifelse(Mortalidade == 0, '0%', 
                              ifelse(Mortalidade > 0 & Mortalidade < 0.05, '[0% - 5%)',
                                     ifelse(Mortalidade >= 0.05 & Mortalidade < 0.075, '[5% - 7,5%)',
                                            ifelse(Mortalidade >= 0.075 & Mortalidade < 0.1,
                                                   '[7,5% - 10%)', '>10%')))))

compared$Mortalidade_fct <- ifelse(is.na(compared$Mortalidade), '0%', compared$Mortalidade_fct)
compared$Mortalidade_fct <- factor(compared$Mortalidade_fct, levels = c('0%', '[0% - 5%)', '[5% - 7,5%)', '[7,5% - 10%)', '>10%'))



compared %>% 
  filter(Date > as.Date('2020-02-29')) %>% 
  ggplot(aes(Date, Ativos, colour = `Country/Region`, size = Mortalidade_fct)) +
  geom_point(aes(group = seq_along(Date)), alpha = .5) +
  geom_line(size = .1) +
  scale_colour_manual(values = viridis::viridis(7), name = NULL, guide = "none") +
  scale_size_manual(values = c(2, 5, 7, 10, 17), name = 'Taxa de Mortalidade') +
  geom_text(aes(x = max(Date)+1.3, label = sprintf("%s", `Country/Region`)), hjust=0, size = 5) +
  scale_x_date(limits = c(as.Date('2020-02-29'), as.Date('2020-03-30')), date_breaks = '1 week', labels=scales::date_format("%D")) +
  
  labs(title = 'Evolução da Taxa de Mortalidade e do Número de Casos Ativos \ndo COVID-19 no Mês de Março de 2020',
       subtitle = 'Subdivisão por Paises, Conforme Especificado', x = NULL, y = 'Número de Casos',
       caption = 'Fonte: CSSEGISandData\nElaboração: @gustavoovital/PET-Economia UFF') +
  
  guides(size = guide_legend(title.position="top", title.hjust = 0.5)) +
  
  theme_hc() +
  theme(text = element_text(family = 'Bookman'),
        plot.title = element_text(size = 20, colour = 'gray30'),
        plot.subtitle = element_text(size = 16, colour = 'gray40'),
        plot.caption = element_text(size = 15, colour = 'gray45')) +
  transition_reveal(Date) -> gif_pet

gif <- animate(gif_pet, fps = 10, width = 700)
magick::image_write(gif, path="gif_pet.gif")
