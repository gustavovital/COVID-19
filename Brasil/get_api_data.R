# Pegar base de dados das cidades e estados do Brasil, de acordo com a base de dados 
# disponibilizadas pelo brasil.io
# 
# Autor: @gustavoovital
# Data: 06/04/2020

# start_time <- Sys.time()

# Pacotes Necessários ----

library(tidyverse)
library(data.table)
library(plotly)
library(viridis)

# Base de dados -----

dt = fread("https://data.brasil.io/dataset/covid19/caso_full.csv.gz")

# Base de dados para os estados do Brasil ----

# colnames(dt)

dt %>% 
  na.omit() %>%
  filter(is_repeated == FALSE) %>% 
  mutate(Data = as.Date(date)) %>% 
  group_by(Data, state) %>% 
  summarise(Confirmados = sum(last_available_confirmed), 
            `Novos casos` = sum(new_confirmed),
            `Novas Mortes` = sum(new_deaths)) -> data_estados

# Base de dados para as cidades do Brasil ----

dt %>% 
  na.omit() %>%
  filter(is_repeated == FALSE) %>% 
  mutate(Data = as.Date(date),
         Confirmados = last_available_confirmed,
         Cidade = city,
         `População Estimada` = estimated_population_2019,
         `Novos casos` = new_confirmed,
         `Novas Mortes` = new_deaths,
         `Taxa de Mortalidade` = last_available_death_rate) %>% 
  select(Data, Confirmados, Cidade, `População Estimada`, `Novos casos`, `Novas Mortes`, `Taxa de Mortalidade`) -> data_cidades

saveRDS(data_estados, "Brasil/data_estados.rds")
saveRDS(data_cidades, "Brasil/data_cidades.rds")

rm(list = ls())  

# end_time <- Sys.time()
# end_time - start_time 

# Time difference of 1.765079 secs

 
data_cidades %>% 
  group_by(`Novas Mortes`, Data, Cidade) %>% 
  summarise(`Mortes Totais` = sum(`Novas Mortes`)) %>% 
  filter(Cidade == 'Rio de Janeiro') %>% 
  ggplot(aes(Data, `Novas Mortes`, colour = Cidade)) +
  geom_line(size = 3, alpha = .4) +
  geom_point(size = 3, alpha = .4) +
  
  labs(title = 'Evolução dos Casos de Coronavirus.') +
  

  theme(legend.position = 'bottom') -> grafico1; ggplotly(grafico1)


data_cidades %>% 
  filter(Cidade != '') %>% 
  select(Cidade) %>% 
  unique() %>% 
  count(Cidade) %>% 
  select(Cidade) %>% 
  as.vector() %>%
  unlist() -> cidades

cidade_names <- list(cidades)
names(cidade_names) <- cidades

data_cidades %>% 
  filter(Cidade != '') %>% 
  filter(Cidade == 'Rio de Janeiro' | Cidade == 'Niterói') %>% 
  ggplot(aes(Data, `Novos casos`, fill = Cidade)) +
  geom_col(position = 'dodge') +
  
  scale_fill_manual(values = inferno(3)) +
  
  labs(title = 'Evolução dos Casos de Coronavirus.') 
  

  