# get_data

library(tidyverse)
library(data.table)
library(R.utils)

# Base de dados -----

dt = fread("https://data.brasil.io/dataset/covid19/caso_full.csv.gz", encoding = 'UTF-8')

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

saveRDS(data_estados, "www/datas/data_estados.rds")
saveRDS(data_cidades, "www/datas/data_cidades.rds")

rm(list = ls())  