# Analise dos resultados das tabelas em relação ao covid-19
# 
# Autor: @gustavoovital
# Data: 18/03/2020

# pacotes ----

library(tidyverse)

# base de dados ----

data <- readRDS('data.rds')
data_wider <- readRDS('data_wider.rds')

# analise grafica ----

# China ----

data %>% 
  filter(`Country/Region` == 'Italy') %>% 
  ggplot(aes(x = Date, y = Total, colour = Case)) +
  geom_line()
