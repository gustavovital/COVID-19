# Analise dos resultados das tabelas em relação ao covid-19
# 
# Autor: @gustavoovital
# Data: 18/03/2020

# pacotes ----

library(tidyverse)

# base de dados ----

data <- readRDS('data.rds')

# analise grafica ----

# China ----

data %>% 