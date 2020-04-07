# Pegar base de dados das cidades e estados do Brasil, de acordo com a base de dados 
# disponibilizadas pelo brasil.io
# 
# Autor: @gustavoovital
# Data: 06/04/2020

# start_time <- Sys.time()

library(tidyverse)
library(data.table)

dt = fread("https://data.brasil.io/dataset/covid19/caso-full.csv.gz")

dt %>% 
  mutate(Data = as.Date(date),
         Confirmados = ifelse(is.na(last_available_confirmed) == TRUE, 0, last_available_confirmed)) %>% 
  filter(city == 'Niterói' | city == 'Rio de Janeiro' | city == 'São Gonçalo', is_repeated == FALSE) %>% 
  ggplot(aes(x = Data, y = Confirmados, colour = city)) +
  geom_line()

# end_time <- Sys.time()
# end_time - start_time

