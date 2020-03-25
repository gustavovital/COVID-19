# Script para uma análise quantitativa, inicialmente visual da questão do
# Corona Virus no Mundo
# 
# Autor: @gustavoovital
# Data: 17/03/2020

# Pacotes necessários ----

library(tidyverse)

# Dase de dados em TS ----

# A partir do CSSEGISandData, se extrai as infos sobre o COVID-19, com atualizações diárias para o mundo, 
# em especial, China. 

confirmed <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv')
deaths <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv')
recovered <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv')

start <- as.Date('2020-01-22')

data.confirmed <- seq(start, length.out = length(confirmed[,5:ncol(confirmed)]), by = 'd')
data.deaths <- seq(start, length.out = length(deaths[,5:ncol(deaths)]), by = 'd')
data.recovered <- seq(start, length.out = length(recovered[,5:ncol(recovered)]), by = 'd')

# manipulação dos dados ----

confirmed_clean <- confirmed %>% 
  pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), names_to = 'Date', values_to = 'Total') 

deaths_clean <- deaths %>% 
  pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), names_to = 'Date', values_to = 'Total') 

recovered_clean <- recovered %>% 
  pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), names_to = 'Date', values_to = 'Total') 

confirmed_clean$Date <- rep(data.confirmed, nrow(confirmed))
deaths_clean$Date <- rep(data.confirmed, nrow(deaths))
recovered_clean$Date <- rep(data.confirmed, nrow(recovered))

confirmed_clean$Case <- 'Confirmed'
deaths_clean$Case <- 'Deaths'
recovered_clean$Case <- 'Recovered'

data <- confirmed_clean %>% 
  full_join(deaths_clean) %>% 
  full_join(recovered_clean) %>% 
  select(`Country/Region`, Date, Total, Case) %>% 
  group_by(Date, `Country/Region`, Case) %>% 
  summarise(Total = sum(Total)) 
  
data_wider <- data %>% 
  pivot_wider(names_from = Case, values_from = Total)

data_wider <- data_wider %>% 
  mutate(Ativos = Confirmed - Deaths - Recovered)
  
saveRDS(data, "data.rds")
saveRDS(data_wider, "data_wider.rds")

rm(list = ls())
