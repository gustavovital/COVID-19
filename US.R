# Script para uma análise quantitativa, inicialmente visual da questão do
# Corona Virus nos EUA
# 
# Autor: @gustavoovital
# Data: 03/04/2020

# Pacotes necessários ----

library(tidyverse)

rm(list = ls())

confirmed_US <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv')
deaths_US <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv')
