# Scripts para graficos bases.
# 
# Autor: @gustavoovital
# Data: 14/05/05

# base de dados ----

data_estados <- readRDS("www/datas/data_estados.rds")
data_cidades <- readRDS("www/datas/data_cidades.rds")

data_cidades %>% 
  filter(Cidade != '') -> data_cidades

data_cidades %>% 
  filter(Cidade != '') %>% 
  select(Cidade) %>% 
  unique() %>% 
  count(Cidade) %>% 
  select(Cidade) %>% 
  as.vector() %>%
  unlist() -> cidades

head(data_cidades)

# Evolução por tamanho populacional ----

data_cidades %>% 
  mutate(Confirmados_pop = Confirmados/Pop) %>% 
  filter(Cidade == 'São Paulo' | Cidade == 'Rio de Janeiro') %>% 
  ggplot(aes(Data, Confirmados_Pop, colour = Cidade)) +
  scale_y_continuous(labels=scales::percent) +
  geom_line()

data_cidades %>% 
  mutate(Confirmados_Pop = Confirmados/Pop) %>% 
  group_by(Cidade) %>%
  filter(Data == max(Data)) %>% 
  mutate(rank=dense_rank(desc(Confirmados_Pop)))
  
  
  

today
