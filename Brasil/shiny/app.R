# shinyapp

library(shiny)
library(shinydashboard)
library(plotly)
library(viridis)
library(ggthemes)
library(tidyverse)

# Base de dados ----

# source('www/scripts/get_data.R') # DESMARCAR SENAO ESSA PORRA NAO VAI RODAR

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

# colnames(data_cidades) <- 

names(cidades) <- cidades

cidades_dois <- c(NA, cidades)
names(cidades_dois) <- c('-', cidades)

# UI interface ----

ui <- 
  dashboardPage(
    skin = 'blue',
                
    dashboardHeader(title = 'O COVID-19 NAS CIDADES DO BRASIL', titleWidth = 450),
    dashboardSidebar(width = 450,
                     
                     sidebarMenu(
                       
                       menuItem('Sobre o DashBoard', tabName = 'info', icon = icon('ello')),
                       menuItem('Evolução das Maiores Cidades do Brasil', tabName = 'bigCities', icon = icon('chart-bar')),
                       menuItem('Comparações entre as Cidades', tabName = 'compCities', icon = icon('chart-bar'))
                     )),
    dashboardBody(
      
      tabItems(
        
        tabItem(tabName = 'info'),
        tabItem(tabName = 'bigCities'),
        
        # evolução geral ----
        
        tabItem(tabName = 'compCities',
                
                sidebarLayout(position = "right",
                              sidebarPanel(
                                h4('O Gráfico ao lado mostra a evolução dos casos de CoronaVirus por valores absolutos de uma cidade - isto é - sem levar em consideração uma ponderação por tamanho populacional. É possível ainda uma linha de tendência para a visualização.'),
                                selectInput("city", "Qual Cidade Desejada?", choices = cidades, selected = 'Niterói'),
                                selectInput("city2", "Deseja comparar com Alguma outra Cidade?", choices = cidades_dois, selected = '-'),
                                br(),
                                h4(span('Abaixo, gráfico com a variação dos casos em escala logarítimica', style = 'color:black; font-weight: bold')),
                                plotlyOutput('evol_log'),
                              ),
                              
                              mainPanel(
                                box(status = 'primary', plotlyOutput('evol', height = 800), width = '90%',
                                    selectizeInput('log', span('Deseja visualizar com linha de tendência?'), choices = c('Sim', 'Não'), select = 'Não'))
                                
                                
                              )
                )
        )
      )
    )
  )

# Server interface ----

server <- function(input, output) {
  
  output$grafico1 <- renderPlotly({
    
    
    data_cidades %>% 
      filter(Cidade == input$city | Cidade == input$city2) %>% 
      ggplot(aes(Data, `Novos casos`, fill = Cidade)) +
      geom_col(position = 'dodge', alpha = .6) +
      
      scale_fill_manual(values = magma(3)) +
      
      labs(title = 'Novos Casos de Coronavirus por Dia', x = NULL) +
      theme_economist() -> grafico1
    
      ggplotly(grafico1) %>% 
        config(displayModeBar = F) %>%
        layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
        
      
  })
  
  output$grafico2 <- renderPlotly({
    
    
    data_cidades %>% 
      filter(Cidade == input$city | Cidade == input$city2) %>% 
      ggplot(aes(Data, `Novas Mortes`, colour = Cidade)) +
      geom_line(alpha = .6) +
      
      scale_colour_manual(values = magma(3)) +
      
      labs(x = NULL) +
      theme_economist() -> grafico1
    
    ggplotly(grafico1) %>% 
      config(displayModeBar = F) %>%
      layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
    
  })
  
  # Evolução dos casos ----
  
  output$evol <- renderPlotly({
    
    
    data_cidades %>% 
      filter(Cidade == input$city | Cidade == input$city2) %>% 
      ggplot(aes(Data, Confirmados, colour = Cidade, size = `Taxa de Mortalidade`)) +
      # geom_point(alpha = .6) +
      geom_point(show.legend = FALSE, alpha = .6) +

      scale_colour_manual(values = magma(3)) +

      labs(title = 'Evolução dos Casos de CoronaVirus. \nE Taxas de Mortalidades pelas cidades', x = NULL) +
      theme_minimal() +
      theme(plot.title.position = 'plot') -> evol
    
    if(input$log == 'Sim'){
      
      ggplotly(evol + geom_smooth(alpha = .1, se = FALSE)) %>% 
        config(displayModeBar = T) %>%
        layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
      
    } else{
      
      ggplotly(evol) %>% 
        config(displayModeBar = T) %>%
        layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
    }
    
    
  })
  
  # Evolução LOG ----
  
  output$evol_log <- renderPlotly({
    
    
    data_cidades %>% 
      filter(Cidade == input$city | Cidade == input$city2) %>% 
      ggplot(aes(Data, Confirmados, colour = Cidade)) +
      # geom_point(alpha = .6) +
      geom_line(size = 1, show.legend = FALSE, alpha = .6) +
      
      scale_colour_manual(values = magma(3)) +
      scale_y_log10() +
      
      labs(x = NULL) +
      theme_minimal() +
      theme(plot.title.position = 'plot') -> evol_log

      
      ggplotly(evol_log) %>% 
        config(displayModeBar = F) %>%
        layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
    
    
    
  })
  
}

# Run shiny ----

shinyApp(ui = ui, server = server)
