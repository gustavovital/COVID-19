# shinyapp

library(shiny)
library(shinydashboard)
library(plotly)
library(viridis)
library(ggthemes)

# Base de dados ----

source('www/scripts/get_data.R')

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

names(cidades) <- cidades

cidades_dois <- c(NA, cidades)
names(cidades_dois) <- c('-', cidades)

# UI interface ----

ui <- fluidPage(
  
  theme = shinythemes::shinytheme('superhero'),
  
  h1('COVID-19 no Brasil'), 
  h3('Uma comparação de casos e mortes nas cidades brasileiras'),
  sidebarLayout(position = "left",
    sidebarPanel(
      selectInput("city", "Qual Cidade Desejada?", choices = cidades, selected = 'Niterói'),
      selectInput("city2", "Deseja comparar com Alguma outra Cidade?", choices = cidades_dois, selected = '-')
                 ),
    
      mainPanel(
        plotlyOutput('grafico3'),
        br(),
        fluidRow(
          column(6, plotlyOutput('grafico1')),
          column(6, plotlyOutput('grafico2')),
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
      
      labs(title = 'Novas Mortes por Coronavirus por Dia', x = NULL) +
      theme_economist() -> grafico1
    
    ggplotly(grafico1) %>% 
      config(displayModeBar = F) %>%
      layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
    
  })
  
  output$grafico3 <- renderPlotly({
    
    
    data_cidades %>% 
      filter(Cidade == input$city | Cidade == input$city2) %>% 
      ggplot(aes(Data, Confirmados, colour = Cidade, size = `Taxa de Mortalidade`)) +
      geom_point(alpha = .6) +
      geom_line(size = .5, show.legend = FALSE) +
      
      scale_colour_manual(values = magma(3)) +

      labs(title = 'Evolução dos Casos de Coronavirus.', x = NULL) +
      theme_economist()  -> grafico1
    
    ggplotly(grafico1) %>% 
      config(displayModeBar = F) %>%
      layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
    
  })
  
}

# Run shiny ----

shinyApp(ui = ui, server = server)
