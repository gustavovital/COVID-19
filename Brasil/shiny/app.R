# shinyapp

require(shiny)
require(shinydashboard)
require(plotly)
require(viridis)

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
  
  theme = shinythemes::shinytheme('yeti'),
  
  titlePanel('COVID-19 Brasil'),
  sidebarLayout(position = "left",
    sidebarPanel(
      selectInput("city", "Qual Cidade Desejada?", choices = cidades, selected = 'Niterói'),
      selectInput("city2", "Deseja comparar com Alguma outra Cidade?", choices = cidades_dois, selected = '-')
                 ),
    
      mainPanel(
        plotlyOutput('grafico3'),
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
      
      scale_fill_manual(values = viridis(3)) +
      
      labs(title = 'Novos Casos de Coronavirus por Dia') +
      theme_gray() -> grafico1
    
      ggplotly(grafico1)
      
      
        
      
  })
  
  output$grafico2 <- renderPlotly({
    
    
    data_cidades %>% 
      filter(Cidade == input$city | Cidade == input$city2) %>% 
      ggplot(aes(Data, `Novas Mortes`, fill = Cidade)) +
      geom_col(position = 'dodge', alpha = .6) +
      
      scale_fill_manual(values = inferno(4)) +
      
      labs(title = 'Novas Mortes por Coronavirus por Dia') +
      theme_gray() -> grafico1
    
    ggplotly(grafico1)
    
  })
  
  output$grafico3 <- renderPlotly({
    
    
    data_cidades %>% 
      filter(Cidade == input$city | Cidade == input$city2) %>% 
      ggplot(aes(Data, Confirmados, colour = Cidade, size = `Taxa de Mortalidade`)) +
      geom_point(alpha = .6) +
      geom_line(size = .5, show.legend = FALSE) +
      
      scale_colour_manual(values = magma(3)) +

      labs(title = 'Evolução dos Casos de Coronavirus.') +
      theme_gray() -> grafico1
    
    ggplotly(grafico1)
    
  })
  
}

# Run shiny ----

shinyApp(ui = ui, server = server)
