# shinyapp

require(shiny)

# Base de dados ----

source('www/scripts/get_data.R')

data_estados <- readRDS("www/datas/data_estados.rds")
data_cidades <- readRDS("www/datas/data_cidades.rds")

data_cidades %>% 
  filter(Cidade != '') -> data_cidades

# UI interface ----

ui <- fluidPage(
  
  titlePanel('COVID-19 Brasil'),
  sidebarLayout(
    
    sidebarPanel(textInput("city", "Qual Cidade Desejada?", 'Niterói'),
                 textInput("city2", "Qual Cidade Desejada?", 'Rio de Janeiro')),
    mainPanel(plotlyOutput('grafico1')
    )
  )
)

# Server interface ----

server <- function(input, output) {
  
  output$grafico1 <- renderPlotly({
    
    
    data_cidades %>% 
      filter(Cidade == input$city | Cidade == input$city2) %>% 
      ggplot(aes(Data, Confirmados, colour = Cidade)) +
      geom_line(size = 3, alpha = .4) +
      geom_point(size = 3, alpha = .4) +
      
      labs(title = 'Evolução dos Casos de Coronavirus.') +

      
      theme_gray() -> grafico1
    
    
    ggplotly(grafico1)
      
  })
  
}

# Run shiny ----

shinyApp(ui = ui, server = server)
