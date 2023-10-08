
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(DT)

ui <- fluidPage(
    titlePanel("Números Aleatórios"),
    sidebarLayout(
        sidebarPanel(
            selectInput("dist", "Distribuição de Probabilidades",
                        choices=c("Normal", "Uniforme", "Exponencial")),
            numericInput("Tamanho_Amostral", "Tamanho Amostral",
                         min=10, max=10000, value=1000),
            textInput("titulo", "Título do Gráfico"),
            sliderInput("n_numeros", "Quantos Nº Exibir",
                        min=10, max=1000, value=100)
        ),
        mainPanel(
            plotOutput("hist"),
            verbatimTextOutput("numeros")
            
        )
    ),
)



server <- function(input, output){

# Distribuições de Probabilidades
x <- reactive({
    switch(input$dist,
           "Normal" = rnorm(input$Tamanho_Amostral,mean = 0, sd = 1),
           "Uniforme" = runif(input$Tamanho_Amostral, min = 0, max = 100),
           "Exponencial" = rexp(input$Tamanho_Amostral)
           )
    }) 

# Histograma
# Semente p/ Aleatorização
set.seed(7)
    
output$hist <- renderPlot({
    #figura1 <- 
        
        hist(x(), main = input$titulo, 
         ylab = "Frequência", 
         xlab = "parâmetro",
         col = "red",
         breaks = "Sturges"
         )
    #ggplotly(figura1)
    })
    
    
# Números

output$numeros <- renderPrint({
    head(x(), input$n_numeros)
    })



}



shinyApp(ui, server)

