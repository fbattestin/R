####################################################################################
#Exercicio: construindo app shiny
####################################################################################

#setando o diretorio de trabalho (work directory)
setwd("C:/Fabio Battestin/Repo/r/material/cursos/R")

#lendo arquivos csv (criando dataset)
iris <- read.csv("Iris.csv")

head(iris)

#instalando pacote shiny para desenvolvimento de APP UI.
install.packages("shiny")

#carregando biblioteca
library("shiny")
library(tree)

#criando User Interface
ui <- fluidPage("Hell World!")


#criando funcao server
server <- function(input,output){}

#criando app
shinyApp(
    ui = ui
    ,server = server
)

#criando ui com interacao de entrada e saida de parametros
ui <- fluidPage(
    titlePanel("Input and Output")
    ,sidebarLayout(
      sidebarPanel(
        sliderInput(
          inputId = "number"
          ,label = "choose a number"
          ,min = 0
          ,max = 100
          ,value = 50))
      ,mainPanel(
        textOutput(
          outputId = "text"))))

#criando funcao
server <- function(input, output){
  output$text <- renderText({
    paste("you selected:", input$num )})
}

shinyApp(
  ui = ui
  ,server = server
)


#### criando app a partir do modelo criado no execicio de machine learning

#setando o diretorio de trabalho (work directory)
setwd("C:/Fabio Battestin/Repo/r/material/cursos/R")

#carregando modelo em memoria 
# o modelo criado no exercicio de machine learning
# ja contem a variavel "model" que sera carregada em memoria
load("Tree.RData")

#carregando biblioteca de cores
library(RColorBrewer)

#criando variavel com a paleta de cores
palette <- brewer.pal(3,"Set2")

# criando interface do usuario
# interface devera conter dois sliders numericos uma para o comprimento e outro para largura
# da petala.
# para a saida sera plotado a visualizacao dos dados e um bloco de texto
# informando qual especies o modelo preve

head(iris)

ui <- fluidPage(
  titlePanel("Iris - Species Predictor")
  ,sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "petal.length"
        ,label = "petal length (cm)"
        ,min = 1
        ,max = 7
        ,value = 4)
      ,sliderInput(
        inputId = "petal.width"
        ,label = "petal width (cm)"
        ,min = 0.0
        ,max = 2.5
        ,step = 0.5
        ,value = 1.5))
    ,mainPanel(
      textOutput(
        outputId = "text")
      ,plotOutput(
        outputId = "plot"))))

#criando codigo do server
server <- function(input, output) {
  output$text = renderText({
    
    #no body da funcao sera criado um data.frame com quatro colunas e uma linha
    predictors <- data.frame(
       petal.length = input$petal.length
      ,petal.width = input$petal.width
      ,sepal.length = 0
      ,sepal.width = 0)
    
    # fazendo predicao
    predictions = predict(
      object = model
      ,newdata = predictors
      ,type = "class")
    
    paste("The predicted specie is: ",
          as.character(predictions))
  })
  
  output$plot = renderPlot({
    #criando scatterplot
    # criando plot de ponto distribuido separando as especieis por cor
    plot(
      x = iris$petal.length
      ,y = iris$petal.width
      ,pch = 19
      ,col = palette[as.numeric(iris$species)]
      ,main = "Iris Comprimento Vs. Largura da Petala"
      ,xlab = "Petal Length(cm)"
      ,ylab = "Petal Width(cm)"
    )
    
    #plotando fronteiras de decisao 
    partition.tree(
      tree = model
      ,label = "Species"
      ,add = TRUE)
    
    # desenhando valor previsto na area de plotagem
    points(
      x = input$petal.length
      ,y = input$petal.width
      ,col = "green"
      ,pch = 5
      ,cex = 2
      ,lwd = 2)
  })
}

##criando a shiny app
shinyApp(
  ui = ui
  ,server = server)
