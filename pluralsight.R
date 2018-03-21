#instalando packages
install.packages("dplyr")

#carregandos packages
library("dplyr")

#help
?data.frame


x <-"Hello World"
print(x)

# funcao
x <- 0.0
f <- function(X) { x + 5.0 }
f(5)

#matriz
m <- matrix(data = 10:1, nrow = 5, ncol= 2)
m

matriz <- matrix(10:1,5,2)
matriz

#comparando matriz
m == matriz
identical(m,matriz)


#data frame 
df <- data.frame(
      name = c("maguini","lola","rapi")
      ,qtos = c(1,3,1)
      ,chatonilda = c("sim","sim","sim"))
df

#indexing (ou select, sendo dataset[linha,coluna])
df[1,2]

#indexing sem especificar o numero da linha buscada, trara todas da coluna 1
df [ , 1]

# indexing atraves de coluna nomeada
df$chatonilda

#subset
#ordem das colunas no subset
df[c(3,2,1)]

#por range de colunas
df [1:2]

# filtro pela coluna
df[df$chatonilda %in% c("sim")]
df[df$name == "rapi",] #nao esquecer do virgula no final para resultado total do dataset
df[df$qtos > 2,]


##################################################################################
## Exercicio: Motor Trend Car Road Tests
##################################################################################

# Format
# A data frame with 32 observations on 11 variables.
# 
# [, 1]	mpg	Miles/(US) gallon
# [, 2]	cyl	Number of cylinders
# [, 3]	disp	Displacement (cu.in.)
# [, 4]	hp	Gross horsepower
# [, 5]	drat	Rear axle ratio
# [, 6]	wt	Weight (1000 lbs)
# [, 7]	qsec	1/4 mile time
# [, 8]	vs	V/S
# [, 9]	am	Transmission (0 = automatic, 1 = manual)
# [,10]	gear	Number of forward gears
# [,11]	carb	Number of carburetors

#setando o diretorio origem do csv
setwd("C:/Fabio Battestin/Repo/r/material/cursos/R")

# criando tabela 
cars <- read.table(
  file = "mtcars.csv"
  ,header = TRUE
  ,sep ="," #separador TAB "\t"
)
head(cars)

# carregando bliblioteca dplyr
library(dplyr)

# selecionando um subset de colunas
temp <- select(
  .data = cars
  ,am #am	Transmission (0 = automatic, 1 = manual) 
  ,cyl #cyl	Number of cylinders
  ,mpg) #mpg	Miles/(US) gallon


head(temp)

#selecionando um subset de linhas com transmissao automatica
temp <- filter(
  .data = temp
  ,am ==0)

head(temp)

#convertendo o consumo de milhas para kilometros por hora
#mutate algo como um DDL de crete column + UPDATE
temp <- mutate(
  .data = temp
  ,consumo = mpg * 0.425)

head(temp)

#group by
temp <- group_by(
  .data = temp
  ,cyl 
)

head(temp)
# am   cyl   mpg consumo
# <int> <int> <dbl>   <dbl>
# 1     0     6  21.4    9.09
# 2     0     8  18.7    7.95
# 3     0     6  18.1    7.69
# 4     0     8  14.3    6.08
# 5     0     4  24.4    10.4 
# 6     0     4  22.8    9.69


#agrupamento baseado em grupos
temp <- summarize(
  .data = temp
  ,Avg.Consumo = mean(consumo)
)

head(temp)

#ordenando resultado 
temp <- arrange(
  .data = temp
  ,Avg.Consumo
  #para valor descendente desc(Avg.Consumo)
)
head(temp)

#convertendo para dataframe
eficiencia <- temp

print(eficiencia)


# metodo CHAIN utilizando dplyr
# o operador ~pipe~ do dplyr é o %>%
# os comandos podem estar juntos para criar um codigo mais legivel
eficienciaa <- cars %>%
  select(am,cyl,mpg) %>%
  filter(am == 0) %>%
  mutate(consumo = mpg * 0.425) %>%
  group_by(cyl) %>%
  summarize(Avg.Cosumo = mean(consumo)) %>%
  arrange(Avg.Cosumo) %>%
  as.data.frame()


print(eficienciaa)

#salvando resultado em csv
write.csv(
  x = eficienciaa
  ,file = "Eficienciaa.csv"
  ,row.names = FALSE
)

#################################################################################
####################################################################################
#Exercicio: Criando estatisticas descritivas, dataset: Motor Trending
#Criando Estatísticas descritivas

# Format
# A data frame with 32 observations on 11 variables.
# 
# [, 1]	mpg	Miles/(US) gallon
# [, 2]	cyl	Number of cylinders
# [, 3]	disp	Displacement (cu.in.)
# [, 4]	hp	Gross horsepower
# [, 5]	drat	Rear axle ratio
# [, 6]	wt	Weight (1000 lbs)
# [, 7]	qsec	1/4 mile time
# [, 8]	vs	V/S
# [, 9]	am	Transmission (0 = automatic, 1 = manual)
# [,10]	gear	Number of forward gears
# [,11]	carb	Number of carburetors
####################################################################################

#setando o diretorio de trabalho (work directory)
setwd("C:/Fabio Battestin/Repo/r/material/cursos/R")

#lendo arquivos csv (criando dataset)
cars <- read.csv("mtcars.csv")

head(cars)

####################################################################################
## PRIMEIRA QUESTAO: 
## QUANTOS CARROS POR TIPO DE TRANSMISSAO TEMOS?
####################################################################################

# criando tabela de frequencia
table(cars$am)

#obtendo o menor valor de economia milhas por hora
min(cars$mpg)

#obtendo o maio valor de economia milhas por hora
max(cars$mpg)

#obtendo media de economia
mean(cars$mpg)

#obtendo mediana de economia
median(cars$mpg)

# Mediana é o valor que separa a metade maior e a metade menor de uma amostra, 
# uma população ou uma distribuição de probabilidade. Em termos mais simples, 
# mediana pode ser o valor do meio de um conjunto de dados. 
# No conjunto de dados {1, 3, 3, 6, 7, 8, 9}, por exemplo, a mediana é 6. 
# Se houver um número par de observações, não há um único valor do meio. 
# Então, a mediana é definida como a média dos dois valores do meio.


#quartil
quantile(cars$mpg)

# Na estatística descritiva, um quartil é qualquer um dos três valores que divide o 
# conjunto ordenado de dados em quatro partes iguais, e assim cada parte representa
# 1/4 da amostra ou população.
# 
# Assim, no caso duma amostra ordenada,
# 
# primeiro quartil (designado por Q1/4) = quartil inferior = é o valor aos 25% da 
# amostra ordenada = 25º percentil
# 
# segundo quartil (designado por Q2/4) = mediana = é o valor até ao qual 
# se encontra 50% da amostra ordenada = 50º percentil, ou 5º decil.
# 
# terceiro quartil (designado por Q3/4) = quartil superior = valor a partir 
# do qual se encontram 25% dos valores mais elevados = valor aos 75% da amostra 
# ordenada = 75º percentil 
# 
# à diferença entre os quartis superior e inferior 
# chama-se amplitude inter-quartil.


#desvio padrao
sd(cars$mpg)


#total (sum)
sum(cars$mpg)

#################################################################################
## Pergunta: Correlacao entre cilindros e economia de combustivel
#################################################################################

#correlacao
cor(
  x = cars$cyl
  ,y = cars$mpg
)
#[1] -0.852162
# correlacao: quanto maior o numero de cilindros, mais se gasta com combustivel.
# forte correlação negativa inversa

summary(cars)

####################################################################################
#Exercicio: Plotando graficos 
# Format
# A data frame with 32 observations on 11 variables.
# 
# [, 1]	mpg	Miles/(US) gallon
# [, 2]	cyl	Number of cylinders
# [, 3]	disp	Displacement (cu.in.)
# [, 4]	hp	Gross horsepower
# [, 5]	drat	Rear axle ratio
# [, 6]	wt	Weight (1000 lbs)
# [, 7]	qsec	1/4 mile time
# [, 8]	vs	V/S
# [, 9]	am	Transmission (0 = automatic, 1 = manual)
# [,10]	gear	Number of forward gears
# [,11]	carb	Number of carburetors
####################################################################################

#setando o diretorio de trabalho (work directory)
setwd("C:/Fabio Battestin/Repo/r/material/cursos/R")

#lendo arquivos csv (criando dataset)
cars <- read.csv("mtcars2.csv")

head(cars)

#instalando pacote de gráfico ggplot2
install.packages("ggplot2")

#carregando biblioteca de plot
library(ggplot2)

#Quantos carros existem por tipo de transmissão?
# campo: transmissão é uma variável CATEGORICA. 
ggplot(
  data = cars
  ,aes(x = am)) + 
  geom_bar() +
  ggtitle("Contagem de Carros por Tipo de Transmissão") +
  xlab("Tipo de Transmisao") +
  ylab("Contagem de Carros") 

#histograma

# Quando o volume de dados aumenta indefinidamente dentro do conjunto de dados e o 
# intervalo de classes tende a zero (o que torna os retângulos cada vez mais finos 
# e altos), a distribuição de frequência torna-se uma distribuição de densidade de 
# probabilidades. A construção de histogramas tem caráter preliminar em qualquer 
# estudo e é um importante indicador da distribuição de dados. Os histogramas 
# podem indicar se uma distribuição se aproxima de uma função normal, assim como 
# também podem indicar a mistura de populações quando se apresentam bimodais.[6]

ggplot(
  data = cars,aes(x = mpg)) +
  geom_histogram(bins = 10) +
  ggtitle("Distribuicao Economica") +
  xlab("Economia de Combustivel(mpg)") +
  ylab("Contagem de Carros") 


#densidade
ggplot(
  data = cars,aes(x = mpg)) +
  geom_density() +
  ggtitle("Distribuicao Economica") +
  xlab("Economia de Combustivel(mpg)") +
  ylab("Contagem de Carros") 

# scatter plot (ponto de distribuicao)
# é possível obeservar a correlacao entre a crescente economia de combustivel 
# conforme o numero de cilindradas diminui.

ggplot(
  data = cars
  ,aes(
    x = cyl
    ,y = mpg)) +
  geom_point() +
  ggtitle("Distribuicao Economica por Cilindradas") +
  xlab("Numero de Cilindros") +
  ylab("Economia de Combustivel(mpg)")   

####################################################################################
#Exercicio: Regressao Linear 
# Format
# A data frame with 32 observations on 11 variables.
# 
# [, 1]	mpg	Miles/(US) gallon
# [, 2]	cyl	Number of cylinders
# [, 3]	disp	Displacement (cu.in.)
# [, 4]	hp	Gross horsepower
# [, 5]	drat	Rear axle ratio
# [, 6]	wt	Weight (1000 lbs)
# [, 7]	qsec	1/4 mile time
# [, 8]	vs	V/S
# [, 9]	am	Transmission (0 = automatic, 1 = manual)
# [,10]	gear	Number of forward gears
# [,11]	carb	Number of carburetors
####################################################################################

#setando o diretorio de trabalho (work directory)
setwd("C:/Fabio Battestin/Repo/r/material/cursos/R")

#lendo arquivos csv (criando dataset)
iris <- read.csv("Iris.csv")

head(iris)

# scatter plot
plot(
  x = iris$petal.length
  ,y = iris$petal.width
  ,main = "Comprimento de Petala de Iris X Largura"
  ,xlab = "Comprimento Petala(cm)"
  ,ylab = "Largura da Petala(cm)")

#criando modelo de regressao linear
model <- lm(
  formula = petal.width ~ petal.length
  ,data = iris)

summary(model)

#plotando regressao linear
lines(
  x = iris$petal.length
  ,y = model$fitted.values 
  ,col = "red"
  ,lwd = 3) #pixel da linha

# correlacao entre comprimento e largura  
cor(
  x = iris$petal.length,
  y = iris$petal.width)
#[1] 0.9627571 ALTA correlação entre o comprimento e a largura das petalas

# prevendo novos valores para o modelo 
predict(
  object = model
  ,newdata = data.frame(
    petal.length = c(2, 5, 7))) # insiro no modelo três valores de comprimento e estimo a largura das petalas.

####################################################################################
#Exercicio: handling big data 
# Format
# A data frame with 32 observations on 11 variables.
# 
# [, 1]	mpg	Miles/(US) gallon
# [, 2]	cyl	Number of cylinders
# [, 3]	disp	Displacement (cu.in.)
# [, 4]	hp	Gross horsepower
# [, 5]	drat	Rear axle ratio
# [, 6]	wt	Weight (1000 lbs)
# [, 7]	qsec	1/4 mile time
# [, 8]	vs	V/S
# [, 9]	am	Transmission (0 = automatic, 1 = manual)
# [,10]	gear	Number of forward gears
# [,11]	carb	Number of carburetors
####################################################################################

#setando o diretorio de trabalho (work directory)
setwd("C:/Fabio Battestin/Repo/r/material/cursos/R")

#lendo arquivos csv (criando dataset)
iris <- read.csv("Iris.csv")

head(iris)

#instalando pacote big data ff
install.packages("ff")

# Acesso aos dados mapeados em memória
# mais performance, pois nao aloca todo o dataframe em memoria. 
library(ff)

# biblioteca ff cria ponteiros em memória para o arquivo físico Iris.csv
# o acesso é através de chunks do arquivo e instanciado em memória
# ff data fram

irisff <- read.table.ffdf(
  file = "Iris.csv"
  ,FUN = "read.csv")

class(irisff)
# [1] "ffdf" <-- ff data frame

head(irisff)

irisff[1:5,]


#instalando pacote biglm
install.packages("biglm")

library("biglm")

# criando modelo de regressao linear utiliando biblioteca ff
model <- biglm(
  formula = petal.width ~ petal.length
  ,data = irisff)

summary(model)

# criando scatter plot
plot(
  x = irisff$petal.length[] # [] utilizado para uso da biblioteca ff, para informar que o mapeamento da coluna refere-se ao arquivo fisico e nao aos vetores 
  ,y = irisff$petal.width[]
  ,main = "Comprimento das Petalas de Iris X Largura"
  ,xlab = "Comprimento da Petala"
  ,ylab = "Largura da Petala")

summary(model)
# Large data regression model: biglm(formula = petal.width ~ petal.length, data = irisff)
# Sample size =  150 
# Coef    (95%     CI)     SE p
# (Intercept)  -0.3665 -0.4463 -0.2867 0.0399 0
# petal.length  0.4164  0.3972  0.4356 0.0096 0

# atribuindo a b, o valor da intercepção y
b <- summary(model)$mat[1,1] # primeira linha e primeira columa para o summary

#atribuindo inclinacao do modelo
m <- summary(model)$mat[2,1]

#plot linha para regressao
lines(
  x = irisff$petal.length[]
  ,y = m * irisff$petal.length[] + b
  ,col = "red"
  ,lwd = 3)

# prevendo novos valores para o modelo
predict(
  object = model
  ,newdata = data.frame(
    petal.length = c(2, 5, 7)
    ,petal.width = c(0, 0, 0)))

####################################################################################
#Exercicio: Machine Learning
# Qual especie de Iris?
# Quao acurado esta o modelo de predicao
#
####################################################################################

#setando o diretorio de trabalho (work directory)
setwd("C:/Fabio Battestin/Repo/r/material/cursos/R")

#lendo arquivos csv (criando dataset)
iris <- read.csv("Iris.csv")


#instalando pacote de arvore de decisao
install.packages("tree")

#carregando biblioteca de arvore de decisao
library(tree)

head(iris)

#setando seed para randomizar a reproducao do modelo
set.seed(42)

# 100 de 150 linhas indexadas com  valores randomicos 
indexes <- sample(x = 1:150, size = 100)

print(indexes)

head(indexes)

indexes

# criando training set a partir do index com dados do dataset iris
# é inserido na variavel linhas o objeto indexes para gerar a massa de trieno
train <- iris[indexes, ]

head(train)

print(train)


# criando teste set a partir do index com dados do dataset iris
# para gerar a massa de teste é feito o mesmo, porem é inserido o sinal de subtracao
# para que seja gerados numeros aletorios diferentes do ja compostostos no objeto index
teste <- iris[- indexes, ]


#treinando arvore de decisao
model <- tree(formula = species ~ . ,data = train)

# inspecionando modelo
summary(model)

# visualizando modelo de arvore de decisao
plot(model)

#adicionando rotulo
text(model)

# criando paleta de cores para plotagem
palette <- heat.colors(3,alpha = 1)

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

#realizando predicao do model inserindo dataset de teste
# type = class significa que o teste de predicao do modelo
# sera realizado para classificao de novos dados
predictions <- predict(
  object = model
  ,newdata = teste
  ,type = "class")


# criando confusion matrix
# confusion matrix é importante para o modelo para observar se o modelo esta 
# correto ou nao ao classificar os dados, normalmente sao necessarios maiores informacoes
# para determinar o quao acurado ou confiavel estar o seu modelo de machine learnin
# para isso é necessario baixar e instalar o pacote caret e as dependencias lattice e ggplot2


install.packages("caret")
install.packages("lattice")
install.packages("ggplot2")
install.packages("e1071")


#carregando biblioteca
library("caret")

table(
  x = predictions
  ,y = teste$species
)

# ##result set
# y
# x                 Iris-setosa Iris-versicolor Iris-virginica
# Iris-setosa              17               0              0
# Iris-versicolor           0              16              0
#Iris-virginica            0               2             15

#carregando liblioteca 
#que significa treinamento de classificação e regressão
library(caret)



#avaliando os resultados de predicao
confusionMatrix(
  data = predictions
  ,reference = teste$species
)


#salvando arvore de decisao
#setando o diretorio de trabalho (work directory)
setwd("C:/Fabio Battestin/Repo/r/material/cursos/R")

#salvando modelo
save(model, file = "Tree.Rdata")


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
      ,pch = 2
      ,cex = 2
      ,lwd = 2)
  })
}

##criando a shiny app
shinyApp(
  ui = ui
  ,server = server)

####################################################################################
#Exercicio: nonlinear regression
####################################################################################

#setando o diretorio de trabalho (work directory)
setwd("C:/Fabio Battestin/Repo/r/material/cursos/R")

#lendo arquivos csv (criando dataset)
cars <- read.csv("cars.csv")

head(cars)

#plotando relacao distancia -> velocidade
plot(cars$dist ~ cars$speed)

#inserindo linha
lines(lowess(cars$dist ~ cars$speed))

#modelo polynomial 
m2 <- lm(dist ~ poly(speed, 2, raw = TRUE), data = cars)

summary(m2)

lines(cars$speed, predict(m2), col=2)
