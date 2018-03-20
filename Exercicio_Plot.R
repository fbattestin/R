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
