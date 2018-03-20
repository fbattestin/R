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
