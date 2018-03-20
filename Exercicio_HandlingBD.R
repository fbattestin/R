####################################################################################
#Exercicio: handling big data 
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
