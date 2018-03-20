####################################################################################
#Exercicio: Regressao Linear 
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
