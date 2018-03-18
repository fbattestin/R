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
