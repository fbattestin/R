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
