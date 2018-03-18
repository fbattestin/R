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
