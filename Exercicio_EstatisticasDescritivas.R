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
