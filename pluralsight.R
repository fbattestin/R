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

