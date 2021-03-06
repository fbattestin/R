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
# o operador ~pipe~ do dplyr � o %>%
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
