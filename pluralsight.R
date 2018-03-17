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
