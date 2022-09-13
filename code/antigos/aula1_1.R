
# Aula 1 de R

# Vamos começar abrindo uma base de dados que  já vem com o R
# install.packages("readr")
# install.packages("epiDisplay")

# vamos abrir o pacote
library(readr)
library(epiDisplay)

iris <- read_csv("iris.csv")
head(iris) #primeiras linhas
str(iris) # estrutura da base
View(iris) # abre a base para poder olhar
summary(iris) # faz um resumo da base
tab1(iris$sepal.length)
tab1(iris$variety)
df <- iris[, 1:4] # O que esta a esquerda da virgula sao as linhas, a direita as colunas
boxplot(df) # boxplot
pairs(df) # scatters 2 a 2
stars(df)
PL <- df$petal.length
barplot(PL)
hist(PL)
SP <- iris$variety
pie(table(SP))
boxplot(PL ~ SP)




