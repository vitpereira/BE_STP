
rm(list=ls()) # apaga o ambiente

vetor_1 <- c(1,2)

car <- mtcars

class(car)
class(vetor_1)

teste <- 1 != 1
class(teste)

3 %in% vetor_1

segredo <- round(runif(1, min = 0, max = 10))

segredo >=0
segredo >10
segredo >=5
segredo %in% c(0,2,4)
segredo*5 >=39
print(segredo)

vetor <- c(1, 4,7)

vetor[4]

is.na(vetor[4])

0/0

log(-1)

log(0)

1/0

10^700

# listas

lista_1 <- list("a", "b", "c")
                
lista_2 <- list(vetor, vetor_1, "a", teste, sum) 

car_list <- as.list(mtcars)

x <-1

if (x == 1) {         
  Sys.time()
}

Sys.setenv(TZ='UTC')



