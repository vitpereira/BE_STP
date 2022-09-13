
# Importa dados dos alunos
# By Vitor Pereira, Aug 2022

rm(list=ls()) # apaga tudo no ambiente

# install.packages("devtools")
# install.packages("pacman")
# devtools::install_github("DanChaltiel/crosstable", build_vignettes=TRUE)
pacman:: p_load(magrittr, dplyr, vctrs, readr,
                janitor, readxl, gt, epiDisplay,
                data.table, formattable, tidyr, crosstable)

# Paths
root     <- "C:/Users/vitor/Dropbox (Personal)/Sao Tome e Principe/2022/BE_STP/"
input    <- paste0(root, "input/")
output   <- paste0(root, "output/")
tmp      <- paste0(root, "tmp/")
code     <- paste0(root, "code/")
setwd(root)

col_types <- readr::cols(.default = readr::col_character())
#######################################

basica            <- clean_names(as.data.frame(read_excel(paste0(input, "BASE BÁSICO - inicio ano 2021-2022.xlsx"), sheet = "Base Aluno")))
secundaria_7_9    <- clean_names(as.data.frame(read_excel(paste0(input,"Base Dados inicio Secundario -18-07-2022.xlsx"), sheet="Alunos 7ª- 9ª classes")))
secundaria_10_12  <- clean_names(as.data.frame(read_excel(paste0(input,"Base Dados inicio Secundario -18-07-2022.xlsx"), sheet="Alunos 10ª- 12ª Classes")))

basica <- basica %>% # o pipe indica que estamos a ultizar a base chamada basica
  remove_empty(c("rows", "cols")) %>% #retiramos as linhas e colunas vazias
  dplyr::select(-codigo_escola) %>%  # ficamos com todas as colunas, excepto o codigo_escola
  mutate(across(.fns = as.character)) # transformamos todas as colunas em texto

secundaria_7_9 <- secundaria_7_9 %>% 
  remove_empty(c("rows", "cols")) %>% 
  dplyr:: select(-codigo_escola)  %>% 
  mutate(across(.fns = as.character)) %>% 
  mutate(nome_da_escola=nome_estabelecimento)

secundaria_10_12 <- secundaria_10_12%>% 
  remove_empty(c("rows", "cols"))  %>% 
  dplyr::select(-codigo_escola)  %>% 
  mutate(across(.fns = as.character)) %>%
  mutate(nome_da_escola=escola)
  

# Teste se as colunas sao iguais
# all.equal(secundaria_7_9_v2,secundaria_10_12_v2,
#          ignore_col_order=TRUE )

# Append- agregacao das bases, uma base em cima da outra
base_alunos <- bind_rows(basica, secundaria_7_9,
                         secundaria_10_12) %>%
  mutate(distrito=replace(distrito,
                          distrito=="LOBATA","Lobata"), 
         classe = gsub("ª", "", classe))

#transforma de volta as colunas numericas para o formato de numero  
base_alunos <- readr::type_convert(base_alunos) 

tab1(base_alunos$distrito)
tab1(base_alunos$idade)
tab1(base_alunos$classe)

base_alunos <- base_alunos %>% 
  mutate(distorcao = case_when(
    idade-classe-5 >=2 ~ "Em distorção",
    idade-classe-5 < 2 ~ "Fora de distorção" )) 

# Vamos retirar as bases que não vamos mais usar
rm(list = c("basica", "col_types",
            "secundaria_7_9", 
            "secundaria_10_12"))

base_alunos <- base_alunos %>% 
  mutate(classe_f=factor(classe))


ciclo <- base_alunos %>% 
  mutate(case_when(
    classe == 1 ~ "1o ciclo", 
    classe == 2 ~ "1o ciclo", 
    classe == 3 ~ "2o ciclo", 
    classe == 4 ~ "2o ciclo"  ))

rm(list=c("ciclo"))


# Salva o ambiente em R

#save.image(file = paste0(tmp,"alunos_todos.RData"))
saveRDS(base_alunos, file = paste0(tmp,"alunos_todos_v2.RData"))

