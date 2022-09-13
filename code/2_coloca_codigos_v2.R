
# Coloca os codigos nas escolas
# By Vitor Pereira, Aug 2022

rm(list=ls()) # apaga tudo no ambiente

pacman:: p_load(tidylog, dplyr, readr,
                janitor, readxl, epiDisplay, stringr,
                tidyr)

# Paths
root     <- "C:/Users/vitor/Dropbox (Personal)/Sao Tome e Principe/2022/BE_STP/"
input    <- paste0(root, "input/")
output   <- paste0(root, "output/")
tmp      <- paste0(root, "tmp/")
code     <- paste0(root, "code/")
setwd(root)

col_types <- readr::cols(.default = readr::col_character())

# Hora de STP
Sys.setenv(TZ="UCT")
#######################################

# Base de alunos
base_alunos<- readRDS(paste0(tmp,"alunos_todos_v2.RData")) %>% 
  mutate(nome_da_escola2=gsub("Escola Básica de","", nome_da_escola )) %>% 
  mutate(nome_da_escola3=gsub("Escola Básica Secundária de","", nome_da_escola2)) %>% 
  mutate(nome_da_escola4=gsub("Escola Secundária de","", nome_da_escola3)) %>% 
  mutate(nome_da_escola5=gsub("Escola Secundária","", nome_da_escola4)) %>% 
  mutate(nome_da_escola6=gsub("Secundária","", nome_da_escola5)) %>% 
  mutate(nome_da_escola7=gsub("Básica","", nome_da_escola6)) %>% 
  mutate(nome_da_escola8=gsub("Escola","", nome_da_escola7)) %>% 
  mutate(nome_da_escola9=stringr::str_trim(nome_da_escola8)) 

# Base dos codigos
base_codigos <- clean_names(read_xlsx(paste0(input,
                "01-CODIGO-Rede escolar-31-01-2022.xlsx"))) %>% 
  mutate(nome_da_escola =nm_infra) %>% 
  mutate(nome_da_escola2=gsub("Escola Básica de","", nome_da_escola )) %>% 
  mutate(nome_da_escola3=gsub("Escola Básica Secundária de","", nome_da_escola2)) %>% 
  mutate(nome_da_escola4=gsub("Escola Secundária de","", nome_da_escola3)) %>% 
  mutate(nome_da_escola5=gsub("Escola Secundária","", nome_da_escola4)) %>% 
  mutate(nome_da_escola6=gsub("Secundária","", nome_da_escola5)) %>% 
  mutate(nome_da_escola7=gsub("Básica","", nome_da_escola6)) %>% 
  mutate(nome_da_escola8=gsub("Escola","", nome_da_escola7)) %>% 
  mutate(nome_da_escola9=stringr::str_trim(nome_da_escola8)) %>% 
  mutate(distrito=distritos) 
    
#  mutate(nome_da_escola3 = str_replace_all(nome_da_escola3, "C. P. Madalena de Canossa", "Escola Centro Promoção Madalena de Canossa")) %>% 
#  mutate(nome_da_escola3 = str_replace_all(nome_da_escola3, "Escola  Angolares", "Angolares"  ))

#a Junta as bases
base_alunos_cod <- base_alunos %>% 
  filter(distrito=="Caué") %>% 
  dplyr::select(nome_da_escola8, distrito, nome_do_aluno, classe) %>% 
  distinct(nome_da_escola8, .keep_all = TRUE) %>% 
  anti_join(base_codigos,
            by="nome_da_escola8")

