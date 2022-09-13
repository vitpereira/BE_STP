
# Coloca os codigos nas escolas
# By Vitor Pereira, Aug 2022

rm(list=ls()) # apaga tudo no ambiente

pacman:: p_load(tidylog, magrittr, dplyr, vctrs, readr,
                janitor, readxl, gt, epiDisplay, stringr,
                data.table, formattable, tidyr, crosstable)

# Paths
root     <- "C:/Users/vitor/Dropbox (Personal)/Sao Tome e Principe/2022/Boletim_estatisco/"
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
  mutate(nome_da_escola2=gsub("Escola Básica","",
                            nome_da_escola )) %>% 
  mutate(nome_da_escola2=gsub("Secundária","",
                              nome_da_escola2)) %>% 
  mutate(nome_da_escola3=stringr::str_trim(nome_da_escola2)) 

# Base dos codigos
base_codigos <- clean_names(read_xlsx(paste0(input,
                "01-CODIGO-Rede escolar-31-01-2022.xlsx"))) %>% 
  mutate(nome_da_escola2=nm_infra) %>% 
  mutate(distrito=distritos) %>% 
  mutate(nome_da_escola3=stringr::str_trim(nome_da_escola2)) %>% 
  mutate(nome_da_escola3 = str_replace_all(nome_da_escola3, "C. P. Madalena de Canossa", "Escola Centro Promoção Madalena de Canossa"))
  
  

# Junta as bases
base_alunos_cod <-  anti_join(base_alunos, 
                              base_codigos,
                              by="nome_da_escola3") %>% 
  select(nome_da_escola3, nome_do_aluno, distrito) %>% 
  arrange(distrito, nome_da_escola3, nome_do_aluno)





class(base_alunos$nome_da_escola2)
class(base_codigos$nome_da_escola2)
