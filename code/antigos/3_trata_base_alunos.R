
rm(list=ls())

pacman:: p_load(magrittr, dplyr, vctrs, readr, stringr,
                janitor, readxl, gt, epiDisplay, tidylog,
                data.table, formattable, tidyr, crosstable)

# Paths
root     <- "C:/Users/vitor/Dropbox (Personal)/Sao Tome e Principe/2022/Boletim_estatisco/"
input    <- paste0(root, "input/")
output   <- paste0(root, "output/")
tmp      <- paste0(root, "tmp/")
code     <- paste0(root, "code/")
alunos   <- paste0(root, "alunos/")
setwd(root)

# Le todas as colunas e decide quais sao numericas
col_types <- readr::cols(.default = readr::col_character())

# Acerta a hora de STP
Sys.setenv(TZ="UCT")

#########################################

# Abre a base dos alunos
load (paste0(tmp,"alunos_todos.RData"))

# Abre a base dos códigos das escolas
codigos <- clean_names(read_csv(paste0(input,"escolas_STP.csv"))) %>% 
  rename(nome_da_escola = nm_infra)
  
# Nomes são unicos?
codigos2 <- codigos %>% 
  group_by(nome_da_escola) %>% 
  arrange(nome_da_escola, distritos) %>% 
  mutate(n_linhas = n(), 
         n_ordem = row_number()) %>% 
  filter(n_ordem == 1) # rever depois as escolas de nome repetido

# distinct(codigos2, nome_da_escola)

repetidos <- codigos2 %>% 
  filter(n_ordem>1) %>% 
  select(nome_da_escola, nm_tipo)

# Angolares           Ensino Secundario
# CR S.José Pre       Escolar
# CR Santa Clotilde   Pre Escolar
# Januário Graça      Ensino Basico
# Ribeira Afonso      Ensino Secundario
# Trindade Ensino     Secundar

# Base dos alunos possui nomes de escolas com designição de secundária ou basica

codigos3 <- codigos2 %>% 
  str_replace_all(nome_da_escola, "Escola Básica", "") 

class()


%>% 
  str_replace(fixed("Escola Secundária"), "")


# Merge com alunos
alunos_cod_escola <- base_alunos %>% 
  left_join(codigos2, by="nome_da_escola")

