
# Mapas
# Dia 08 de setembro de 2022
# By Vitor Pereira

rm(list=ls())

pacman:: p_load(dplyr, tidylog, ggplot2, sf, tidyr)

# Paths
root     <- "C:/Users/vitor/Dropbox (Personal)/Sao Tome e Principe/2022/R_STP/"
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

# Abre as bases

base_alunos <- readRDS(paste0(tmp,"alunos_todos.RData"))

maps_STP <- st_read(paste0(input,"shapes/stp_admbnda_adm2_gadm_2020.shp"))

# Exercicio: PLotar no mapa o racio da quantidade de alunos da 12 e da 1a classe

# 1a forma de fazer

alunos_1a_dis <- base_alunos %>% 
  filter(classe==1) %>% 
  group_by(distrito) %>% 
  summarise(alunos_1a = n())

alunos_12a_dis <- base_alunos %>% 
  filter(classe==12) %>% 
  group_by(distrito) %>% 
  summarise(alunos_12a = n())

# Join
base_racio_12_1 <- alunos_1a_dis %>% 
  left_join(alunos_12a_dis, by = "distrito") %>%
  mutate(racio= alunos_12a/alunos_1a) %>% 
  mutate(ADM2_PCODE=
           case_when(distrito == "R.A.Príncipe" ~ "ST0101" , 
                     distrito == "Água Grande"  ~ "ST0201" ,
                     distrito == "Cantagalo"    ~ "ST0202" ,
                     distrito == "Caué"         ~ "ST0203" ,
                     distrito == "Lembá"        ~ "ST0204" ,
                     distrito == "Lobata"       ~ "ST0205" ,
                     distrito == "Mé-Zochi"     ~ "ST0206"))%>% 
  left_join(maps_STP, by="ADM2_PCODE")

base_mapa_racio <- st_as_sf(base_racio_12_1)  

# Mapa
ggplot(base_mapa_racio) +
  geom_sf(aes(fill=racio), alpha=0.8, col="black") +
  scale_fill_viridis_c(option="magma", direction=-1) +
  labs( title = "Racio de alunos da 12a sobre a 1a") 

##############################
base_al_1a_v2 <- base_alunos %>% 
  filter(classe==1 | classe==12) %>%  # Filtra para 1a e 12a 
  group_by(distrito, sexo, classe) %>% # Agrupa por distrito
  summarise(tot_alunos=n()) %>%  # Calcula o numero de alunos
  pivot_wider(names_from=c(sexo,classe) , # Pivota a base
              values_from=tot_alunos) %>%  
  mutate(A_1=F_1+M_1,  # Gera o total de alunos na 1a classe
         A_12=F_12+M_12) %>% # Gera o total de alunos na 12a classe
  mutate(racio_A=A_12/A_1, # Gera o racio de 12a e 1a- Todos
         racio_F=F_12/F_1, # Gera o racio de 12a e 1a- Feminino
         racio_M=M_12/M_1) %>%  # Gera o racio de 12a e 1a- Masculino
  mutate(ADM2_PCODE=
           case_when(distrito == "R.A.Príncipe" ~ "ST0101" , 
                     distrito == "Água Grande"  ~ "ST0201" ,
                     distrito == "Cantagalo"    ~ "ST0202" ,
                     distrito == "Caué"         ~ "ST0203" ,
                     distrito == "Lembá"        ~ "ST0204" ,
                     distrito == "Lobata"       ~ "ST0205" ,
                     distrito == "Mé-Zochi"     ~ "ST0206"))%>% 
  left_join(maps_STP, by="ADM2_PCODE")

base_mapa_racio2 <- st_as_sf(base_al_1a_v2)  

# Mapa
ggplot(base_mapa_racio2) +
  geom_sf(aes(fill=racio_M), alpha=0.8, col="black") +
  scale_fill_viridis_c(option="magma", direction=-1) +
  labs( title = "Racio de alunAs da 12a sobre a 1a") 
