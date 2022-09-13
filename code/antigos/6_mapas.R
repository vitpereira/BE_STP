

rm(list=ls())

# s
pacman:: p_load( dplyr, ggplot2, sf)

# Paths
root     <- "C:/Users/Utilizador/Desktop/R_STP/"
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
base_alunos<- readRDS(paste0(tmp,"alunos_todos.RData"))

maps_STP <- st_read(paste0(input,"shapes/stp_admbnda_adm2_gadm_2020.shp" ))

# Base escolar no nivel do distrito
base_distrito <- st_as_sf(base_alunos %>%
  mutate(idade_5a  = ifelse(classe == 5, idade, NA),
         idade_8a  = ifelse(classe == 8, idade, NA),
         idade_11a = ifelse(classe ==11, idade, NA)) %>% 
  group_by(distrito, nome_da_escola, classe, turma, 
           periodo, regime, 
           area_de_estudo_10a_a_12a_classe) %>% 
  summarise(alunos_turma=n(), 
            media_idade_5a_turma=mean(idade_5a,na.rm=TRUE), 
            media_idade_8a_turma=mean(idade_8a,na.rm=TRUE), 
            media_idade_11a_turma=mean(idade_11a,na.rm=TRUE)) %>% 
      group_by(distrito) %>% 
  summarise(total_alunos = n(), 
            media_alunos_turma  = mean(alunos_turma,na.rm = TRUE),
            media_idade_5a_dis  = weighted.mean(media_idade_5a_turma,alunos_turma,na.rm = TRUE ), 
            media_idade_8a_dis  = weighted.mean(media_idade_8a_turma,alunos_turma,na.rm = TRUE ), 
            media_idade_11a_dis = weighted.mean(media_idade_11a_turma,alunos_turma,na.rm = TRUE )) %>% 
        mutate(ADM2_PCODE=
           case_when(distrito == "R.A.Príncipe" ~ "ST0101" , 
                     distrito == "Água Grande"  ~ "ST0201" ,
                     distrito == "Cantagalo"    ~ "ST0202" ,
                     distrito == "Caué"         ~ "ST0203" ,
                     distrito == "Lembá"        ~ "ST0204" ,
                     distrito == "Lobata"       ~ "ST0205" ,
                     distrito == "Mé-Zochi"     ~ "ST0206")) %>% 
  left_join(maps_STP, by="ADM2_PCODE"))

class(base_distrito)

# Mapa!!!!

ggplot(base_distrito) +
  geom_sf(aes(fill=total_alunos), alpha=0.8, col="black") +
  scale_fill_viridis_c(option="magma", direction=-1) +
  labs( title = " Total de alunos por distrito") 

ggplot(base_distrito) +
  geom_sf(aes(fill=media_idade_5a_dis), alpha=0.8, col="black") +
  scale_fill_viridis_c(option="magma", direction=-1) +
  labs( title = "Média de idade dos alunos", 
        subtitle= "5a classe") 

ggplot(base_distrito) +
  geom_sf(aes(fill=media_idade_8a_dis), alpha=0.8, col="black") +
  scale_fill_viridis_c(option="magma", direction=-1) +
  labs( title = "Média de idade dos alunos", 
        subtitle= "8a classe") 

ggplot(base_distrito) +
  geom_sf(aes(fill=media_idade_11a_dis), alpha=0.8, col="black") +
  scale_fill_viridis_c(option="magma", direction=-1) +
  labs( title = "Média de idade dos alunos", 
        subtitle= "11a classe") 