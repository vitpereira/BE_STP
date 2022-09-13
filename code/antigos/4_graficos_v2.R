
# Exercicios para criar graficos

rm(list=ls()) # apaga tudo no ambiente

<<<<<<< HEAD
pacman:: p_load(ggridges, ggplot2, magrittr, dplyr, vctrs, readr,
=======
pacman:: p_load(ggplot2, magrittr, dplyr, vctrs, readr,
>>>>>>> 1aed5b89a472b678cd3e487a505c00e7b4851830
                janitor, readxl, gt, epiDisplay,
                data.table, formattable, tidyr, crosstable)

# Paths
<<<<<<< HEAD
root     <- "C:/Users/vitor/Dropbox (Personal)/Sao Tome e Principe/2022/R_STP/"
=======
root     <- "C:/Users/Utilizador/Desktop/R_STP/"
>>>>>>> 1aed5b89a472b678cd3e487a505c00e7b4851830
input    <- paste0(root, "input/")
output   <- paste0(root, "output/")
tmp      <- paste0(root, "tmp/")
code     <- paste0(root, "code/")
alunos   <- paste0(root, "alunos/")
setwd(root)

Sys.setenv(TZ="UCT")

#########################################
<<<<<<< HEAD
# Grafico de piramide etaria dos alunos
=======
>>>>>>> 1aed5b89a472b678cd3e487a505c00e7b4851830

# Abre a base dos alunos
base_alunos <- readRDS(paste0(tmp,"alunos_todos.rData"))

base_piramide <- base_alunos %>% 
  group_by(sexo, idade) %>% 
  summarise(contagem_alunos = n()) %>% 
  mutate(contagem_alunos= 
           replace(contagem_alunos, 
                   sexo=="F", (-1)* contagem_alunos)) %>% 
  arrange(idade, sexo)

# grafico
grafico_piramide <- ggplot(base_piramide, 
                           aes(x= idade, y= contagem_alunos, 
                               fill= sexo)) +
  geom_bar(data=subset(base_piramide, sexo=="M"),
           stat="identity")+
  geom_bar(data=subset(base_piramide, sexo=="F"),
           stat="identity") +
  scale_y_continuous(breaks = seq(-3000, 3000, 1000), 
                   labels = abs(seq(-3000, 3000, 1000)), 
                   "Quantidade de alunos") +
  coord_flip() +
  scale_fill_brewer(palette="Set2") +
  theme_bw()

grafico_piramide                     

###############################################
<<<<<<< HEAD
# Grafico de quantidade de alunos por distrito

=======

# Grafico de quantidade de alunos por distrito
>>>>>>> 1aed5b89a472b678cd3e487a505c00e7b4851830
base_distritos <- base_alunos %>% 
  group_by(distrito) %>% 
  summarise(contagem_alunos = n()) 

grafico_distritos <- ggplot(base_distritos, 
                           aes(x= distrito, y= contagem_alunos, 
                               label = contagem_alunos)) +
  geom_bar(stat="identity", fill="blue") +
  geom_text(color= "white", size = 6, position = position_stack(vjust = 0.5)) +
  coord_flip() +
  theme_bw()

grafico_distritos
######################################

base_piramide_dist <- base_alunos %>% 
  group_by(distrito, sexo) %>% 
  summarise(contagem_alunos = n()) %>% 
  mutate(contagem_alunos= 
           replace(contagem_alunos, 
                   sexo=="F", (-1)* contagem_alunos)) %>% 
  arrange(distrito, sexo)

# grafico
grafico_piramide2 <- ggplot(base_piramide_dist, 
                           aes(x= distrito, y= contagem_alunos, 
                               fill= sexo)) +
  geom_bar(data=subset(base_piramide_dist, sexo=="M"),
           stat="identity")+
  geom_bar(data=subset(base_piramide_dist, sexo=="F"),
           stat="identity") +
  scale_y_continuous(breaks = seq(-15000, 15000, 5000), 
                     labels = abs(seq(-15000, 15000, 5000)), 
                     "Quantidade de alunos") +
  coord_flip() +
  scale_fill_brewer(palette="Set2") +
  theme_bw()

<<<<<<< HEAD
grafico_piramide2

###########################################

grafico_idade_classe1 <- base_alunos  %>% 
  group




ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = Month, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Lincoln NE in 2016')

=======
grafico_piramide2
>>>>>>> 1aed5b89a472b678cd3e487a505c00e7b4851830
