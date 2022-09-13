
# Importa dados dos alunos
# By Vitor Pereira, Aug 2022

rm(list=ls()) # apaga tudo no ambiente

devtools::install_github("DanChaltiel/crosstable", build_vignettes=TRUE)

pacman:: p_load(dplyr, readxl, gt, epiDisplay, data.table, formattable, tidyr, crosstable)

# Paths
root     <- "C:/Users/vitor/Dropbox (Personal)/Sao Tome e Principe/2022/Boletim_estatisco/"
input    <- paste0(root, "input/")
output   <- paste0(root, "output/")
tmp      <- paste0(root, "tmp/")
code     <- paste0(root, "code/")
setwd(root)

#######################################

secundaria <- as_tibble(read_excel(paste0(input,"Base Dados inicio Secundario -18-07-2022.xlsx"), sheet="Alunos 7ª- 9ª classes"))

glimpse(secundaria)

# Alunos matriculados por distrito, classe, frequencia e idade

base_class_freq_idad <- secundaria    %>%
  rename(Frequencia='Nº de frequência') %>%
  group_by(DISTRITO, Frequencia, Sexo, Classe ) %>%
  summarise(n_obs= n(), na.rm=TRUE)

secundaria2 <- secundaria %>% 
  rename(Frequencia='Nº de frequência') 

ct1 = crosstable(secundaria2, c(DISTRITO, Frequencia,Idade), by=c(Classe, Sexo ), total="both", 
                 percent_pattern="{n} ({p_row}/{p_col})", percent_digits=0) %>%
  as_flextable()

ct1

crosstable(base_class_freq_idad, c(DISTRITO), by(idade) )

, by(i)
gt(base_class_freq_idad)



airquality_m <- 
  airquality %>%
  mutate(Year = 1973L) %>%
  slice(1:10)



