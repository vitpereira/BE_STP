# Mais  Exercicios para criar graficos

rm(list=ls()) # apaga tudo no ambiente

pacman:: p_load(ggplot2, tibble, gapminder, magrittr,
                tidyverse, dplyr, vctrs, readr,
                janitor, readxl, gt, epiDisplay,
                data.table, formattable, tidyr, crosstable)

# Paths
root     <- "C:/Users/vitor/Dropbox (Personal)/Sao Tome e Principe/2022/R_STP/"
input    <- paste0(root, "input/")
output   <- paste0(root, "output/")
tmp      <- paste0(root, "tmp/")
code     <- paste0(root, "code/")
alunos   <- paste0(root, "alunos/")
setwd(root)

Sys.setenv(TZ="UCT")

#########################################

# Abre a base dos alunos
base_alunos <- readRDS(paste0(tmp,"alunos_todos.rData"))
