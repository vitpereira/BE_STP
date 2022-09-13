
# Analisando dados dos estudantes
#install.packages("devtools")
#install.packages("pacman")
#install.packages('stringi')

rm(list=ls())
 
#devtools:: install_github("data-edu/dataedu")

pacman:: p_load(tidyverse, apaTables, sjPlot,
                readxl, dataedu, janitor, epiDisplay)

Sys.setenv(TZ="UCT") # Acerta a hora de STP

pre_survey <- clean_names(dataedu::pre_survey)
course_data <- clean_names(dataedu::course_data)
course_minutes <-clean_names(dataedu::course_minutes)

# renomear variaveis
pre_survey <- pre_survey %>%
  rename(
    q1 = q1maincellgroup_row1 , 
    q2 = q1maincellgroup_row2 , 
    q3 = q1maincellgroup_row3 , 
    q4 = q1maincellgroup_row4 , 
    q5 = q1maincellgroup_row5 , 
    q6 = q1maincellgroup_row6 , 
    q7 = q1maincellgroup_row7 , 
    q8 = q1maincellgroup_row8 , 
    q9 = q1maincellgroup_row9 , 
    q10 = q1maincellgroup_row10, 
    usuario = opdata_username, 
    curso = opdata_course_id
    ) 
    
#class(pre_survey$q10)  
 
funcao_reverter <- function(questao){
  questao_mod <- case_when(
    questao == 1 ~ 5 ,
    questao == 2 ~ 4 ,
    questao == 3 ~ 3 ,
    questao == 4 ~ 2 ,
    questao == 5 ~ 1 )
    
questao_mod
}

# reveter dentro do mutate

pre_survey2 <- pre_survey %>% 
  mutate(q4=funcao_reverter(q4),
         q7=funcao_reverter(q7))

#table(pre_survey$q4, pre_survey2$q4)

#tab1(pre_survey2$opdata_course_id)

measure_mean <- pre_survey2 %>% 
  pivot_longer(cols = q1:q10, 
               names_to = "questao", 
               values_to = "resposta") %>% 
  arrange(usuario, curso, questao)

measure_mean <- measure_mean %>% 
  mutate( construto = case_when(
      questao %in% c("q1", "q4", "q5", "q8", "q10") ~ "interesse" ,
      questao %in% c("q2", "q6" , "q9") ~ "utilidade do curso", 
      questao %in% c("q3", "q7") ~ "competencia percebida"))


# Vamos fazer as medias por construto
medias_construto <- measure_mean %>% 
  group_by(construto) %>%  # agrupa por construto
  summarise( media_respostas = mean(resposta, na.rm=TRUE), # media
             median_repostas = median(resposta, na.rm=TRUE), # mediana
             desv_pad_repostas = sd(resposta, na.rm=TRUE), # desvio padrao
             perc_missing    = mean(is.na(resposta)) # percentual de missings
             )

# Vamos agora juntar bases de dados (merge)

# precisamos modificar a coluna de usuario

pre_survey3 <- pre_survey2 %>% 
  mutate(usuario= as.numeric( str_sub(usuario, start=2, end=-3)))


# Precisamos checar o numero de valores distintos

pre_survey4 <- pre_survey3 %>% 
  group_by(usuario, curso) %>% 
  arrange(usuario, curso, q1) %>% 
  mutate(n_linhas = n(), 
         n_ordem = row_number()) %>% 
  # => Multiplas combinacoes de aluno e curso
  filter(n_ordem == 1)

distinct(pre_survey4, usuario, curso)

# Renomeio as colunas na base que vamos juntar
# bom ficar com o mesmo nome na coluna chave
course_data <- course_data %>% 
  rename(usuario= bb_user_pk, 
         curso = course_section_orig_id)


# checando agora na base de cursos
course_data2 <- course_data %>% 
  group_by(usuario, curso) %>% 
  arrange(usuario, curso, points_earned) %>% 
  mutate(n_linhas = n(), 
         n_ordem = row_number())
tab1(course_data2$n_linhas)

# Left join é o merge mais comum
# let is one, right is many

dat <- pre_survey4 %>% 
  left_join(course_data2, by = c("usuario", "curso"))  %>% 
  arrange(usuario, curso, gradebook_item)

# Inner join
dat2 <- pre_survey4 %>% 
  inner_join(course_data2, by = c("usuario", "curso"))  %>% 
  arrange(usuario, curso, gradebook_item)
          
# Full join
dat3 <- pre_survey4 %>% 
  full_join(course_data2, by = c("usuario", "curso"))  %>% 
  arrange(usuario, curso, gradebook_item)

# Right join
dat4 <- pre_survey4 %>% 
  right_join(course_data2, by = c("usuario", "curso"))  %>% 
  arrange(usuario, curso, gradebook_item)

# Right join
dat5 <- course_data2 %>% 
  right_join( pre_survey4, by = c("usuario", "curso"))  %>% 
  arrange(usuario, curso, gradebook_item)

# anti_join
dat6 <- pre_survey4 %>% 
  anti_join(course_data2, by = c("usuario", "curso"))  %>% 
  arrange(usuario, curso )





distinct(pre_survey3, curso, usuario) 
