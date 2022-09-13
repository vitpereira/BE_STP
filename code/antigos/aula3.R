
# Analisando dados dos estudantes
#install.packages("devtools")
#install.packages("pacman")
#install.packages('stringi')
  
devtools:: install_github("data-edu/dataedu")

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




