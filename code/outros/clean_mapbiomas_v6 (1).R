library(tidyverse)
library(haven)
library(readxl)
library(stringr)
library(janitor)
library(pryr)

setwd("C:\\Users\\Yuri Oliveira\\Dropbox\\deforestation\\data")

# read land use excel file
df = read_excel("mapbiomas_land_use_v6/land_use_v6_municipios.xlsx",
                sheet = "LAND COVER")

write.csv(df, "mapbiomas_land_use_v6/mapiomas_v6.csv")


teste = read_excel("mapbiomas_land_use_v6/mapbiomas_land_use_v6/land_use_v6_municipios.xlsx",
                sheet = "LAND COVER")


# reshape the data
df = teste %>% mutate(across(13:49, ~replace_na(.x, 0))) %>%
  select(-(`1985`:`2007`)) %>%
  gather(key = year, value = area, `2008`:`2020`) %>%
  relocate(state, city, geo_code, year , level_0, level_1, level_2, level_3, level_4, area)



df_total = df %>% group_by(state, city, geo_code, year) %>%
  summarise(tot_area_mapbiomas = sum(area, na.rm = T))

# farming (pasture + agriculture), forest and non forest natural formations
df_level1 = df %>% group_by(state, city, geo_code, year, level_1) %>%
  summarise(area = sum(area, na.rm = T)) %>%
  spread(key = level_1, value = area) %>%
  janitor::clean_names() %>%
  select(state, city, geo_code, year, x1_forest, x2_non_forest_natural_formation, x3_farming, ) %>%
  rename(forest_area = x1_forest,
         nat_formation_area = x2_non_forest_natural_formation,
         farm_area= x3_farming) %>%
  mutate(across(forest_area:farm_area, ~replace_na(.x, 0)))


tabyl(df$level_2) %>% View()


# Pasture, agriculture, mosaic,  area
df_level2 = df %>% group_by(state, city, geo_code, year, level_2) %>%
  summarise(area = sum(area, na.rm = T)) %>%
  spread(key = level_2, value = area) %>%
  janitor::clean_names() %>%
  select(state, city, geo_code, year, agriculture, pasture, mining, mosaic_of_agriculture_and_pasture) %>%
  rename(agric_area = agriculture,
         pasture_area= pasture,
         mosaic_area = mosaic_of_agriculture_and_pasture,
         mining_area = mining) %>%
  mutate(across(agric_area:mosaic_area, ~replace_na(.x, 0)))


tabyl(df$level_4) %>% View()

# Soybean  area
df_level4 = df %>% group_by(state, city, geo_code, year, level_4) %>%
  summarise(area = sum(area, na.rm = T)) %>%
  spread(key = level_4, value = area) %>%
  janitor::clean_names() %>%
  select(state, city, geo_code, year, soy_beans) %>%
  rename(soy_area = soy_beans) %>%
  mutate(across(soy_area:soy_area, ~replace_na(.x, 0)))

# MERGE ALL CLASSIFICATIONS IN ONE
df_final = left_join(df_total, df_level1,
                     by = c("state", "city", "geo_code", "year"))
df_final = left_join(df_final, df_level2,
                     by = c("state", "city", "geo_code", "year"))
df_final = left_join(df_final, df_level4,
                     by = c("state", "city", "geo_code", "year"))



# save in dta format
write_dta(df_final, "panel_mapbiomas_v6.dta")

