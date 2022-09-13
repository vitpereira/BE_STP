
# Grafico de expectativa de vida

pacman:: p_load(dplyr, gapminder, ggplot2)
# Acerta a hora de STP
Sys.setenv(TZ="UCT")
############################

base_paises <- gapminder

# duas formas de filtrar
base_paises_cplp <- base_paises %>% 
  filter(
      country == "Sao Tome and Principe" |
      country == "Guinea-Bissau" |
      country == "Mozambique" |
      country == "Angola" |
      country == "Brazil" |
      country == "Portugal" )

base_paises_cplp2 <- base_paises %>% 
  filter(country %in% c(
    "Portugal", "Brazil", "Mozambique", "Angola", 
    "Guinea-Bissau", "Sao Tome and Principe"))

grafico_10 <- ggplot(base_paises_cplp2, 
                     aes(
                       x=year, 
                       y=lifeExp, 
                       color=country)) +
    geom_line() +
    geom_point()+
    labs(title="Expectativa de vida dos países da CPLP", 
         subtitle="De 1962 a 2007", 
         x="Ano", 
         y="Expectativa de vida")     

grafico_10
