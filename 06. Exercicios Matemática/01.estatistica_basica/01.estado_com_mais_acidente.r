# 01. Estado com mais acidente no pais 
library(readr)
library(dplyr) 

dados <- read_csv2("D:/donwloads/datatran2024.csv", show_col_types = FALSE)
View(dados)

dados_agrupados <- dados %>%
  group_by(uf) %>%
  summarise(qtde = n())

dados_agrupados_ordenados <- dados_agrupados %>%
  arrange(desc(qtde))

estado_mais_acidentes <- dados_agrupados_ordenados %>% slice(1)

print(estado_mais_acidentes)
View(dados_agrupados_ordenados)

